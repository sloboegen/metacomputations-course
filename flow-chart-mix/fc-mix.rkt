#lang racket

(require "fc-int.rkt")

(provide fc-mix)
(provide fc-mix-outer fc-mix-trick fc-mix-inner-inner pretty-labels-program)


(define (fc-reduce expr static-state)
  (match expr
    [`(,h . ,t) (cons (fc-reduce h static-state) (fc-reduce t static-state))]
    [`,v (if (dict-has-key? static-state v) `',(dict-ref static-state v) v)]
    ))

(define (fc-expr-static? expr division)
  (match expr
    [`(,h . ,t) (and (fc-expr-static? h division) (fc-expr-static? t division))]
    [`,v (if (dict-has-key? division v) (equal? (dict-ref division v) "static") #t)]))

(define (list-subtract xs ys)
  (if (empty? ys) xs (list-subtract (remove (car ys) xs) (cdr ys))))

; returns basic block by label
; @param pp - a label
; @param pgm - a program without read instruction
(define (lookup-bb pp pgm)
  (match pgm
    ['() (error (format "no-such label ~a" pp))]
    [`(,h . ,t) (
                 match h
                  [`(,lbl ,_ . ,_) (if (equal? lbl pp) (cdr h) (lookup-bb pp t))])])
  )

; evaluates `expr` in fc-ns \/ `state`
; @param expr - expression in FC
; @param state - a list of pairs [varname, value]
(define (fc-eval-with-state expr state)
  (dict-for-each state (lambda (k v) (context-set k v)))
  (fc-eval expr))

; removes static input from read
; @param read - (read ...)
; @param vs   - a hash [var -> value]
(define (filter-read read-instr division)
  (let ([newread (list 'fc-read)])
    (for ([v (cdr read-instr)])  ; skip read-keyword
      (if (equal? (dict-ref division v) "static") null (set! newread (append newread (list v)))))
    newread
    )
  )

(eval (context-set 'dict-ref           dict-ref) fc-ns)
(eval (context-set 'dict-set           dict-set) fc-ns)
(eval (context-set 'fc-reduce          fc-reduce) fc-ns)
(eval (context-set 'fc-eval-with-state fc-eval-with-state) fc-ns)
(eval (context-set 'fc-expr-static?    fc-expr-static?) fc-ns)
(eval (context-set 'list-subtract      list-subtract) fc-ns)
(eval (context-set 'filter-read        filter-read) fc-ns)
(eval (context-set 'lookup-bb          lookup-bb) fc-ns)


; =======================================================================
; ======================== SIMPLE MIX (FOR I PROJECTION) ================
; =======================================================================

; @param program  - a program on FC, a quoted value
; @param division - a hash [variable -> {static, dynamic}]
; @param vs0      - values of the static input
(define fc-mix
  '((fc-read program division vs0)

    (init (fc-assign pp0      (caadr program))
          (fc-assign pending  (list (cons pp0 vs0)))
          (fc-assign marked  '())
          (fc-assign residual (list (filter-read (car program) division))) ; read instruction
          (fc-goto main-loop))

    (main-loop (fc-if (equal? pending '()) stop iter))

    (iter (fc-assign ppvs    (car pending))
          (fc-assign pp      (car ppvs))
          (fc-assign vs      (cdr ppvs))
          (fc-assign pending (cdr pending))
          (fc-assign marked  (append marked (list ppvs)))
          (fc-assign code    (list (cons pp vs)))
          (fc-assign bb      (lookup-bb pp (cdr program)))
          (fc-goto in-loop))

    (in-loop (fc-if (equal? bb '()) upd-residual case-cmd))

    (case-cmd (fc-assign command (car bb))
              (fc-assign bb (cdr bb))
              (fc-goto if-assign))

    (if-assign (fc-if (equal? (car command) 'fc-assign) do-assign if-return))
    (if-return (fc-if (equal? (car command) 'fc-return) do-return if-goto))
    (if-goto   (fc-if (equal? (car command) 'fc-goto)   do-goto   if-if))
    (if-if     (fc-if (equal? (car command) 'fc-if)     do-if     errlbl))

    ; BEGIN CASE

    ; ASSIGN: varname := expr
    (do-assign (fc-assign varname (cadr command))
               (fc-assign expr    (caddr command))
               (fc-if (equal? (dict-ref division varname) "static") do-assign-st do-assign-dy))

    ; static-ASSIGN
    (do-assign-st (fc-assign vs (dict-set vs varname (fc-eval-with-state expr vs)))
                  (fc-goto in-loop))

    ; dynamic-ASSIGN
    (do-assign-dy (fc-assign instr (list `(fc-assign ,varname ,(fc-reduce expr vs))))
                  (fc-assign code  (append code instr))
                  (fc-goto in-loop))

    ; IF: if expr then goto pp' else goto pp''
    (do-if (fc-assign expr (cadr command))
           (fc-if (equal? (fc-expr-static? expr division) #t) do-if-st do-if-dy))

    ; static-IF
    (do-if-st (fc-assign st-cond (fc-eval-with-state expr vs))
              (fc-if (equal? st-cond #t) do-if-st-t do-if-st-f))

    ; static-IF-true
    (do-if-st-t (fc-assign pp1 (caddr command))
                (fc-assign bb (lookup-bb pp1 (cdr program)))
                (fc-goto in-loop))

    ; static-IF-false
    (do-if-st-f (fc-assign pp1 (cadddr command))
                (fc-assign bb (lookup-bb pp1 (cdr program)))
                (fc-goto in-loop))

    ; dynamic-IF
    (do-if-dy   (fc-assign pp1     (caddr command))
                (fc-assign pp2     (cadddr command))
                (fc-assign pp1vs   (cons pp1 vs))
                (fc-assign pp2vs   (cons pp2 vs))
                (fc-assign npend   (list-subtract (list pp1vs pp2vs) marked))
                (fc-assign pending (append pending npend))
                (fc-assign label   (cons pp vs))
                (fc-assign instr   (list `(fc-if ,(fc-reduce expr vs) ,pp1vs ,pp2vs)))
                (fc-assign code    (append code instr))
                (fc-goto in-loop))

    ; GOTO: goto pp'
    (do-goto (fc-assign pp1 (cadr command))
             (fc-assign bb (lookup-bb pp1 (cdr program)))
             (fc-goto in-loop))

    ; RETURN: return expr
    (do-return (fc-assign expr    (cadr command)) ; cdr returns an expression wrapped into a list
               (fc-assign reduced (fc-reduce expr vs))
               (fc-assign label   (cons pp vs))
               (fc-assign instr   (list `(fc-return ,reduced)))
               (fc-assign code    (append code instr))
               (fc-goto in-loop))

    ; END CASE
    (upd-residual (fc-assign residual (append residual (list code)))
                  (fc-goto main-loop))

    (stop (fc-return residual))

    (errlbl (error (format "fc-mix: unknown label ~a" ,pp)))
    )
  )

(define (numerate-labels pgm)
  (let ([lbl2num #hash()]
        [num      0     ])
    (for ([lbl-inst (cdr pgm)]) ; skip read-instruction
      (set! lbl2num (dict-set lbl2num (car lbl-inst) num))
      (set! num (+ num 1))
      )
    (printf "*** DEBUG: COUNT OF STATES ~a\n" (- num 1))
    lbl2num
    )
  )

(define (pretty-labels-program pgm)
  (let ([lbl2num (numerate-labels pgm)]
        [pp-pgm  (list (car pgm))]) ; read-instruction without label
    (for ([lbl-bb (cdr pgm)])
      (match lbl-bb
        [(cons lbl bb) (set! pp-pgm (append pp-pgm (list (cons (dict-ref lbl2num lbl) (pretty-labels-bb bb lbl2num)))))]
        ))
    pp-pgm)
  )

(define (pretty-labels-bb bb lbl2num)
  (match bb
    [`(,h)  (match h
              [`(fc-goto ,lbl)   (list `(fc-goto ,(dict-ref lbl2num lbl)))]
              [`(fc-if ,c ,t ,f) (list `(fc-if ,c ,(dict-ref lbl2num t) ,(dict-ref lbl2num f)))]
              [other             (list other)]) ; other instructions doesn't contain labels
            ]
    [`(,h . ,t) (append (list h) (pretty-labels-bb t lbl2num))] ; don't modify assign

    ))


; =======================================================================
; ========================  MIXES FOR II PORJECTION  ====================
; ========================  (WITH TRICK AND WITHOUT) ====================
; =======================================================================


(define (get-dynamic-labels pgm division)
  (let ([lbls-dy '()])
    (for ([lbl-bb (cdr pgm)]) ; skip read-instruction
      (match lbl-bb
        [(cons lbl bb)
         (let ([instr (last bb)])
           (when (equal? (car instr) 'fc-if)
             (when (not (fc-expr-static? (cadr instr) division))
               (set! lbls-dy (append lbls-dy (list (caddr instr) (cadddr instr)))))))])
      )
    (set->list (list->set lbls-dy))
    )
  )

(eval (context-set 'get-dynamic-labels get-dynamic-labels) fc-ns)

; ------------------------ 1. Simple mix but with renamed variables -----

(define fc-mix-outer
  '((fc-read program-s division-s vs0-s)

    (init-s (fc-assign pp0-s      (caadr program-s))
            (fc-assign pending-s  (list (cons pp0-s vs0-s)))
            (fc-assign marked-s  '())
            (fc-assign residual-s (list (filter-read (car program-s) division-s))) ; read instruction
            (fc-goto main-loop-s))

    (main-loop-s (fc-if (equal? pending-s '()) stop-s iter-s))

    (iter-s (fc-assign ppvs-s    (car pending-s))
            (fc-assign pp-s      (car ppvs-s))
            (fc-assign vs-s      (cdr ppvs-s))
            (fc-assign pending-s (cdr pending-s))
            (fc-assign marked-s  (append marked-s (list ppvs-s)))
            (fc-assign code-s    (list (cons pp-s vs-s)))
            (fc-assign bb-s      (lookup-bb pp-s (cdr program-s)))
            (fc-goto in-loop-s))

    (in-loop-s (fc-if (equal? bb-s '()) upd-residual-s case-cmd-s))

    (case-cmd-s (fc-assign command-s (car bb-s))
                (fc-assign bb-s (cdr bb-s))
                (fc-goto if-assign-s))

    (if-assign-s (fc-if (equal? (car command-s) 'fc-assign) do-assign-s if-return-s))
    (if-return-s (fc-if (equal? (car command-s) 'fc-return) do-return-s if-goto-s))
    (if-goto-s   (fc-if (equal? (car command-s) 'fc-goto)   do-goto-s   if-if-s))
    (if-if-s     (fc-if (equal? (car command-s) 'fc-if)     do-if-s     if-print-s))
    (if-print-s  (fc-if (equal? (car command-s) 'fc-print)  do-print-s  errlbl-s))

    ; BEGIN CASE

    (do-print-s
     ;  (fc-print debug)
     (fc-goto in-loop-s))

    ; ASSIGN: varname := expr
    (do-assign-s (fc-assign varname-s (cadr command-s))
                 (fc-assign expr-s    (caddr command-s))
                 (fc-if (equal? (dict-ref division-s varname-s) "static") do-assign-st-s do-assign-dy-s))

    ; static-ASSIGN
    (do-assign-st-s
     (fc-assign vs-s (dict-set vs-s varname-s (fc-eval-with-state expr-s vs-s)))
     (fc-goto in-loop-s))

    ; dynamic-ASSIGN
    (do-assign-dy-s
     (fc-assign instr-s (list `(fc-assign ,varname-s ,(fc-reduce expr-s vs-s))))
     (fc-assign code-s  (append code-s instr-s))
     (fc-goto in-loop-s))

    ; IF: if expr then goto pp' else goto pp''
    (do-if-s (fc-assign expr-s (cadr command-s))
             (fc-if (equal? (fc-expr-static? expr-s division-s) #t) do-if-st-s do-if-dy-s))

    ; static-IF
    (do-if-st-s (fc-assign st-cond-s (fc-eval-with-state expr-s vs-s))
                (fc-if (equal? st-cond-s #t) do-if-st-t-s do-if-st-f-s))

    ; static-IF-true
    (do-if-st-t-s (fc-assign pp1-s (caddr command-s))
                  (fc-assign bb-s (lookup-bb pp1-s (cdr program-s)))
                  (fc-goto in-loop-s))

    ; static-IF-false
    (do-if-st-f-s (fc-assign pp1-s (cadddr command-s))
                  (fc-assign bb-s (lookup-bb pp1-s (cdr program-s)))
                  (fc-goto in-loop-s))

    ; dynamic-IF
    (do-if-dy-s   (fc-assign pp1-s     (caddr command-s))
                  (fc-assign pp2-s     (cadddr command-s))
                  (fc-assign pp1vs-s   (cons pp1-s vs-s))
                  (fc-assign pp2vs-s   (cons pp2-s vs-s))
                  (fc-assign npend-s   (list-subtract (list pp1vs-s pp2vs-s) marked-s))
                  (fc-assign pending-s (append pending-s npend-s))
                  (fc-assign instr-s   (list `(fc-if ,(fc-reduce expr-s vs-s) ,pp1vs-s ,pp2vs-s)))
                  (fc-assign code-s    (append code-s instr-s))
                  (fc-goto in-loop-s))

    ; GOTO: goto pp'
    (do-goto-s (fc-assign pp1-s (cadr command-s))
               (fc-assign bb-s (lookup-bb pp1-s (cdr program-s)))
               (fc-goto in-loop-s))

    ; RETURN: return expr
    (do-return-s (fc-assign expr-s    (cadr command-s)) ; cdr returns an expression wrapped into a list
                 (fc-assign reduced-s (fc-reduce expr-s vs-s))
                 (fc-assign instr-s   (list `(fc-return ,reduced-s)))
                 (fc-assign code-s    (append code-s instr-s))
                 (fc-goto in-loop-s))

    ; END CASE
    (upd-residual-s (fc-assign residual-s (append residual-s (list code-s)))
                    (fc-goto main-loop-s))

    (stop-s (fc-return residual-s))

    (errlbl-s (fc-return (format "fc-mix-outer: unknown label ~a" pp-s)))
    )
  )


; ------------------------ 2. Mix with the trick (inner mix) ------------

(define fc-mix-trick
  '((fc-read program division vs0)

    (init (fc-assign pp0      (caadr program))
          (fc-assign pending  (list (cons pp0 vs0)))
          (fc-assign marked  '())
          (fc-assign residual (list (filter-read (car program) division))) ; read instruction
          (fc-goto main-loop))

    (main-loop (fc-if (equal? pending '()) stop iter))

    (iter (fc-assign pp      (caar pending))
          (fc-assign vs      (cdar pending))
          (fc-assign pending (cdr pending))
          (fc-assign marked  (append marked (list (cons pp vs))))
          (fc-goto trick-begin))

    (in-loop (fc-if (equal? bb '()) upd-residual case-cmd))

    (case-cmd (fc-assign command (car bb))
              (fc-assign bb (cdr bb))
              (fc-goto if-assign))

    (if-assign (fc-if (equal? (car command) 'fc-assign) do-assign if-return))
    (if-return (fc-if (equal? (car command) 'fc-return) do-return if-goto))
    (if-goto   (fc-if (equal? (car command) 'fc-goto)   do-goto   if-if))
    (if-if     (fc-if (equal? (car command) 'fc-if)     do-if     errlbl))

    ; BEGIN CASE

    ; ASSIGN: varname := expr
    (do-assign (fc-if (equal? (dict-ref division (cadr command)) "static") do-assign-st do-assign-dy))

    ; static-ASSIGN
    (do-assign-st (fc-assign vs (dict-set vs (cadr command) (fc-eval-with-state (caddr command) vs)))
                  (fc-goto in-loop))

    ; dynamic-ASSIGN
    (do-assign-dy (fc-assign code  (append code (list `(fc-assign ,(cadr command) ,(fc-reduce (caddr command) vs)))))
                  (fc-goto in-loop))

    ; IF: if expr then goto pp' else goto pp''
    (do-if (fc-if (equal? (fc-expr-static? (cadr command) division) #t) do-if-st do-if-dy))

    ; static-IF
    (do-if-st (fc-if (equal? (fc-eval-with-state (cadr command) vs) #t) do-if-st-t do-if-st-f))

    ; static-IF-true
    (do-if-st-t (fc-assign bb (lookup-bb (caddr command) (cdr program)))
                (fc-goto in-loop))

    ; static-IF-false
    (do-if-st-f (fc-assign bb (lookup-bb (cadddr command) (cdr program)))
                (fc-goto in-loop))

    ; dynamic-IF
    (do-if-dy (fc-assign pending (append pending (list-subtract (list (cons (caddr command) vs) (cons (cadddr command) vs)) marked)))
              (fc-assign code    (append code
                                         (list `(fc-if ,(fc-reduce (cadr command) vs)
                                                       ,(cons (caddr command) vs)
                                                       ,(cons (cadddr command) vs)))))
              (fc-goto in-loop))

    ; GOTO: goto pp'
    (do-goto (fc-assign bb (lookup-bb (cadr command) (cdr program)))
             (fc-goto in-loop))

    ; RETURN: return expr
    (do-return (fc-assign code    (append code (list `(fc-return ,(fc-reduce (cadr command) vs)))))
               (fc-goto in-loop))

    ; END CASE
    (upd-residual (fc-assign residual (append residual (list code)))
                  (fc-goto main-loop))

    (stop (fc-return residual))

    ; THE TRICK
    ; pp-dynamic is pp
    (trick-begin (fc-assign labels (append (list pp0) (get-dynamic-labels program division)))
                 (fc-goto trick-cycle))

    (trick-cycle (fc-if (equal? labels '()) trick-error-label trick-iter))

    (trick-iter  (fc-assign pp-static (car labels))
                 (fc-assign labels (cdr labels))
                 (fc-if (equal? pp pp-static) trick-found-pp trick-cycle))

    (trick-found-pp (fc-assign bb (lookup-bb pp-static (cdr program)))
                    (fc-assign code (list (cons pp-static vs)))
                    (fc-goto in-loop))

    (trick-error-label (fc-return (format "trick in fc-mix: unknown label")))

    (errlbl (fc-return (format "fc-mix: unknown label")))
    )
  )

; =======================================================================
; ========================  MIX FOR III PORJECTION  =====================
; =======================================================================

; mix for 3 Futamura projection
; @param program-i  - a program on FC, a quoted value
; @param division-i - a hash [variable -> {static, dynamic}]
; @param vs0-i      - values of the static input
(define fc-mix-inner-inner
  '((fc-read program-i division-i vs0-i)

    (init-i (fc-assign pp0-i      (caadr program-i))
            (fc-assign pending-i  (list (cons pp0-i vs0-i)))
            (fc-assign marked-i  '())
            (fc-assign residual-i (list (filter-read (car program-i) division-i)))
            (fc-goto main-loop-i))

    (main-loop-i (fc-if (equal? pending-i '()) stop-i iter-i))

    (iter-i (fc-assign pp-i      (caar pending-i))
            (fc-assign vs-i      (cdar pending-i))
            (fc-assign pending-i (cdr pending-i))
            (fc-assign marked-i  (append marked-i (list (cons pp-i vs-i))))
            (fc-goto trick-begin-i))

    (in-loop-i (fc-if (equal? bb-i '()) upd-residual-i case-cmd-i))

    (case-cmd-i (fc-assign command-i (car bb-i))
                (fc-assign bb-i (cdr bb-i))
                (fc-goto if-assign-i))

    (if-assign-i (fc-if (equal? (car command-i) 'fc-assign) do-assign-i if-return-i))
    (if-return-i (fc-if (equal? (car command-i) 'fc-return) do-return-i if-goto-i))
    (if-goto-i   (fc-if (equal? (car command-i) 'fc-goto)   do-goto-i   if-if-i))
    (if-if-i     (fc-if (equal? (car command-i) 'fc-if)     do-if-i     errlbl-i))

    ; BEGIN CASE

    ; ASSIGN: varname := expr
    (do-assign-i (fc-if (equal? (dict-ref division-i (cadr command-i)) "static")
                        do-assign-st-i
                        do-assign-dy-i))

    ; static-ASSIGN
    (do-assign-st-i (fc-assign vs-i (dict-set vs-i (cadr command-i) (fc-eval-with-state (caddr command-i) vs-i)))
                    (fc-goto in-loop-i))

    ; dynamic-ASSIGN
    (do-assign-dy-i (fc-assign code-i (append code-i (list `(fc-assign ,(cadr command-i) ,(fc-reduce (caddr command-i) vs-i)))))
                    (fc-goto in-loop-i))

    ; IF: if expr then goto pp' else goto pp''
    (do-if-i (fc-if (equal? (fc-expr-static? (cadr command-i) division-i) #t) do-if-st-i do-if-dy-i))

    ; static-IF
    (do-if-st-i (fc-if (equal? (fc-eval-with-state (cadr command-i) vs-i) #t) do-if-st-t-i do-if-st-f-i))

    ; static-IF-true
    (do-if-st-t-i (fc-assign bb-i (lookup-bb (caddr command-i) (cdr program-i)))
                  (fc-goto in-loop-i))

    ; static-IF-false
    (do-if-st-f-i (fc-assign bb-i (lookup-bb (cadddr command-i) (cdr program-i)))
                  (fc-goto in-loop-i))

    ; dynamic-IF
    (do-if-dy-i (fc-assign pending-i (append pending-i (list-subtract (list (cons (caddr command-i) vs-i) (cons (cadddr command-i) vs-i)) marked-i)))
                (fc-assign code-i    (append code-i
                                             (list `(fc-if ,(fc-reduce (cadr command-i) vs-i)
                                                           ,(cons (caddr command-i) vs-i)
                                                           ,(cons (cadddr command-i) vs-i)))))
                (fc-goto in-loop-i))

    ; GOTO: goto pp'
    (do-goto-i (fc-assign bb-i (lookup-bb (cadr command-i) (cdr program-i)))
               (fc-goto in-loop-i))

    ; RETURN: return expr
    (do-return-i (fc-assign code-i (append code-i (list `(fc-return ,(fc-reduce (cadr command-i) vs-i)))))
                 (fc-goto in-loop-i))

    ; END CASE
    (upd-residual-i (fc-assign residual-i (append residual-i (list code-i)))
                    (fc-goto main-loop-i))

    (stop-i (fc-return residual-i))

    ; THE TRICK
    ; pp-dynamic is pp
    (trick-begin-i (fc-assign labels-i (append (list pp0-i) (get-dynamic-labels program-i division-i)))
                   (fc-goto trick-cycle-i))

    (trick-cycle-i (fc-if (equal? labels-i '()) trick-error-label-i trick-iter-i))

    (trick-iter-i  (fc-assign pp-static-i (car labels-i))
                   (fc-assign labels-i (cdr labels-i))
                   (fc-if (equal? pp-i pp-static-i) trick-found-pp-i trick-cycle-i))

    (trick-found-pp-i (fc-assign bb-i (lookup-bb pp-static-i (cdr program-i)))
                      (fc-assign code-i (list (cons pp-static-i vs-i)))
                      (fc-goto in-loop-i))

    (trick-error-label-i (fc-return (format "trick in fc-mix: unknown label")))

    (errlbl-i (fc-return (format "fc-mix: unknown label")))
    )
  )