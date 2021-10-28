#lang racket

(require "../hw1/fc-int.rkt")
(require "../hw1/tm-int.rkt")

(provide fc-mix)


(define (fc-reduce expr static-state)
  (match expr
    [`(,h . ,t) (cons (fc-reduce h static-state) (fc-reduce t static-state))]
    [`,v (if (dict-has-key? static-state v) (dict-ref static-state v) v)]
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
    (println "COUNT OF STATES")
    (println num)
    lbl2num
    )
  )

(define (pretty-labels-program pgm)
  (let ([lbl2num (numerate-labels pgm)]
        [pp-pgm  (list (car pgm))]) ; read-instruction without label
    (for ([lbl-bb (cdr pgm)])
      (match lbl-bb
        [(cons lbl bb) (set! pp-pgm (append pp-pgm (list (cons (dict-ref lbl2num lbl) (list (pretty-labels-bb bb lbl2num))))))]
        ))
    pp-pgm)
  )

(define (pretty-labels-bb bb lbl2num)
  (match bb
    [`(,h)  (match h
              [`(fc-goto ,lbl)   (list `(fc-goto ,(dict-ref lbl2num lbl)))]
              [`(fc-if ,c ,t ,f) (list `(fc-if c ,(dict-ref lbl2num t) ,(dict-ref lbl2num f)))]
              [other             (list other)]) ; other instructions doesn't contain labels
            ]
    [`(,h . ,t) (append (list h) (pretty-labels-bb t lbl2num))] ; don't modify assign

    ))


; TESTS
; Basics
(define simple-program
  '((fc-read x)
    (fake   (fc-assign x 1)
            (fc-goto fake2))
    (fake2  (fc-goto result))
    (result (fc-assign y 42)
            (fc-return (+ x y)))
    ))

(define if-program-st
  '((fc-read x)
    (con (fc-assign y 1)
         (fc-if (equal? y 1) ok err))
    (ok  (fc-return y))
    (err (fc-return -1))
    ))

(define if-program-dy
  '((fc-read x)
    (con (fc-assign x 1)
         (fc-if (equal? x 1) ok err))

    (ok  (fc-return 42))
    (err (fc-return -1))
    ))

(define if-program-dy-division #hash((y . "static") (x . "dynamic")))
(define if-program-dy-vs #hash((y . 0)))
(define (if-program-dy-mix) (fc-int fc-mix `(,if-program-dy ,if-program-dy-division ,if-program-dy-vs)))
(define (if-program-dy-mix-pp) (pretty-labels-program (if-program-dy-mix)))

; Find-name
(define find-name
  '((fc-read name namelist valuelist)
    (search (fc-if (equal? name (car namelist)) found cont))
    (cont (fc-assign valuelist (cdr valuelist))
          (fc-assign namelist (cdr namelist))
          (fc-goto search))
    (found (fc-return (car valuelist)))
    ))


(define find-name-division #hash((name . "static") (namelist . "static") (valuelist . "dynamic")))
(define find-name-vs  #hash((name . z) (namelist . (x y z))))

(define (find-name-mix) (fc-int fc-mix `(,find-name ,find-name-division ,find-name-vs)))

(define (find-name-mix-pp) (pretty-labels-program (find-name-mix)))

(define tm-example
  '((0 tm-if 0 tm-goto 3)
    (1 tm-right)
    (2 tm-goto 0)
    (3 tm-write 1))
  )

; Turing Machine
(define tm-int-division #hash((Q           . "static")
                              (Qtail       . "static")
                              (Instruction . "static")
                              (Operator    . "static")
                              (Symbol      . "static")
                              (Nextlabel   . "static")
                              (Left        . "dynamic")
                              (Right       . "dynamic")))

(define tm-int-vs `#hash((Q     . ,tm-example)
                         (Right . (1 1 1 0 1 0 1))))

(define (tm-int-example-mix) (fc-int fc-mix `(,tm-int ,tm-int-division ,tm-int-vs)))

(define (tm-int-example-mix-pp) (pretty-labels-program (tm-int-example-mix)))


;===============FC-MIX WITH THE TRICK===============================================


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
     ; (fc-print "static assn: ")
     ; (fc-print `(,varname-s ,expr-s))
     (fc-assign vs-s (dict-set vs-s varname-s (fc-eval-with-state expr-s vs-s)))
     (fc-goto in-loop-s))

    ; dynamic-ASSIGN
    (do-assign-dy-s
     ; (fc-print "dynamic assn: ")
     ; (fc-print `(,varname-s ,expr-s))
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

; MIX WITH THE TRICK
; inner mix
(define fc-mix-trick
  '((fc-read program division vs0)

    (init (fc-assign pp0      (caadr program))
          (fc-assign pending  (list (cons pp0 vs0)))
          (fc-assign marked  '())
          (fc-assign residual (list (filter-read (car program) division))) ; read instruction
          (fc-goto main-loop))

    (main-loop (fc-if (equal? pending '()) stop iter))

    (iter (fc-assign pp      (car ppvs))
          (fc-assign vs      (cdr ppvs))
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
    (if-if     (fc-if (equal? (car command) 'fc-if)     do-if     if-print))
    (if-print  (fc-if (equal? (car command) 'fc-print)  do-print  errlbl))

    ; BEGIN CASE
    (do-print 
            ; (fc-assign debug command)
              (fc-goto in-loop))

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
    (do-if-st (fc-if (equal? (fc-eval-with-state expr vs) #t) do-if-st-t do-if-st-f))

    ; static-IF-true
    (do-if-st-t (fc-assign bb (lookup-bb (caddr command) (cdr program)))
                (fc-goto in-loop))

    ; static-IF-false
    (do-if-st-f 
                (fc-assign bb (lookup-bb (cadddr command) (cdr program)))
                (fc-goto in-loop))

    ; dynamic-IF
    (do-if-dy   (fc-assign pending (append pending (list-subtract (list (cons (caddr command) vs) (cons (cadddr command) vs)) marked)))
                (fc-assign code (append code (list `(fc-if ,(fc-reduce expr vs)
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
    (trick-begin (fc-assign pgm-rest (cdr program))
                 (fc-goto trick-cycle))

    (trick-cycle (fc-if (equal? pgm-rest '()) trick-error-label trick-iter))

    (trick-iter  (fc-assign pp-static (caar pgm-rest))
                 (fc-assign cur-bb (cdar pgm-rest))
                 (fc-assign pgm-rest (cdr pgm-rest))
                 (fc-if (equal? pp pp-static) trick-found-pp trick-cycle))

    (trick-found-pp (fc-assign bb cur-bb)
                    (fc-assign code (list (cons pp-static vs)))
                    (fc-goto in-loop))

    (trick-error-label (fc-return (format "trick in fc-mix: unknown label ~a" ,pp)))

    (errlbl (fc-return (format "fc-mix: unknown label ~a" ,pp)))
    )
  )

(define fc-mix-division #hash((program   . "static")
                              (division  . "static")
                              (vs0       . "dynamic")
                              (pp0       . "static")
                              (pending   . "dynamic")
                              (marked    . "dynamic")
                              (residual  . "dynamic")
                              (pp        . "dynamic")
                              (vs        . "dynamic")
                              (pgm-rest  . "static")
                              (pp-static . "static")
                              (cur-bb    . "static")
                              (bb        . "static")
                              (code      . "dynamic")
                              (command   . "static"))
  )

(define fc-mix-vs `#hash((program  . ,tm-int)
                         (division . ,tm-int-division))
  )

(define (fc-mix-mix) (fc-int fc-mix-outer `(,fc-mix-trick ,fc-mix-division ,fc-mix-vs)))

(define (fc-mix-mix-pp) (pretty-labels-program (fc-mix-mix)))
