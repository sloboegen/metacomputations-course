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

; evaluates `expr` in fc-ns \/ `state`
; @param expr - expression in FC
; @param state - a list of pairs [varname, value]
(define (fc-eval-with-state expr state)
  (dict-for-each state (lambda (k v) (context-set k v)))
  (fc-eval expr))

(eval (context-set 'dict-ref           dict-ref) fc-ns)
(eval (context-set 'dict-set           dict-set) fc-ns)
(eval (context-set 'fc-reduce          fc-reduce) fc-ns)
(eval (context-set 'fc-eval-with-state fc-eval-with-state) fc-ns)
(eval (context-set 'fc-expr-static?    fc-expr-static?) fc-ns)
(eval (context-set 'list-subtract      list-subtract) fc-ns)

; @param program  - a program on FC, a quoted value
; @param division - a hash [variable -> {static, dynamic}]
; @param vs0      - values of the static input
(define fc-mix
  '((fc-read program division vs0)

    (init (fc-assign pp0      (caadr program))
          (fc-assign pending  (list (cons pp0 vs0)))
          (fc-assign marked  '())
          (fc-assign residual (list (car program))) ; read instruction
          (fc-goto main-loop))

    (main-loop (fc-if (equal? pending '()) stop iter))

    (iter (fc-assign ppvs    (car pending))
          (fc-assign pp      (car ppvs))
          (fc-assign vs      (cdr ppvs))
          (fc-assign pending (cdr pending))
          (fc-assign marked  (append marked (list ppvs)))
          (fc-assign code    '())
          (fc-goto findbb))

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
    (do-assign-dy (fc-assign label (cons pp vs))
                  (fc-assign instr (cons label `(fc-assign ,varname ,(fc-reduce expr vs))))
                  (fc-assign code  (append code (list instr)))
                  (fc-goto in-loop))

    ; IF: if expr then goto pp' else goto pp''
    (do-if (fc-assign expr (cadr command))
           (fc-if (equal? (fc-expr-static? expr division) #t) do-if-st do-if-dy))
    
    ; static-IF
    (do-if-st (fc-assign st-cond (fc-eval-with-state expr vs))
              (fc-if (equal? st-cond #t) do-if-st-t do-if-st-f))

    ; static-IF-true
    (do-if-st-t (fc-assign pp (caddr command))
                (fc-goto findbb))

    ; static-IF-false
    (do-if-st-f (fc-assign pp (cadddr command))
                (fc-goto findbb))

    ; dynamic-IF
    (do-if-dy   (fc-assign pp1     (caddr command))
                (fc-assign pp2     (cadddr command))
                (fc-assign pp1vs   (cons pp1 vs))
                (fc-assign pp2vs   (cons pp2 vs))
                (fc-assign npend   (list-subtract (list pp1vs pp2vs) marked))
                (fc-assign pending (append pending npend))
                (fc-assign label   (cons pp vs))
                (fc-assign instr   (cons label `(fc-if ,(fc-reduce expr vs) pp1vs pp2vs)))
                (fc-assign code    (append code (list instr)))
                (fc-goto in-loop))

    ; GOTO: goto pp'
    (do-goto (fc-assign pp (cadr command))
             (fc-goto findbb))

    ; RETURN: return expr
    (do-return (fc-assign expr    (cadr command)) ; cdr returns an expression wrapped into a list
               (fc-assign reduced (fc-reduce expr vs))
               (fc-assign label   (cons pp vs))
               (fc-assign instr   (cons label `(fc-return ,reduced)))
               (fc-assign code    (append code (list instr)))
               (fc-goto in-loop))

    ; END CASE
    (upd-residual (fc-assign residual (append residual code))
                  (fc-goto main-loop))

    (stop (fc-return residual))

    ; BEGIN get basic block by label
    (findbb (fc-assign Uprogram program)
            (fc-goto bbloop))

    (bbloop (fc-if (equal? Uprogram '()) errlbl bbiter))

    (bbiter (fc-assign ULabel   (caar Uprogram))
            (fc-assign UCommand (cdar Uprogram))
            (fc-assign Uprogram (cdr Uprogram))
            (fc-if (equal? ULabel pp) bbfound bbloop))

    (bbfound (fc-assign bb UCommand)
             (fc-goto in-loop))
    ; END get basic block by label

    (errlbl (error (format "fc-mix: unknown label ~a" pp)))
    )
  )

; TESTS
(define find-name
  '((fc-read name namelist valuelist)
    (search (fc-if (equal? name (car namelist)) found cont))
    (cont (fc-assign valuelist (cdr valuelist))
          (fc-assign namelist (cdr namelist))
          (fc-goto search))
    (found (fc-return (car valuelist)))
    ))

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
    (con (fc-assign y 1)
         (fc-if (equal? x 1) ok err))
    
    (ok  (fc-return 42))
    (err (fc-return -1))
  ))

(define find-name-division #hash((name . "static") (namelist . "static") (valuelist . "dynamic")))
(define find-name-vs  #hash((name . z) (namelist . (x y z))))

(define (find-name-mix) (fc-int fc-mix `(,find-name ,find-name-division ,find-name-vs)))
