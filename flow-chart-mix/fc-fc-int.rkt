#lang racket

(require "fc-int.rkt")

(provide fc-fc-int)

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

(define (fc-eval-with-vars-vals expr vrbs vals)
  (for ([vrb vrbs]
        [val vals])
    (context-set vrb val))
  (fc-eval expr))

; updates lists `vrbs` and `vals` after assignment `x := e`
; @param x -- a variable
; @param e -- a new value for `x`
; @param vrbs -- a list of all variables
; @param vals -- a list of values (a value of ith variable (vrbs[i]) is vals[i])
(define (do-assign-vrbval x e vrbs vals)
  (if (equal? (member x vrbs) #f)
      (cons (append vrbs (list x)) (append vals (list e)))
      (let ([i (index-of vrbs x)]) (cons vrbs (list-set vals i e)))
      ))

(eval (context-set 'do-assign-vrbval       do-assign-vrbval)       fc-ns)
(eval (context-set 'fc-eval-with-vars-vals fc-eval-with-vars-vals) fc-ns)
(eval (context-set 'lookup-bb              lookup-bb)              fc-ns)

; Flow Chart interpreter written on Flow Chart
(define fc-fc-int
  '((fc-read pgm-fc data)

    ; hardcode all input variables
    (init    (fc-assign vrbs '(name namelist valuelist))
             (fc-assign vals '())
             (fc-goto   do-read))

    (do-read (fc-assign vals (append vals (list (car data))))
             (fc-assign data (cdr data))
             (fc-if (equal? data '()) loop-bbs do-read))

    (loop-bbs (fc-assign bb-rest (cdadr pgm-fc))
              (fc-goto loop-bb))

    (loop-bb (fc-assign inst (car bb-rest))
             (fc-assign bb-rest (cdr bb-rest))
             (fc-if (equal? bb-rest '()) do-jump do-assn)) ; last instruction is jump instruction

    (do-assn (fc-assign var (cadr inst))
             (fc-assign expr (caddr inst))
             (fc-assign vrbsvals (do-assign-vrbval var (fc-eval-with-vars-vals expr vrbs vals) vrbs vals))
             (fc-assign vrbs (car vrbsvals))
             (fc-assign vals (cdr vrbsvals))
             (fc-goto loop-bb))

    (do-jump   (fc-if (equal? (car inst) 'fc-goto) do-goto if-if))
    (if-if     (fc-if (equal? (car inst) 'fc-if) do-if if-return))
    (if-return (fc-if (equal? (car inst) 'fc-return) do-return error-inst))

    (do-goto   (fc-assign lbl (cadr inst))
               (fc-assign bb-rest (lookup-bb lbl pgm-fc))
               (fc-goto loop-bb))

    (do-if     (fc-assign lbl-t (caddr inst))
               (fc-assign lbl-f (cadddr inst))
               (fc-if (fc-eval-with-vars-vals (cadr inst) vrbs vals) do-if-t do-if-f))

    (do-if-t   (fc-assign bb-rest (lookup-bb lbl-t pgm-fc))
               (fc-goto loop-bb))

    (do-if-f   (fc-assign bb-rest (lookup-bb lbl-f pgm-fc))
               (fc-goto loop-bb))

    (do-return (fc-return (fc-eval-with-vars-vals (cadr inst) vrbs vals)))

    (error-inst (fc-return "error instruction"))
    ))


;; TESTS
; Simple program
(define pgm-simple
  '(
    (fc-read a b)
    (found
     (fc-assign x 42)
     (fc-assign y (+ x 1))
     (fc-return (+ a x)))
    )
  )

(define (fc-fc-int-simple-test) (fc-int fc-fc-int `(,pgm-simple (1 2))))

; Find-name
(define find-name
  '((fc-read name namelist valuelist)
    (search (fc-if (equal? name (car namelist)) found cont))
    (cont (fc-assign valuelist (cdr valuelist))
          (fc-assign namelist (cdr namelist))
          (fc-goto search))
    (found (fc-return (car valuelist)))
    ))

(define (fc-fc-int-findname-test) (fc-int fc-fc-int `(,find-name (y (x y z) (1 2 3)))))
