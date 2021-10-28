#lang racket

(require "fc-int.rkt")
(provide tm-int)

; returns tails of pgm starting from lbl
(define (tm-tail-by-label lbl pgm)
  (match pgm
    ['() (error (format "tm-int: no such label ~a" lbl))]
    [`(,h . ,t) (if (equal? (car h) lbl) (cons h t) (tm-tail-by-label lbl t))]))

(eval (context-set 'tm-tail-by-label tm-tail-by-label) fc-ns)

(define tm-int
  '((fc-read Q Right)

    (init (fc-assign Qtail Q)
          (fc-assign Left '())
          (fc-goto loop))

    (loop (fc-if (equal? Qtail '()) stop cont))

    (cont (fc-assign Instruction (car Qtail))
          (fc-assign Qtail (cdr Qtail))
          (fc-assign Operator (cadr Instruction))
          (fc-goto ifright))

    (ifright (fc-if (equal? Operator 'tm-right) doright ifleft))
    (ifleft  (fc-if (equal? Operator 'tm-left) doleft ifwrite))
    (ifwrite (fc-if (equal? Operator 'tm-write) dowrite ifgoto))
    (ifgoto  (fc-if (equal? Operator 'tm-goto) dogoto ifif))
    (ifif    (fc-if (equal? Operator 'tm-if) doif errinst))


    (doright  (fc-assign Left (cons (car Right) Left))
              (fc-assign Right (cdr Right))
              (fc-goto loop))

    (doleft   (fc-assign Right (cons (car Left) Right))
              (fc-assign Left (cdr Left))
              (fc-goto loop))

    (dowrite  (fc-assign Symbol (caddr Instruction))
              (fc-assign Right (cons Symbol (cdr Right)))
              (fc-goto loop))

    (dogoto   (fc-assign Nextlabel (caddr Instruction))
              (fc-assign Qtail (tm-tail-by-label Nextlabel Q))
              (fc-goto loop))

    (doif     (fc-assign Symbol (caddr Instruction))
              (fc-assign Nextlabel (car (cddddr Instruction)))
              (fc-if (equal? Symbol (car Right)) jump loop))

    (jump     (fc-assign Qtail (tm-tail-by-label Nextlabel Q))
              (fc-goto loop))

    (stop (fc-return Right))

    (errlbl  (fc-return "tm-int: unknown label"))
    (errinst (fc-return "tm-int: unknown instruction"))
    )
  )

(define tm-simple
  '((0 tm-goto 3)
    (1 tm-goto 2)
    (2 tm-right)
    (3 tm-right))
  )

(define tm-example
  '((0 tm-if 0 tm-goto 3)
    (1 tm-right)
    (2 tm-goto 0)
    (3 tm-write 1))
  )
