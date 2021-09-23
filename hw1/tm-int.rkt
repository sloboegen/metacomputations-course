#lang racket

(require "fc-int.rkt")
(provide tm-int)

; saved just for debug
; (define tm-inst-by-label
;   '((fc-read Q lbl)

;     (init (fc-assign Qtail Q)
;           (fc-goto loop))

;     (loop (fc-if (equal? Qtail '()) err iter))

;     (iter (fc-assign Label (caar Qtail))
;           (fc-assign Command (cdar Qtail))
;           (fc-assign Qtail (cdr Qtail))
;           (fc-if (equal? Label lbl) found loop))

;     (err (fc-return "tm-int: unknown label"))
;     (found (fc-return (cons (cons Label Command) Qtail)))
;     )
;   )

; implementation is based on the book
; "Partial Evaluation and Automatic Program Generation" (Jones, Gomard, Sestoft), page 74
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
              (fc-goto gotoinit))

    (doif     (fc-assign Symbol (caddr Instruction))
              (fc-assign Nextlabel (car (cddddr Instruction)))
              (fc-if (equal? Symbol (car Right)) gotoinit loop))

    (stop (fc-return Right))

    ; utils for goto
    ; BEGIN get program-tail by label
    (gotoinit (fc-assign UQtail Q)
              (fc-goto uloop))

    (uloop (fc-if (equal? UQtail '()) errlbl uiter))

    (uiter (fc-assign ULabel (caar UQtail))
           (fc-assign UCommand (cdar UQtail))
           (fc-assign UQtail (cdr UQtail))
           (fc-if (equal? ULabel Nextlabel) ufound uloop))

    (ufound (fc-assign Qtail (cons (cons ULabel UCommand) UQtail))
            (fc-goto loop))
    ; END get program-tail by label

    (errlbl (fc-return "tm-int: unknown label"))
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
