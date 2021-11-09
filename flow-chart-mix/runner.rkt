#lang racket

(require "fc-int.rkt")
(require "fc-fc-int.rkt")
(require "fc-mix.rkt")
(require "tm-int.rkt")


; ====PROGRAM WITH DIVISIONS====

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


; mix for Turing Machine-interpreter for `tm-example`-program
(define tm-example
  '((0 tm-if 0 tm-goto 3)
    (1 tm-right)
    (2 tm-goto 0)
    (3 tm-write 1))
  )

(define tm-int-division #hash((Q           . "static")
                              (Qtail       . "static")
                              (Instruction . "static")
                              (Operator    . "static")
                              (Symbol      . "static")
                              (Nextlabel   . "static")
                              (Left        . "dynamic")
                              (Right       . "dynamic")))

(define tm-int-vs       `#hash((Q          . ,tm-example)))

; ====FIRST FUTAMURA PROJECTION====
; Testing all mix-implementations
; simple
(define (tm-int-example-mix-simple) (fc-int fc-mix `(,tm-int ,tm-int-division ,tm-int-vs)))
(define (tm-int-example-mix-simple-pp) (pretty-labels-program (tm-int-example-mix-simple)))

; outer (for 2nd projection)
(define (tm-int-example-mix-outer) (fc-int fc-mix-outer `(,tm-int ,tm-int-division ,tm-int-vs)))
(define (tm-int-example-mix-outer-pp) (pretty-labels-program (tm-int-example-mix-outer)))

; with the trick (for 2nd projection)
(define (tm-int-example-mix-trick) (fc-int fc-mix-trick `(,tm-int ,tm-int-division ,tm-int-vs)))
(define (tm-int-example-mix-trick-pp) (pretty-labels-program (tm-int-example-mix-trick)))

; ====SECOND FUTAMURA PROJECTION====

; inner mix (with the trick) division
(define fc-mix-division #hash((program   . "static")
                              (division  . "static")
                              (vs0       . "dynamic")
                              (pp0       . "static")
                              (pending   . "dynamic")
                              (marked    . "dynamic")
                              (residual  . "dynamic")
                              (pp        . "dynamic")
                              (vs        . "dynamic")
                              (labels    . "static")
                              (pp-static . "static")
                              (bb        . "static")
                              (code      . "dynamic")
                              (command   . "static"))
  )

; 1. TM-interpreter

; vs for TM-interpreter
(define fc-mix-tm-vs `#hash((program  . ,tm-int)
                            (division . ,tm-int-division))
  )

; fc-mix-mix-pp is a generated compiler for TM program
(define (fc-mix-mix-tm) (fc-int fc-mix-outer `(,fc-mix-trick ,fc-mix-division ,fc-mix-tm-vs)))

(define (fc-mix-mix-tm-pp) (pretty-labels-program (fc-mix-mix-tm)))

(define (fc-mix-mix-tm-example-vs) `#hash((vs0 . ,tm-int-vs)))

; check correctness
(define (test-generate-compiler-TM) (fc-int (fc-mix-mix-tm-pp) `(,tm-int-vs)))


; 2. FlowChart-interpreter
(define fc-fc-int-division #hash((program  . "static")
                                 (data     . "dynamic")
                                 (vrbs     . "static")
                                 (vals     . "dynamic")
                                 (rdvrbs   . "static")
                                 (rddata   . "dynamic")
                                 (inst     . "static")
                                 (bb-rest  . "static")
                                 (var      . "static")
                                 (expr     . "static")
                                 (vrbsvals . "dynamic")
                                 (lbl      . "static")
                                 (lbl-t    . "static")
                                 (lbl-f    . "static"))
  )

(define fc-mix-vs-fc-int `#hash((program  . ,fc-fc-int)
                                   (division . ,fc-fc-int-division))
                                   )

(define (fc-mix-mix-fc) (fc-int fc-mix-outer `(,fc-mix-trick ,fc-mix-division ,fc-mix-vs-fc-int)))

(define (fc-mix-mix-fc-pp) (pretty-labels-program (fc-mix-mix-fc)))

(define fc-int-vs `#hash((program . ,find-name)))

(define (test-generate-compiler-fc) (fc-int (fc-mix-mix-fc-pp) `(,fc-int-vs)))
