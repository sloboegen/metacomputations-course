#lang racket

(require "fc-int.rkt")
(require "fc-fc-int.rkt")
(require "fc-mix.rkt")
(require "tm-int.rkt")


; =======================================================================
; ======================== PROGRAMS AND DIVISIONS =======================
; =======================================================================

; Find-name
(define find-name
  '((fc-read name namelist valuelist)
    (search (fc-if (equal? name (car namelist)) found cont))
    (cont (fc-assign valuelist (cdr valuelist))
          (fc-assign namelist (cdr namelist))
          (fc-goto search))
    (found (fc-return (car valuelist)))
    ))

(define find-name-division #hash((name      . "static")
                                 (namelist  . "static")
                                 (valuelist . "dynamic"))
  )

(define find-name-vs  #hash((name . z) (namelist . (x y z))))

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

; =======================================================================
; ======================== I FUTAMURA PROJECTION ========================
; =======================================================================

; Testing all mix-implementations
; simple
(define (tm-int-example-mix-simple) (fc-int fc-mix `(,tm-int ,tm-int-division ,tm-int-vs)))
(define (tm-int-example-mix-simple-pp) (pretty-labels-program (tm-int-example-mix-simple)))

(define (find-name-mix-simple) (fc-int fc-mix `(,find-name ,find-name-division ,find-name-vs)))
(define (find-name-mix-simple-pp) (pretty-labels-program (find-name-mix-simple)))

; outer (for 2nd projection)
(define (tm-int-example-mix-outer) (fc-int fc-mix-outer `(,tm-int ,tm-int-division ,tm-int-vs)))
(define (tm-int-example-mix-outer-pp) (pretty-labels-program (tm-int-example-mix-outer)))

(define (find-name-mix-outer) (fc-int fc-mix-outer `(,find-name ,find-name-division ,find-name-vs)))
(define (find-name-mix-outer-pp) (pretty-labels-program (find-name-mix-outer)))

; with the trick (for 2nd projection)
(define (tm-int-example-mix-trick) (fc-int fc-mix-trick `(,tm-int ,tm-int-division ,tm-int-vs)))
(define (tm-int-example-mix-trick-pp) (pretty-labels-program (tm-int-example-mix-trick)))

(define (find-name-mix-trick) (fc-int fc-mix-trick `(,find-name ,find-name-division ,find-name-vs)))
(define (find-name-mix-trick-pp) (pretty-labels-program (find-name-mix-trick)))

; inner-inner mix (for 3d projection)
(define (tm-int-example-mix-inner-inner) (fc-int fc-mix-inner-inner `(,tm-int ,tm-int-division ,tm-int-vs)))
(define (tm-int-example-mix-inner-inner-pp) (pretty-labels-program (tm-int-example-mix-inner-inner)))

(define (find-name-mix-inner-inner) (fc-int fc-mix-inner-inner `(,find-name ,find-name-division ,find-name-vs)))
(define (find-name-mix-inner-inner-pp) (pretty-labels-program (find-name-mix-inner-inner)))

; =======================================================================
; ======================== II FUTAMURA PROJECTION =======================
; =======================================================================

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

; ------------------------ 1. TM-interpreter ----------------------------

; vs for TM-interpreter
(define fc-mix-tm-vs `#hash((program  . ,tm-int)
                            (division . ,tm-int-division))
  )

; self-application, compiler for TM
(define (fc-mix-mix-tm) (fc-int fc-mix-outer `(,fc-mix-trick ,fc-mix-division ,fc-mix-tm-vs)))

; compiler for TM with pretty labels
(define (TM-compiler-II) (pretty-labels-program (fc-mix-mix-tm)))

; compile `tm-example` program
(define (tm-example-compiled-II) (fc-int (TM-compiler-II) `(,tm-int-vs)))

; run compiled program `tm-example`
(define (test-compiled-tm-example-II) (fc-int (tm-example-compiled-II) (list '(1 1 1 0 1 0 1))))

; ------------------------ 2. Flow Chart-interpreter --------------------

(define fc-fc-int-division #hash((pgm-fc   . "static" )
                                 (data     . "dynamic")
                                 (vrbs     . "dynamic")
                                 (vals     . "dynamic")
                                 (inst     . "static" )
                                 (bb-rest  . "static" )
                                 (var      . "static" )
                                 (expr     . "static" )
                                 (vrbsvals . "dynamic")
                                 (lbl      . "static" )
                                 (lbl-t    . "static" )
                                 (lbl-f    . "static" ))
  )

(define fc-mix-vs-fc-int `#hash((program  . ,fc-fc-int)
                                (division . ,fc-fc-int-division))
  )

; self-application, compiler for Flow Chart
(define (fc-mix-mix-fc) (fc-int fc-mix-outer `(,fc-mix-trick ,fc-mix-division ,fc-mix-vs-fc-int)))

; compiler for Flow Chart with pretty labels
(define (FC-compiler-II) (pretty-labels-program (fc-mix-mix-fc)))

; compile `find-name` program
(define fc-int-vs `#hash((pgm-fc . ,find-name)))
(define (find-name-compiled-not-pp) (fc-int (FC-compiler-II) `(,fc-int-vs)))
(define (find-name-compiled-II) (pretty-labels-program (find-name-compiled-not-pp)))

; run compiled program `find-name`
(define (test-compiled-find-name-II) (fc-int (find-name-compiled-II) (list '(y (x y z) (1 2 3)))))

; =======================================================================
; ======================== III FUTAMURA PROJECTION ======================
; =======================================================================

; III projection: mix1 (mix2 (mix3))
; * mix1 is `fc-mix-outer` from fc-mix.rkt
; * mix2 is `fc-mix-trick` from fc-mix.rkt
; * mix3 is `fc-mix-inner-inner` from fc-mix.rkt

(define fc-mix-inner-inner-division #hash((program-i   . "static")
                                          (division-i  . "static")
                                          (vs0-i       . "dynamic")
                                          (pp0-i       . "static")
                                          (pending-i   . "dynamic")
                                          (marked-i    . "dynamic")
                                          (residual-i  . "dynamic")
                                          (pp-i        . "dynamic")
                                          (vs-i        . "dynamic")
                                          (labels-i    . "static")
                                          (pp-static-i . "static")
                                          (bb-i        . "static")
                                          (code-i      . "dynamic")
                                          (command-i   . "static"))
  )

(define fc-mix-vs-fc-mix `#hash((program  . ,fc-mix-inner-inner)
                                (division . ,fc-mix-inner-inner-division))
  )

; returns compiler generator
(define (fc-mix-mix-mix) (fc-int fc-mix-outer `(,fc-mix-trick ,fc-mix-division ,fc-mix-vs-fc-mix)))

; returns compiler generator with pretty labels
(define (fc-mix-mix-mix-pp) (pretty-labels-program (fc-mix-mix-mix)))


; ------------------------ 1. TM-compiler -------------------------------

(define tm-vs-III `#hash((program-i  . ,tm-int)
                         (division-i . ,tm-int-division))
  )

; generated compiler for TM by TM interpreter
(define (TM-compiler-III) (fc-int (fc-mix-mix-mix) `(,tm-vs-III)))

; compile `tm-example` program by generated TM compiler
(define (tm-example-compiled-III) (fc-int (TM-compiler-III) `(,tm-int-vs)))

; run compiled `tm-example` on input
(define (test-compiled-tm-example-III) (fc-int (tm-example-compiled-III) (list '(1 1 1 0 1 0 1))))

; ------------------------------------------------------------------------------
;| `tm-example` compiled by generated compiler looks like                      |
; ------------------------------------------------------------------------------
;
; (define tm-example-compiled-pretty-labels
; '((fc-read Right)
;   (0 (fc-assign Left '())
;      (fc-if (equal? '0 (car Right)) 1 2))
;   (1 (fc-assign Right (cons '1 (cdr Right)))
;      (fc-return Right))
;   (2
;    (fc-assign Left (cons (car Right) Left))
;    (fc-assign Right (cdr Right))
;    (fc-if (equal? '0 (car Right)) 1 2)))
; )
; ------------------------------------------------------------------------------


; ------------------------ 2. FlowChart-compiler -------------------------------

(define fc-vs-III `#hash((program-i   . ,fc-fc-int)
                         (division-i  . ,fc-fc-int-division))
  )

; generated compiler for FlowChart by FlowChart interpreter
(define (FC-compiler-III) (fc-int (fc-mix-mix-mix) `(,fc-vs-III)))

; compile `find-name` program by generated FlowChart compiler
(define (find-name-compiled-III) (fc-int (FC-compiler-III) `(,fc-int-vs)))
(define (find-name-compiled-III-pp) (pretty-labels-program (find-name-compiled-III)))

; ; run compiled `find-name` on input
(define (test-compiled-find-name-III) (fc-int (find-name-compiled-III) (list '(y (x y z) (1 2 3)))))

; ------------------------------------------------------------------------------
;| `find-name` compiled by generated compiler looks like                       |
; ------------------------------------------------------------------------------
;
; (define find-name-compiled-pretty-labels
;   '((fc-read data)
;     (0
;      (fc-assign vrbs '(name namelist valuelist))
;      (fc-assign vals '())
;      (fc-assign vals (append vals (list (car data))))
;      (fc-assign data (cdr data))
;      (fc-if (equal? data '()) 1 2))
;     (1
;      (fc-if
;       (fc-eval-with-vars-vals
;        (cadr '(fc-if (equal? name (car namelist)) found cont))
;        vrbs
;        vals)
;       3
;       4))
;     (2
;      (fc-assign vals (append vals (list (car data))))
;      (fc-assign data (cdr data))
;      (fc-if (equal? data '()) 1 2))
;     (3
;      (fc-return
;       (fc-eval-with-vars-vals (cadr '(fc-return (car valuelist))) vrbs vals)))
;     (4
;      (fc-assign
;       vrbsvals
;       (do-assign-vrbval
;        'valuelist
;        (fc-eval-with-vars-vals '(cdr valuelist) vrbs vals)
;        vrbs
;        vals))
;      (fc-assign vrbs (car vrbsvals))
;      (fc-assign vals (cdr vrbsvals))
;      (fc-assign
;       vrbsvals
;       (do-assign-vrbval
;        'namelist
;        (fc-eval-with-vars-vals '(cdr namelist) vrbs vals)
;        vrbs
;        vals))
;      (fc-assign vrbs (car vrbsvals))
;      (fc-assign vals (cdr vrbsvals))
;      (fc-if
;       (fc-eval-with-vars-vals
;        (cadr '(fc-if (equal? name (car namelist)) found cont))
;        vrbs
;        vals)
;       5
;       6))
;     (5
;      (fc-return
;       (fc-eval-with-vars-vals (cadr '(fc-return (car valuelist))) vrbs vals)))
;     (6
;      (fc-assign
;       vrbsvals
;       (do-assign-vrbval
;        'valuelist
;        (fc-eval-with-vars-vals '(cdr valuelist) vrbs vals)
;        vrbs
;        vals))
;      (fc-assign vrbs (car vrbsvals))
;      (fc-assign vals (cdr vrbsvals))
;      (fc-assign
;       vrbsvals
;       (do-assign-vrbval
;        'namelist
;        (fc-eval-with-vars-vals '(cdr namelist) vrbs vals)
;        vrbs
;        vals))
;      (fc-assign vrbs (car vrbsvals))
;      (fc-assign vals (cdr vrbsvals))
;      (fc-if
;       (fc-eval-with-vars-vals
;        (cadr '(fc-if (equal? name (car namelist)) found cont))
;        vrbs
;        vals)
;       5
;       6)))
;   )
; ------------------------------------------------------------------------------
