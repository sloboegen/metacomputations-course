#lang racket

(require "utils.rkt")
(provide fc-int context-set fc-eval fc-ns)

(define DEBUG true)

; the base block definition is slightly different from the definition in the task1.pdf
; basic block (bb) is a sequence of assignments*; jump-instruction

; namespace (aka context)
(define fc-ns (make-base-namespace))
(define (context-set x e) (namespace-set-variable-value! x e #f fc-ns))
(define (bb-by-label lbl2bb lbl) (dict-ref lbl2bb lbl))

(define (fc-eval expr) (eval expr fc-ns))

; constructions of Flow Chart
(struct fc-read (vars))

(struct fc-assign (x e))

(struct fc-goto (label))

(struct fc-if (c t f))

(struct fc-return (e))

(struct fc-print (e))

; runs interpreter for an assignment instruction
; @param assn - an assignmnet in Flow Chart
(define (fc-run-assign assn)
  (
   match assn
    [`(fc-assign ,x ,e) (context-set x (fc-eval e))]
    [`(fc-print ,e) (if DEBUG (println (format "**DEBUG** print ~a" (fc-eval e)))
                        (error "print instruction supported only in DEBUG"))]
    [_ (if DEBUG  (print (format "**DEBUG** error: assignment error ~a" assn))
           (error (format "fc-int: assignment error ~a" assn)))]
    ))

; runs interpreter for a jump instruction
; @param jump - a jump instruction
; @param lbl2bb - a dict [label -> basic block]
(define (fc-run-jump jump lbl2bb)
  (
   match jump
    [`(fc-goto ,lbl) (fc-run-bb (bb-by-label lbl2bb lbl) lbl2bb)]
    [`(fc-if ,c ,t ,f)
     (if (fc-eval c)
         (fc-run-bb (bb-by-label lbl2bb t) lbl2bb)
         (fc-run-bb (bb-by-label lbl2bb f) lbl2bb))]
    [`(fc-return ,e) (fc-eval e)]
    [_ (if DEBUG (print (format "**DEBUG** error: incorrect jump instruction ~a" jump))
           (error (format "fc-int: incorrect jump instruction ~a" jump)))]
    )
  )

; runs interpreter on basic block `cur-bb`
; @param cur-bb - a sequence of instruction in current basic block. (car cur-bb) is the current instruction
; @param lbl2bb - a dict [label -> basic block]
(define (fc-run-bb cur-bb lbl2bb)
  (
   match cur-bb
    ['() (error "fc-int: empty basic block")]
    [`(,h) (fc-run-jump h lbl2bb)]
    [`(,h . ,t)
     (fc-run-assign h)
     (fc-run-bb t lbl2bb)]
    ))

; runs interpreter for read instruction
; adds pairs (var, value) into fc-namespace
; @param read-instr - read instruction
; @param data - input
(define (fc-run-read read-instr data)
  (match read-instr
    [`(fc-read ,h . ,t)
     (map (lambda (xd)
            (match xd
              [(cons x d) (context-set x d)])
            )
          (zip (cons h t) data))
     ]
    [_ (error (format "fc-int: incorrect read instruction ~a" read-instr))]
    ))


; converts a program to a dict [label -> basic block]
; @param pgm - a program on Flow Chart (quoted value in Racket)
(define (pgm2insts pgm)
  (let ([lbl2bb #hash()])
    (for ([bb pgm])
      (match bb
        [`(,lbl ,h . ,t) (set! lbl2bb (dict-set lbl2bb lbl (cons h t)))]
        [`(,lbl ,h) (set! lbl2bb (dict-set lbl2bb lbl h))]
        [_ (if DEBUG  (print (format "**DEBUG** error: incorrect basic block ~a" bb))
               (error (format "fc-int: incorrect basic block: ~a" bb)))]
        ))
    lbl2bb
    )
  )

; runs interpreter on basic blocks
; @param pgm - a program without read command
(define (fc-run-bbs pgm)
  (let ([lbl2bb (pgm2insts pgm)])
    (let ([start-bb (bb-by-label lbl2bb (caar pgm))])
      (fc-run-bb start-bb lbl2bb)
      )
    )
  )

; runs interpreter
; @param pgm - program on Flow Chart (quoted value)
; @param data - input (quoted value)
(define (fc-int pgm data)
  (fc-run-read (car pgm) data)
  (fc-run-bbs (cdr pgm))
  )

; examples
(define pgm-simple
  '(
    (fc-read a b)
    (found
     (fc-assign x 42)
     (fc-assign y (+ x 1))
     (fc-return (+ a b)))
    )
  )

(define find_name
  '((fc-read name namelist valuelist)
    (search (fc-if (equal? name (car namelist)) found cont))
    (cont (fc-assign valuelist (cdr valuelist))
          (fc-assign namelist (cdr namelist))
          (fc-goto search))
    (found (fc-return (car valuelist)))
    ))
