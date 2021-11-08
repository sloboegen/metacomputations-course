#lang racket

(provide zip)

(define (zip xs ys)
  (if (null? ys) null
      (match xs
        ['() null]
        [(cons h t) (cons (cons h (car ys)) (zip t (cdr ys)))]
        )
      )
  )
