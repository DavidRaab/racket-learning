#lang racket/base
(require racket/list)

(define (indexed xs)
  (for/list
      ([i (range (length xs))]
       [x xs])
    (list i x)))

(define (permut x)
  (cond
    [(null? x) (error "Fucked Up!")]
    [(eqv? x 1)
     (list (list 0))]
    [else
     (append-map
      (lambda (xs)
        (map
         (lambda (num)
           (cons num xs))
         (range 0 x)))
      (permut (sub1 x)))]))
