#lang racket/base
(require racket/list)

(define (concat xs)
  (foldl append null xs))

(define (cartesian xs ys)
  (concat
   (for/list ([x xs])
     (for/list ([y ys])
       (list x y)))))

(define l1 '(a b c))
(define l2 '(1 2 3))

(cartesian l1 l2)
