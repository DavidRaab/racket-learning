#lang racket

(define (evens-only* xs)
  (cond
    [(null? xs)
     null]
    [(list? (car xs))
     (list*
     (evens-only* (car xs))
     (evens-only* (cdr xs)))]
    [(even? (car xs))
     (list* (car xs) (evens-only* (cdr xs)))]
    [else
     (evens-only* (cdr xs))]))

(define l1 '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(evens-only* l1)
