#lang racket

(define (binop f stack)
  (letrec ([x (first stack)]
           [y (second stack)]
           [z (f x y)])
    (displayln z)
    (cons z (drop stack 2))))

(define (loop stack)
  (let ([input (read-line)])
    (loop
     (cond
       [(string->number input) (cons (string->number input) stack)]
       [(equal? input "+") (binop + stack)]
       [(equal? input "*") (binop * stack)]
       [else stack]))))
       
(loop empty)