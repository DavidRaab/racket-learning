#lang racket/base

(define (is-pair? x)
  (cond
    [(not (list? x)) #f]
    [(null? x)       #f]
    [(eqv? (length x) 2)
     #t]
    [else
     #f]
    ))

(define (filter predicate xs)
  (cond
    [(null? xs) null]
    [(predicate (car xs))
     (cons (car xs) (filter predicate (cdr xs)))]
    [else
     (filter predicate (cdr xs))]
    ))



;-----------------
; Data

(define l1 '(3 Apple 2 Banana 1 Kiwi (A B) (C D) (E F G)))

(filter number? l1)
(filter (lambda (x) (not (number? x))) l1)
(filter is-pair? l1)
(filter list? l1)
(filter (lambda (x) (eq? x 'Apple)) l1)
(filter (lambda (x) (not (eq? x 'Apple))) l1)
(filter (lambda (x) (not (member x '(Apple Banana)))) l1)