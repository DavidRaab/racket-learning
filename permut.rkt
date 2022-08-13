#lang racket/base
(require racket/list)

(define (insertPos x pos xs)
  (cond
    [(eqv? pos 0)
     (cons x xs)]
    [else
     (cons (car xs) (insertPos x (sub1 pos) (cdr xs)))]))

(define (insert x xss)
  (cond
    [(null? xss)
     (list (list x))]
    [else
     (append-map (lambda (xs)
                   (map (lambda (pos)
                          (insertPos x pos xs))
                        (inclusive-range 0 (length xs))))
                 xss)]
    ))
 
(define (permut xs)
  (cond
    [(null? xs) null]
    [else
     (insert (car xs) (permut (cdr xs)))]))
    

; Test
(permut '(1 2 3 4))