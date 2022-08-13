#lang racket/base
(require racket/list)

(define (map f xs)
  (cond
    [(null? xs)
     null]
    [else
     (cons (f (car xs)) (map f (cdr xs)))]))

(define (mapc f xs cont)
  (cond
    [(null? xs)
     (cont null)]
    [else
     (mapc f (cdr xs)
          (lambda (rest)
            (cont (cons (f (car xs)) rest))))]))

(define (id x) x)
(define (square x) (* x x))

(define lst (range 1 100000))

(printf "map:  ")
(define x (time (map  square lst)))
(printf "napc: ")
(define y (time (mapc square lst id)))
