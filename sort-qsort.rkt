#lang racket/base

; A Basic Quick-Sort implementation on immutable list
;
; * Implemented for learning the algorithm and Recursion
; * Speed is not considered in this implementation
; * Tail-Recursion also not considered
;-----------------------------------------------------------

; Returns a list of all elements smaller or equal to pivot
; Example:
;   (smallers 5 '(3 10 2 60 4))
;   (3 2 4)
(define (smallers pivot xs)
  (cond
    [(null? xs)
     null]
    [(<= (car xs) pivot)
     (cons (car xs) (smallers pivot (cdr xs)))]
    [else
     (smallers pivot (cdr xs))]))

; Another implementation of 'smallers' using
; higher-order function 'filter'
(define (smallersf pivot xs)
  (filter (lambda (x) (<= x pivot)) xs))

; Returns a list of all elements greater to pivot
; Example:
;   (greaters 5 '(3 10 2 60 4))
;   (10 60)
(define (greaters pivot xs)
  (cond
    [(null? xs)
     null]
    [(> (car xs) pivot)
     (cons (car xs) (greaters pivot (cdr xs)))]
    [else
     (greaters pivot (cdr xs))]))

; Another implementation of 'greaters' using
; higher-order function 'filter'
(define (greatersf pivot xs)
  (filter (lambda (x) (> x pivot)) xs))

; Quick-Sort (qsort)
; 1. Take first element of list as pivot
; 2. qsort on remaining elements smaller than pivot
; 3. qsort on remaining elements greater than pivot
; 4. Create new list by append smallers onto pivot onto greaters
(define (qsort xs)
  (cond
    [(null? xs)           xs]
    [(eqv? (length xs) 1) xs]
    [else
     (append
      (qsort (smallers (car xs) (cdr xs)))
      (list (car xs))
      (qsort (greaters (car xs) (cdr xs))))]))
