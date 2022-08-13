#lang racket/base

; A Basic Merge-Sort implementation of immutable list
;
; * Implemented for learning the algorithm and Recursion
; * Speed is not considered in this implementation
; * Tail-Recursion also not considered
;
; A Pair in this example is a list with exactly two elements
; A Pair has a left and a right element
; ----------------------------------------------------------

; take n elements from a list and discards the rests
; Example:
;   (take 4 '(a b c x y z))
;   (a b c x)
(define (take n xs)
  (cond
    [(eqv? n 0) null]
    [(eqv? n 1) (cons (car xs) null)]
    [else
     (cons (car xs) (take (sub1 n) (cdr xs)))]))

; skip n elements from a list returning the rest
; Example:
;   (skip 4 '(a b c x y z))
;   (y z)
(define (skip n xs)
  (cond
    [(eqv? n 0) xs]
    [else
     (skip (sub1 n) (cdr xs))]))

; A pair of the list splitted into two-halfes
; Example:
;   (split '(1 2 3 4 5))
;   ((1 2) (3 4 5))
(define (split xs)
  (list
   (take (quotient (length xs) 2) xs)
   (skip (quotient (length xs) 2) xs)))

; Left element of a pair
; Example:
;   (left '((1 2) (3 4 5)))
;   (1 2)
(define left  car)

; right element of a pair
; Example:
;   (right '((1 2) (3 4 5)))
;   (3 4 5)
(define right cadr)

; swaps the elements of a pair
; Example:
;   (swap '(a b))
;   (b a)
(define (swap xs)
  (list (right xs) (left xs)))


; merges two-sorted lists into one sorted list
; Implementation:
;   Both input lists must be sorted
;   Only checks the first element of both lists
;   Takes smallest element of xs/ys and recurse on rest
;
; Example:
;   (merge '(4 10 20) '(3 12 22))
;   (3 4 10 12 20 22)
(define (merge xs ys)
  (cond
    [(and (null? xs) (null? ys))
     null]
    [(null? xs)
     ys]
    [(null? ys)
     xs]
    [(< (car xs) (car ys))
     (cons (car xs) (merge (cdr xs) ys))]
    [else
     (cons (car ys) (merge xs (cdr ys)))]))


; Merge sort
; 1. Splits a list into two halfes
; 2. Sort every list recursive
; 3. Than merge every sorted list, into a new sorted list
(define (msort xs)
  (cond
    [(null? xs)
     null]
    [(eqv? (length xs) 1)
     xs]
    [(eqv? (length xs) 2)
     (if (< (car xs) (cadr xs))
         xs
         (swap xs))]
    [else
     (merge
      (msort (left  (split xs)))
      (msort (right (split xs))))]))

