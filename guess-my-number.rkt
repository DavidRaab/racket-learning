#lang racket

(define low 0)
(define high 100)
(define current_guess 0)

(define (guess)
  (quotient (+ low high) 2))

(define (lower)
  (set! high (max low (guess)))
  (guess))

(define (higher)
  (set! low (min high (guess)))
  (guess))

(define (reset)
  (set! low 0)
  (set! high 100)
  (guess))