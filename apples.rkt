#lang racket

; An Apple struct
(struct apple (color size))

; Sorted
(define (Sorted apples)
  (sort apples (lambda (a b) (< (apple-size a) (apple-size b)))))

; Biggest
(define (Biggest apples) (last apples))

; apple list
(define apples
  (list
   (apple 'red 1.0)
   (apple 'orange 3.0) 
   (apple 'blue 2.0)
   (apple 'yellow 4.0)))

; semi-declarative version
(define biggest (Biggest (Sorted apples)))

; new -- no-op function
;(define (new func . args)
;  (apply func args))
;
;(define biggest (new Biggest (new Sorted apples)))
;(display (apple-color biggest))