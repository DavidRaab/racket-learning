#lang racket

(define (assov search xss)
  (cond
    [(null? xss)             #f]
    [(not (list? (car xss))) #f]
    [(equal? search (caar xss))
     (cadar xss)]
    [else
     (assov search (cdr xss))]))

; Small Evaluator of mathematical expressions
(define operator-dispatch
  (list
   [list '+   (lambda (x y) (+ x y))]
   [list '-   (lambda (x y) (- x y))]
   [list '*   (lambda (x y) (* x y))]
   [list '/   (lambda (x y) (/ x y))]
   [list '**  (lambda (x y) (expt x y))]
   [list 'min (lambda (x y) (min x y))]
   [list 'e   (lambda (x y) (* x (expt 10 y)))]
   ))

(define (is-operator? x)
  (if (member x (map car operator-dispatch)) #t #f))

; Checks if expression is valid
(define (numbered? lst)
  (cond
    [(number? lst)     #t]
    [(not (list? lst)) #f]
    [(null? lst)       #f]
    [(not (eqv? (length lst) 3))
     #f]
    [(and
      (or (number?  (car lst))   (numbered? (car lst)))
      (is-operator? (cadr lst))
      (or (number?  (caddr lst)) (numbered? (caddr lst))))
     #t]
    [else #f]))

; Evaluates expression
(define (eval-numbered exp)
  (cond
    [(not (numbered? exp)) (error 'numbered? (format "Not valid: ~a" exp))]
    [(number? exp)         exp]
    [else
     ((assov (cadr exp) operator-dispatch)
      (eval-numbered (car exp))
      (eval-numbered (caddr exp)))]
    ))

;; Syntax Transformers / Macros
; Full bloated example
(define-syntax foo
  (lambda (expr)
    (syntax "I am foo")))

; define-syntax can be sshortened like define
; shortcut for (syntax) is #'
(define-syntax (show-me stx)
  (print stx)
  #'(void))


; calc is a shortcut for eval-numbered without explicit quoting
(define-syntax calc
  (lambda (syntax)
    (define expr (syntax->list syntax))
    (cond
      [(not (eqv? (length expr) 2))
       (apply raise-arity-error 'calc 1 (cdr (syntax->datum syntax)))]
      [else
       (datum->syntax
        syntax
        `(eval-numbered ',(cadr expr)))]
      )))

(define xs '(1 2 3))

(define-syntax (our-if-v2 stx)
  (define xs (syntax->list stx))
  (datum->syntax
   stx
   `(cond [,(cadr xs) ,(caddr xs)]
          [else ,(cadddr xs)])
   ))






