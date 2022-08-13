#lang racket/base

(define (scons x xs)
  (cond
    [(member x xs)
     xs]
    [else
     (cons x xs)]))

(define (set? xs)
  (cond
    [(null? xs)
     #t]
    [(member (car xs) (cdr xs))
     #f]
    [else
     (set? (cdr xs))]))

(define (make-set xs)
  (foldl scons null xs))

(define (subset? set1 set2)
  (cond
    [(null? set1)
     #t]
    [(member (car set1) set2)
     (subset? (cdr set1) set2)]
    [else
     #f]))

(define (eqset? set1 set2)
  (and
   (eq? (length set1) (length set2))
   (subset? set1 set2)))

(define (intersect set1 set2)
  (cond
    [(null? set1)
     null]
    [(member (car set1) set2)
     (cons (car set1) (intersect (cdr set1) set2))]
    [else
     (intersect (cdr set1) set2)]
    ))

(define (intersect? set1 set2)
  (if (null? (intersect set1 set2)) #f #t))

(define (union set1 set2)
  (if (null? set1)
      set2
      (scons (car set1) (union (cdr set1) set2))))

(define (union2 set1 set2)
  (cond
    [(not (null? set1))
     (scons (car set1) (union2 (cdr set1) set2))]
    [(not (null? set2))
     (scons (car set2) (union2 set1 (cdr set2)))]
    [else
     null]))

(define (union3 set1 set2)
  (make-set (append set1 set2)))

(define (intersectall xs)
  (foldl intersect (car xs) (cdr xs)))

; Builds a Pair
(define (tuple x y)
  (cons x (cons y null)))

(define (a-pair? x)
  (and
   (list? x) (eqv? (length x) 2)))

(define (fst xs) (car xs))
(define (snd xs) (car (cdr xs)))

; relation - checks if we have a set of pairs
(define (rel xs)
  (cond
    [(not (list? xs))
     #f]
    [(null? xs)
     #t]
    [(or
      (not (a-pair? (car xs)))
      (member (car xs) (cdr xs)))
     #f]
    [else
     (rel (cdr xs))]
    ))

; Checks if something is a function
; This means:
;   1. f must be a set of key/value pairs
;   2. Each key can only appear once
(define (fun? f)
  (cond
    [(not (list? f))
     #f]
    [(null? f)
     #t]
    [else
     (set? (map car f))]))

; Checks if sencond values are unique
(define (fullfun? f)
  (and
   (fun? f)
   (set? (map snd f))))

(define (revtuple x)
  (tuple (snd x) (fst x)))

(define (revrel rel)
  (cond
    [(null? rel)
     null]
    [else
     (scons
      (revtuple (car rel))
      (revrel   (cdr rel)))]
    ))


;----------------------
; Data / Tests
(define l1 (list 'Freddy 'Apple 'Foo 'Apple 'Bar))
(define sl1 (make-set l1))

l1
sl1
(scons 'Freddy sl1)

; Boolean
(printf "Boolean~n")
(subset? '(Apple Foo) l1)
(subset? '(Apple Foo Baz) l1)
(eqset? '(6 large chickens with wings) '(6 chickens with large wings))

;Intersect
(printf "Intersect~n")
(define set1 '(stewed tomatoes and macaroni))
(define set2 '(macaroni and cheese))

(intersect set1 set2)
(intersect? set1 set2)
(intersect '(foo bar) '(baz raz))
(intersect? '(foo bar) '(baz raz))

;Union
(printf "Union~n")

(union set1 set2)
(union '(foo foo foo) '(Bar Baz Bar))
(union2 '(foo foo foo) '(Bar Baz Bar))
(union3 '(foo foo roo) '(Bar Baz Bar Raz))

;Intersetall
(printf "Intersectall~n")
(define l-set '((a b c) (c a d e) (e f g h a b)))
(define l-set2 '((6 pears and)
                 (3 peaches and 6 peppers)
                 (8 pears and 6 plums)
                 (and 6 prunes with some apples)))

(intersectall l-set)
(intersectall l-set2)

; Pairs
(printf "Pairs~n")

(rel '((a b) (c d)))     ; #t
(rel '((a b) (c d) (e f) (g h))) ; #t
(rel '(a b c d))   ; #f
(rel '(a b))       ; #f
(rel '((a b c) (c d e)))    ; #f
(rel '((a b) (c d) (a b)))  ; #f

; Functions
(printf "Functions~n")
(define f1 '((8 3) (4 8) (7 6) (6 2) (3 4)))
(define f2 '((8 3) (4 3) (7 6) (6 2) (3 4)))

(fun? '((a b) (c d)))       ; #t
(fun? '((0 1) (1 2) (a b))) ; #t
(fun? '((a b) (a e)))     ; #f
(fun? '((a b) (a b)))     ; #f
(fun? '((a b c) (a b c))) ; #f
(revrel '((8 a) (pumpkin pie) (got sick)))

; Fullfun
(printf "Fullfun~n")

(fullfun? f1) ; #t
(fullfun? f2) ; #f

