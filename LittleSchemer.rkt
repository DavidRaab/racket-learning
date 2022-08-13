#lang racket/base

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (xs)
    (cond
      [(null? xs)       #t]
      [(list? (car xs)) #f]
      [else             (lat? (cdr xs))])))

(define member?
  (lambda (a l)
    (cond
      [(null? l)       #f]
      [(eq? a (car l)) #t]
      [(list? (car l)) (or (member? a (car l)) (member? a (cdr l)))]
      [else            (member? a (cdr l))])))

(define rember
  (lambda (a lat)
    (cond
      [(null? lat)       null]
      [(eq? (car lat) a) (cdr lat)]
      [else              (cons (car lat) (rember a (cdr lat)))]
      )))

(define multirember
  (lambda (a xs)
    (cond
      [(null? xs)       null]
      [(eq? (car xs) a) (multirember a (cdr xs))]
      [else             (cons (car xs) (multirember a (cdr xs)))])))

(define (reverse x)
  (define (rev old new)
    (cond
      [(null? old) new]
      [else        (rev (cdr old) (cons (car old) new))]))
  (rev x null))

(define (remberLast search xs)
  (reverse (rember search (reverse xs))))

(define append
  (lambda (x xs)
    (cond
      [(null? xs) (cons x null)]
      [else (cons (car xs) (append x (cdr xs)))])))

(define (first xs)  (car xs))
(define (second xs) (car (cdr xs)))
(define (third xs)  (car (cdr (cdr xs))))

(define index
  (lambda (n xs)
    (cond
      [(null? xs) #f]
      [(<= n 0)   (car xs)]
      [else       (index (- n 1) (cdr xs))])))

(define (pick n xs) (index (sub1 n) xs))

(define has-index?
  (lambda (n xs)
    (to-bool (index n xs))))


(define (to-bool x)     (not (not x)))
(define (hasFirst? xs)  (has-index? 0 xs))
(define (hasSecond? xs) (has-index? 1 xs))
(define (hasThird? xs)  (has-index? 2 xs))


(define firsts
  (lambda (xs)
    (cond
      [(null? xs)           null]
      [(not (hasFirst? (car xs))) (firsts (cdr xs))]
      [else                 (cons (first (first xs)) (firsts (cdr xs)))])))

(define seconds
  (lambda (xs)
    (cond
      [(null? xs)                  null]
      [(not (hasSecond? (car xs))) (seconds (cdr xs))]
      [else                        (cons (second (first xs)) (seconds (cdr xs)))])))

(define thirds
  (lambda (xs)
    (cond
      [(null? xs)                 null]
      [(not (hasThird? (car xs))) (thirds (cdr xs))]
      [else                       (cons (third (first xs)) (thirds (cdr xs)))])))

(define forths
  (lambda (xs)
    (cond
      [(null? xs)                    null]
      [(not (has-index? 3 (car xs))) (forths (cdr xs))]
      [else                          (cons (pick 4 (car xs)) (forths (cdr xs)))])))


(define insertR
  (lambda (new old xs)
    (cond
      [(null? xs)         null]
      [(eq? (car xs) old) (cons old (cons new (cdr xs)))]
      [else               (cons (car xs) (insertR new old (cdr xs)))])))

(define multiInsertR
  (lambda (new search xs)
    (cond
      [(null? xs)
       null]
      [(eq? (car xs) search)
       (cons (car xs) (cons new (multiInsertR new search (cdr xs))))]
      [else
       (cons (car xs) (multiInsertR new search (cdr xs)))])))

(define insertL
  (lambda (new old xs)
    (cond
      [(null? xs)         null]
      [(eq? (car xs) old) (cons new xs)]
      [else               (cons (car xs) (insertL new old (cdr xs)))])))

(define multiInsertL
  (lambda (new search xs)
    (cond
      [(null? xs)
       null]
      [(eq? (car xs) search)
       (cons new (cons search (multiInsertL new search (cdr xs))))]
      [else
       (cons (car xs) (multiInsertL new search (cdr xs)))])))

(define enclose
  (lambda (search left right xs)
    (multiInsertL left search (multiInsertR right search xs))))

(define subst
  (lambda (new old xs)
    (cond
      [(null? xs)         null]
      [(eq? (car xs) old) (cons new (cdr xs))]
      [else               (cons (car xs) (subst new old (cdr xs)))])))

(define multisubst
  (lambda (new search xs)
    (cond
      [(null? xs) null]
      [(eq? (car xs) search)
       (cons new      (multisubst new search (cdr xs)))]
      [else
       (cons (car xs) (multisubst new search (cdr xs)))])))

(define subst2
  (lambda (new o1 o2 xs)
    (cond
      [(null? xs)        null]
      [(eq? (car xs) o1) (cons new (cdr xs))]
      [(eq? (car xs) o2) (cons new (cdr xs))]
      [else              (cons (car xs) (subst2 new o1 o2 (cdr xs)))])))

(define length
  (lambda (xs)
    (cond
      [(null? xs) 0]
      [else       (+ 1 (length (cdr xs)))])))

; Utility Functions

(define double
  (lambda (x)
    (* x 2)))

(define square
  (lambda (x)
    (* x x)))

;(define map
;  (lambda (f xs)
;    (cond
;      [(null? xs) null]
;      [else       (cons (f (car xs)) (map f (cdr xs)))])))



; Tuple Functions
; tup   = Tuple
; Tuple = List of Numbers

(define tup?
  (lambda (xs)
    (cond
      [(null? xs)               #t]
      [(not (number? (car xs))) #f]
      [else                     (tup? (cdr xs))])))

(define addtup
  (lambda (tup)
    (cond
      [(null? tup) 0]
      [else
       (+ (car tup) (addtup (cdr tup)))])))

(define tup+
  (lambda (t1 t2)
    (cond
      [(and (null? t1) (null? t2)) null]
      [(null? t1)                  t2]
      [(null? t2)                  t1]
      [else
       (cons
        (+ (car t1) (car t2))
        (tup+ (cdr t1) (cdr t2)))])))

(define rempick
  (lambda (n xs)
    (cond
      [(null? xs) null]
      [(= n 1)    (cdr xs)]
      [else       (cons (car xs) (rempick (sub1 n) (cdr xs)))])))

(define no-nums
  (lambda (xs)
    (cond
      [(null? xs)
       null]
      [(number? (car xs))
       (no-nums (cdr xs))]
      [else
       (cons (car xs) (no-nums (cdr xs)))])))

(define all-nums
  (lambda (xs)
    (cond
      [(null? xs)
       null]
      [(number? (car xs))
       (cons (car xs) (all-nums (cdr xs)))]
      [else
       (all-nums (cdr xs))])))

(define occur
  (lambda (a xs)
    (cond
      [(null? xs)        0]
      [(eqv? (car xs) a) (add1 (occur a (cdr xs)))]
      [else              (occur a (cdr xs))])))

;(define (concat xs ys)
;  (cond
;    [(null? xs)
;     ys]
;    [else
;     (cons (car xs) (concat (cdr xs) ys))]))

(define (concat xs ys)
  (define (loop xs acc)
    (cond
      [(null? xs)
       acc]
      [else
       (loop (cdr xs) (cons (car xs) acc))]))
  (loop (rev xs) ys))

(define (flatten xs)
  (cond
    [(null? xs)
     null]
    [(list? (car xs))
     (concat (flatten (car xs)) (flatten (cdr xs)))]
    [else
     (cons (car xs) (flatten (cdr xs)))]))

;(define (rev xs)
;  (define (irev xs ys)
;    (cond
;      [(null? xs)
;       ys]
;      [else
;       (irev (cdr xs) (cons (car xs) ys))]))
;  (irev xs null))


;(define (has-exactly-one xs)
;  (cond
;    [(null? xs)       #f]
;    [(null? (cdr xs)) #t]
;    [else             #f]))
;
;(define (reduce f xs)
;  (cond
;    [(null? xs)           (error 'reduce "Cannot work on empty list")]
;    [(has-exactly-one xs) (car xs)]
;    [else                 (reduce f (cons (f (car xs) (cadr xs)) (cddr xs)))]))

(define (fold f acc xs)
  (cond
    [(null? xs) acc]
    [else       (fold f (f acc (car xs)) (cdr xs))]))

(define (reduce f xs)
  (fold f (car xs) (cdr xs)))

(define (rev xs)
  (fold (lambda (acc x) (cons x acc)) null xs))

(define (foldBack f xs acc)
  (fold (lambda (acc x) (f x acc)) acc (rev xs)))

(define (map f xs)
  (foldBack (lambda (x acc) (cons (f x) acc)) xs null))

(define (f-mrember search xs)
  (foldBack
   (lambda (acc x)
     (cond
       [(equal? x search) acc]
       [else              (cons x acc)]))
   xs '()))


; fold-back implementation - not tail-recursive
(define (fold-back f xs acc)
  (cond
    [(null? xs) acc]
    [else
     (f (car xs) (fold-back f (cdr xs) acc))]))

; occur implemented with fold-back
; tail-recursive: false  --   (because fold-back is not)
(define (foccur search xs)
  (fold-back
   (lambda (x acc)
     (cond
       [(equal? x search) (+ 1 acc)]
       [else             acc]))
   xs 0
   ))

; occur implemented with fold
; tail-reurive: true
(define (goccur search xs)
  (fold
   (lambda (acc x)
     (cond
       [(equal? x search) (+ 1 acc)]
       [else              acc]))
   0 xs))

; occur implemented with inner function
; tail-recursive: true
(define (toccur search xs)
  (define (loop l acc)
    (cond
      [(null? l)
       acc]
      [(equal? (car l) search)
       (loop (cdr l) (+ 1 acc))]
      [else
       (loop (cdr l) acc)]))
  (loop xs 0))

;(define (reduce f init xs)
;  (if (null? xs)
;      init
;      (f (car xs) (reduce f init (cdr xs)))))

; ----------------------
; List of List Functions

(define pa2
  (lambda (f)
    (lambda (x)
      (lambda (y)
        (f x y)))))

(define (if-number f x)
  (cond
    [(number? x)
     (f x)]
    [else
     x]))

(define (map* f xs)
  (cond
    [(null? xs)
     null]
    [(list? (car xs))
     (cons (map* f (car xs)) (map* f (cdr xs)))]
    [else
     (cons (f (car xs)) (map* f (cdr xs)))]))

(define (rember* a xs)
  (cond
    [(null? xs)
     null]
    [(list? (car xs))
     (cons (rember* a (car xs)) (rember* a (cdr xs)))]
    [(equal? (car xs) a)
     (rember* a (cdr xs))]
    [else
     (cons (car xs) (rember* a (cdr xs)))]))

(define (insertR* old new xs)
  (cond
    [(null? xs)
     null]
    [(list? (car xs))
     (cons (insertR* old new (car xs)) (insertR* old new (cdr xs)))]
    [(equal? (car xs) old)
     (list* old new (insertR* old new (cdr xs)))]
    [else
     (cons (car xs) (insertR* old new (cdr xs)))]))

;(define (occur* a xs)
;  (cond
;    [(null? xs)
;     0]
;    [(list? (car xs))
;     (+
;      (occur* a (car xs))
;      (occur* a (cdr xs)))]
;    [(equal? (car xs) a)
;     (+ 1 (occur* a (cdr xs)))]
;    [else
;     (occur* a (cdr xs))]))


(define (occur* a xs)
  (define (match-to-number x)
    (if (equal? x a) 1 0))
  (reduce + 0 (map match-to-number (flatten xs))))
                 

;(define (occur* a xs)
;  (addtup
;   (flatten
;    (map*
;     (lambda (x)
;       (cond
;         [(equal? x a) 1]
;         [else         0]))
;     xs))))

(define (subst* old new xs)
  (map*
   (lambda (x)
     (cond
       [(equal? x old) new]
       [else           x]))
   xs))


; Inserts 'new left to 'old
;(define (insertL* new old xs)
;  (cond
;    [(null? xs)            null]
;    [(list? (car xs))      (cons (insertL* new old (car xs)) (insertL* new old (cdr xs)))]
;    [(equal? (car xs) old) (cons new (cons old (insertL* new old (cdr xs))))]
;    [else                  (cons (car xs) (insertL* new old (cdr xs)))]))

; Checks if 'a exists in 'xs
(define (member* a xs)
  (cond
    [(null? xs)          #f]
    [(list? (car xs))    (or (member* a (car xs)) (member* a (cdr xs)))]
    [(equal? (car xs) a) #t]
    [else                (member* a (cdr xs))]))

(define (id x) x)

(define (mapReduce* fx fxs xs init)
  (cond
    [(null? xs)
     init]
    [(list? (car xs))
     (fxs
      (list*
       (mapReduce* fx fxs (car xs) init)
       (mapReduce* fx fxs (cdr xs) init)
       null))]
    [else
     (fxs
      (list*
       (fx (car xs))
       (mapReduce* fx fxs (cdr xs) init)
       null
       ))]))


;(define (fold-back f xs acc)
;  (cond
;    [(null? xs)
;     acc]
;    [else
;     (f (car xs) (fold-back f (cdr xs) acc))]))

; foldBack on arbitary list-of-lists
(define (foldBack* f xs acc)
  (cond
    [(null? xs)
     acc]
    [(list? (car xs))
     (f
      (foldBack* f (car xs) acc)
      (foldBack* f (cdr xs) acc))]
    [else
     (f (car xs) (foldBack* f (cdr xs) acc))]))


(define (insertL* new old xs)
  (foldBack*
   (lambda (x acc)
     (if (equal? x old)
         (list* new x acc)
         (cons x acc))))
  xs
  null)           


(define (collect* xs)
  (foldBack*
   (lambda (x acc)
     (cond
       [(list? x) (concat x acc)]
       [else      (cons x acc)]))
   xs null))

; Returns leftmost atom in any lol Depth
(define (leftmost xs)
  (cond
    [(null? xs)       #f]
    [(list? (car xs)) (leftmost (car xs))]
    [else             (car xs)]))

; Recursively checks if two lists are equal
(define (eqlist? xs ys)
  (cond
    [(and (null? xs) (null? ys))  #t]
    [(or  (null? xs) (null? ys))  #f]
    [(and (list? (car xs)) (list? (car ys)))
     (and
      (eqlist? (car xs) (car ys))
      (eqlist? (cdr xs) (cdr ys)))]
    [(eq?  (car xs)   (car ys))
     (eqlist? (cdr xs) (cdr ys))]
    [else  #f]))

; Greatest Common Divisior
(define (gcd m n)
  (cond
    [(eqv? n 0) m]
    [else
     (gcd n (modulo m n))]))

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

; Checks if a operator is supported
(define valid-operators (map car operator-dispatch))
(define (is-operator? x)
    (if (member? x valid-operators) #t #f))

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
(define (calc exp)
  (cond
    [(not (numbered? exp)) (error 'numbered? (format "Not valid: ~a" exp))]
    [(number? exp)         exp]
    [else
     ((second (assoc (second exp) operator-dispatch))
      (calc (first exp))
      (calc (third exp)))]
    ))
     
      
; Data
                                     
(define l1 '(a b c))
(define l2 '(w (x y) z))
(define l3 '(bacon lettuce and tomato))
(define l4 '(a b c d f g d h))
(define l5 '(1 2 3 4))
(define l6 '(4 3 2 1))
(define l7 '(6 tomaten 4 spaghetti 2 eier))


(define pizza '(tomato cheese brokolli zucchini cheese))
(define lol1
  '((a b)
    (c d)
    (e f)))

(define lol2
  '(((five plums) four)
    (eleven green oranges apples)
    ((no) more fucking)))

(define lol3
  '((3 coffe) cup ((2 tea) cup)
              (and (1 hick)) cup))

(define lol4
  '((how much (wood))
    could
    ((a (wood) chuck))
    (((chuck)))
    (if (a) ((wood chuck)))
    could chuck wood))

(define lol5
  '((potato) (chips ((with) fish) (chips))))

(define lol6
  '((1 (1 2) 2 3)
    (4 (5) (((2 3))) 6)))


