#lang racket


(define (partition pred lat)
  (define (loop lat cont)
    (cond
      [(null? lat)
       (cont null null)]
      [(pred (car lat))
       (loop
        (cdr lat)
        (lambda (newlat seen)
          (cont 
           (cons (car lat) newlat)
           seen)))
       ]
      [else
       (loop
        (cdr lat)
        (lambda (newlat seen)
          (cont
           newlat
           (cons (car lat) seen))))
       ]))
  (loop lat (lambda (x y) (list x y))))


(define (part pred lat)
  (define (loop lat wanted unwanted)
    (cond
      [(null? lat)
       (list wanted unwanted)]
      [(pred (car lat))
       (loop
        (cdr lat)
        (cons (car lat) wanted)
        unwanted)]
      [else
       (loop
        (cdr lat)
        wanted
        (cons (car lat) unwanted))]
      ))
  (loop (reverse lat) null null))



(define l1 '(1 2 3 4 5 6 7 8 9 10))

(let ([part (partition even? l1)])
  (list
   (length (first  part))
   (length (second part))))

(part even? l1)