#lang racket

(provide matrix-ref matrix? matrix-transpose filter-in invert pascal-triangle)

(define matrix-ref
  (lambda (a b c)
    (list-ref (list-ref a b) c)))

(define matrix?
  (lambda (a)
    (if (list? a)
         (if (member #f (map (lambda (x) (cond [(null? x) #f]
                           [(and (list? x) (= (length (first a)) (length x)) (not(member #t (map symbol? x)))) #t]
                           [else #f])) a))
             #f
             #t)             
         #f)))
  

(define matrix-transpose
  (lambda (a)
    (mat-row a 0 (length (car a)))))

(define mat-row
  (lambda (lst count limit)
          (cond [(= count limit) '()]
                [else (cons (mat-col lst 0 count (length lst)) (mat-row lst (+ count 1) limit))])))
(define mat-col
  (lambda (lst b c limit)
    (cond [ (= b limit) '()]
          [ else (cons (matrix-ref lst b c) (mat-col lst (+ b 1) c limit))])))

(define filter-in
  (lambda (a b)
    (cond [(null? b) '()]
          [(a (car b)) (cons (car b) (filter-in a (cdr b)))]
          [else (filter-in a (cdr b))])))

(define invert
  (lambda (a)
    (map (lambda (x) (list (second x) (first x))) a)))

(define pascal-triangle
  (lambda (a)
    (cond [(< a 0) '()]
          [else (cons (pascy a 0) (pascal-triangle (- a 1)))])))
(define pascy
  (lambda (n k)
    ( cond [(< n k) '()]
           [else (cons (choose n k) (pascy n (+ k 1)))])))



(define fact
  (lambda (a)
    (cond [ (= a 0) 1]
          [ else (* a (fact (- a 1)))] )))

(define choose
  (lambda (a b)
    ( / (fact a) ( * (fact b) (fact (- a b ) )))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
