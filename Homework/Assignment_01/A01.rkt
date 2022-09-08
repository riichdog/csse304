#lang racket

(provide interval-contains? interval-intersects? interval-union my-first my-second my-third make-vec-from-points dot-product vector-magnitude distance)

(define interval-contains?
  (lambda (a b)
     (and (<= b (cadr a)) (>= b (car a)))))

(define interval-intersects?
  (lambda (a b)
    (cond [ (= (car a) (car b)) #t]
          [ (> (car a) (car b)) (>= (cadr b) (car a))]
          [ else (<= (car b) (cadr a) )])))
(define interval-union
  (lambda (a b)
   (cond [ (and (interval-contains? a (cadr b)) (interval-contains? a (car b) )) (list (list (car a) (cadr a))) ] 
         [ (and (interval-contains? a (car b)) (not (interval-contains? a (cadr b)))) (list (list (car a) (cadr b)))  ]
         [ (and (interval-contains? a (cadr b)) (not (interval-contains? a (car b))))  (list (list (car b) (cadr a))) ]
         [ else (list a b)])))



(define my-first
  (lambda (a)
    (car a)))

(define my-second
  (lambda (a)
    ( cadr a)))

(define my-third
  (lambda (a)
    ( caddr a)))

(define make-vec-from-points
  (lambda (a b)
    ( list (- (my-first b) (my-first a)) (- (my-second b) (my-second a)) (- (my-third b) (my-third a)))))

(define dot-product
  (lambda (a b)
   (+ (* (my-first b) (my-first a)) (* (my-second b) (my-second a)) (* (my-third b) (my-third a)) )))

(define vector-magnitude
  (lambda (a)
    (sqrt (+ (* (my-first a) (my-first a)) (* (my-second a) (my-second a)) (* (my-third a) (my-third a))) )))



(define distance
  (lambda (a b)
    ( sqrt (+ (* (-( my-first a) (my-first b)) (-( my-first a) (my-first b)))
              (* (-( my-second a) (my-second b)) (-( my-second a) (my-second b)))
              (* (-( my-third a) (my-third b)) (-( my-third a) (my-third b)))))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
