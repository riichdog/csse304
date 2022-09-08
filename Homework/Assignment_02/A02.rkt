#lang racket

(provide choose sum-of-squares range my-set? union more-positives? nearest-pair)

(define fact
  (lambda (a)
    (cond [ (= a 0) 1]
          [ else (* a (fact (- a 1)))] )))

(define choose
  (lambda (a b)
    ( / (fact a) ( * (fact b) (fact (- a b ) )))))

(define sum-of-squares
  (lambda (a)
    (cond [(null? a) 0]
          [else (+ (* (car a) (car a)) (sum-of-squares (cdr a)))])))
(define range
  (lambda (a b)
    (cond [ (>= a b) '() ]
          [ else (cons a (range (+ a 1) b))]
          )))

(define my-set?
  (lambda (a)
    ( cond [ (null? a) #t]
           [ (member (car a) (cdr a)) #f]
           [ else (my-set? (cdr a) ) ]
           )))

(define union
  (lambda (a b)
    (cond [ (null? b) a]
          [ (member (car b) a) (union a (cdr b))]
          [else (union (append a (list(car b))) (cdr b))]
          )))
    
(define more-positives?
  (lambda (lon)
    (> (less-postives? lon 0) 0)))

(define less-postives?
  (lambda (lon acc)
          (cond [ (null? lon) acc ]
                [ (> (car lon) 0) (less-postives? (cdr lon) (+ acc 1))]
                [ else (less-postives? (cdr lon) (- acc 1) )])))



(define nearest-pair
  (lambda (lon)
    (let ((on (sort lon <)))
    ( near (cdr on) (cons (car on) (cadr on))))))

(define near
  ( lambda (lon b)
     ( cond [ (null? (cdr lon )) (cons (car b) (cdr b))]
            [ (< (- (cdr b)(car b)) (- (cadr lon) (car lon) )) (near (cdr lon) b)]
            [else (near (cdr lon) (cons(car lon) (cadr lon)) )]
            )))
           

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
