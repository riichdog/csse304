#lang racket

(provide intersection subset? relation? domain reflexive? multi-set? ms-size last all-but-last)

(define intersection
  (lambda (a b)
    (cond [ (null? b) '()]
          [ (member (car b) a) (cons (car b) (intersection a (cdr b)))]
          [ else (intersection a (cdr b))])))

(define subset?
  (lambda (a b)
    (cond [ (null? a) #t]
          [ (member (car a) b) (subset? (cdr a) b)]
          [else #f])))

(define my-set?
  (lambda (a)
    ( cond [ (null? a) #t]
           [ (member (car a) (cdr a)) #f]
           [ else (my-set? (cdr a) ) ]
           )))

(define relation?
  (lambda (a)
     ( cond [ (null? a) #t]
            [ (not(list? a)) #f]
            [ (not(list? (car a))) #f]
            [ (not ( = (length (car a)) 2)) #f] 
            [ (member (car a) (cdr a)) #f ]
            [ else (relation? (cdr a) ) ]
           )))

(define domain
  (lambda (a)
    ( dom a '() )))

(define dom
  (lambda (a curr)
    (cond [ (null? a) curr]
          [ (member (caar a) curr) ( dom (cdr a) curr)]
          [ else (dom (cdr a) (cons (caar a) curr))]
          )))

(define range
  (lambda (a)
    ( ran a '() )))

(define ran
  (lambda (a curr)
    (cond [ (null? a) curr]
          [ (member (cadar a) curr) ( ran (cdr a) curr)]
          [ else (ran (cdr a) (cons (cadar a) curr))]
          )))


(define reflexive?
  (lambda (a)
    (let ((x (domain a)) (y (range a)))
     (cond [ (= (length x) (length y)) (reflex a x y)]
           [else #f]))))

(define reflex
  (lambda (a x y)
    (cond [(null? x) #t]
          [(and (member (list (car x) (car x)) a) (member (list (car y) (car y)) a)) (reflex a (cdr x) (cdr y)) ]
          [ else #f])))

(define (is-variable-a-letter? x)
  (let ((var (string->list (symbol->string x))))
    (and (= (length var) 1)
         (char-alphabetic? (car var)))))

(define multi-set?
  (lambda (a)
    (cond [ (null? a) #t]
          [ (not (list? a)) #f]
          [ (not (relation? a)) #f]
          [ (ms a '()) #f]
          [ (and (number? (cadar a)) (symbol? (caar a))) (multi-set? (cdr a))]
          [ else #f])))

(define ms
  (lambda (a curr)
    (cond [ (null? a) #f]
          [ (member (caar a) curr) #t]
          [ else (ms (cdr a) (cons (caar a) curr))]
          )))


(define ms-size
  (lambda (a)
    (apply + (map cadr a))))

(define last
  (lambda (a)
   ( lastly (cdr a) (car a))))

(define lastly
  (lambda (a l)
    (cond [(null? a) l]
          [else (lastly (cdr a) (car a))])))

(define all-but-last
  (lambda (a)
    (cond [(= (length a) 1) '()]
          [ else (cons(car a)(all-but-last (cdr a)) )])))

(define allhelper
  (lambda (a lol )
    (cond [(= (length a) 1) lol]      
          [else (allhelper (cdr a) (cons (car a) lol))])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
