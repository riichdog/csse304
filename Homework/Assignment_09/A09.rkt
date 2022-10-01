#lang racket

(provide sn-list-recur sn-list-sum sn-list-map sn-list-paren-count sn-list-reverse sn-list-occur sn-list-depth bt-recur bt-sum bt-inorder)

(define sn-list-recur
  (lambda (base listproc symproc)
    (letrec
        ([helper
          (lambda (lst)
            (cond [(null? lst) base]
                  [(list? (car lst)) (listproc (helper (car lst)) (helper (cdr lst)))]
                  [else (listproc (symproc (car lst)) (helper(cdr lst)))]))])
      helper)))

(define sn-list-sum
  (lambda (a)
    ((sn-list-recur 0 + +) a)))

(define sn-list-map
  (lambda (a b)
    ((sn-list-recur '() cons a) b)))


(define sn-list-paren-count
  (lambda (a)
    ((sn-list-recur 2 + (lambda (x) 0) ) a)))

(define sn-list-reverse
  (lambda (a)
    ((sn-list-recur '() (lambda (x y) (append y (cons x '()))) (lambda (x) x) ) a)))

(define sn-list-occur
  (lambda (a b)
    ((sn-list-recur 0 + (lambda (x) (if (eq? x a) 1 0)) ) b)))

(define sn-list-depth
  (lambda (a)
     ((sn-list-recur 1 (lambda (x y) (max (add1 x) y)) (lambda (x) 0)) a)))

(define bt-recur
  (lambda (base btproc numproc)
    (letrec
        ([helper
          (lambda (bt)
            (cond [(null? bt) base]
                  [(list? bt) (btproc (first bt) (helper (second bt)) (helper (third bt)))]
                  [else (numproc bt)]))])
      helper)))

(define bt-sum
  (lambda (a)
   ((bt-recur 0 (lambda (x y z) (+ y z)) (lambda (x) x)) a)))

(define bt-inorder
  (lambda (a)
    ((bt-recur '() (lambda (x y z) (append y (cons x '()) z)) (lambda (x) '()) ) a)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
