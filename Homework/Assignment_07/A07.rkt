#lang racket

(provide vector-append-list group-by-two group-by-n bt-leaf-sum bt-inorder-list bt-max bt-max-interior slist-map slist-reverse slist-paren-count slist-depth slist-symbols-at-depth path-to make-c...r)

(define vector-append-list
  (lambda (vec ls)
    (let ([new-vector (make-vector (+ (vector-length vec) (length ls)))])
      (copy-from-vector new-vector vec 0)
      (copy-from-list new-vector ls  (vector-length vec) (+ (vector-length vec) (length ls)) ) new-vector)))

(define copy-from-vector
  (lambda (new vec len)
    (cond [(= len  (sub1 (vector-length vec))) (vector-set! new len (vector-ref vec len))]
          [(= 0 (vector-length vec)) ]
          [else (begin (vector-set! new len (vector-ref vec len)) (copy-from-vector new vec (add1 len)))])))

(define copy-from-list
  (lambda (new lst len stop )
    (cond [(= len (sub1 stop)) (vector-set! new len (car lst))]
          [(null? lst) ]
          [else ( begin (vector-set! new len (car lst)) (copy-from-list new (cdr lst) (add1 len) stop))])))

               
(define group-by-two
  (lambda (a)
    (group a 2)))
(define group
  (lambda (lst stop)
    (cond [(null? lst) '()]
          [else (cons (groupnum lst stop) (group (rmvnum lst stop) stop))])))
(define groupnum
  (lambda (lst num)
    (cond [(= num 0) '()]
          [(null? lst) '()]
          [else (cons (car lst) (groupnum (cdr lst) (- num 1)))])))
(define rmvnum
  (lambda (lst num)
    (cond [(= num 0) lst]
          [(null? lst) '()]
          [else (rmvnum (cdr lst) (- num 1))])))

(define group-by-n
  (lambda (a b)
    (group a b)))

(define bt-right
  (lambda (x)
    (caddr x)))
(define bt-left
  (lambda (x)
    (cadr x)))
(define bt-leaf-sum
  (lambda (a)
    (cond [(number? a) a]
          [else (+ (bt-leaf-sum (bt-left a)) (bt-leaf-sum (bt-right a)))])))

(define bt-inorder-list
  (lambda (a)
    (cond [(number? a) '()]
          [else (append (bt-inorder-list (bt-left a)) (cons (car a) (bt-inorder-list (bt-right a))))])))

(define bt-max
  (lambda (a)
     (cond [(number? a) a]
           [else ( max (bt-max (bt-left a)) (bt-max (bt-right a)))])))

(define bt-max-interior
  (lambda (a)
    (nyi)))


(define slist-map
  (lambda (a b)
    (cond [(null? b) b]
          [(list? b) (cons (slist-map a (car b)) (slist-map a (cdr b)))]
          [else (a b)])))

(define slist-reverse
  (lambda (a)
   (reversehelp (reverse a))))
(define reversehelp
  (lambda (lst)
   (cond  [(null? lst) '()]
          [(list? (car lst)) (cons (reverse (car lst)) (reversehelp  (cdr lst)))]
          [else (cons (car lst) (reversehelp (cdr lst)))])))

(define slist-paren-count
  (lambda (a)
    (cond [(null? a) 2]
          [(list? a) (+ (slist-paren-count (cdr a)) (slist-paren-count (car a)))]
          [else 0])))

(define slist-depth
  (lambda (a)
       (cond [(null? a) 1]
          [(list? (car a))  (max (+ 1 (slist-depth (car a))) (slist-depth (cdr a)))]
          [else (slist-depth (cdr a))])))

(define slist-symbols-at-depth
  (lambda (a b)
    (cond [(null? a) '()]
          [(list? (car a) )])))

(define path-to
  (lambda (a b)
    (nyi)))

(define make-c...r
  (lambda (a)
    (nyi)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
