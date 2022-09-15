#lang racket

(provide curry2 curried-compose compose make-list-c reverse-it map-by-position empty-BST empty-BST? BST-insert BST-inorder BST? BST-element BST-left BST-right BST-insert-nodes BST-contains? BST-height let->application let*->let qsort sort-list-of-symbols)

(define curry2
  (lambda (a)
   (lambda ($)
     (lambda (s)
       (a $ s)))))

(define curried-compose
  (lambda (a)
    (lambda (s)
      (lambda (x)
        (a (s x) )))))

(define compose
  (lambda a
    (lambda (q)
      (cond [(null? a) q]
            [else ((car a) ((apply compose (cdr a)) q))]))))


(define make-list-c
  (lambda (a)
    (lambda (b)
      (makem a b))))
(define makem
  (lambda (num el)
    (cond [(= num 0) '()]
          [else (cons el (makem (- num 1) el))])))

(define reverse-it
  (lambda (a)
    (if (null? a)
        '()
        (let rev ( [lst a]
                   [reversed '()])
          (cond [(null? lst) reversed]
                [else (rev (cdr lst) (cons (car lst) reversed))])))))

(define map-by-position
  (lambda (a b)
    (map (lambda (s n) (s n)) a b)))

(define empty-BST
  (lambda ()
    '()))

(define empty-BST?
  (lambda (a)
    (null? a)))

(define bst
  (lambda (node left right)
    (list node left right)))

(define BST-insert
  (lambda (a b)
    (cond [(empty-BST? b) (bst a (empty-BST) (empty-BST))]
          [(< a (BST-element b)) (bst (BST-element b) (BST-insert  a (BST-left b)) (BST-right b))]
          [(> a (BST-element b)) (bst (BST-element b) (BST-left b)  (BST-insert a (BST-right b)))]
          [else b ])))

(define BST-inorder
  (lambda (a)
    (cond [(empty-BST? a) (empty-BST)]
          [else (append (BST-inorder (BST-left a)) (list (BST-element a)) (BST-inorder (BST-right a)))])))

(define BST?
  (lambda (a)
    (cond [(not (list? a)) #f]
          [(empty-BST? a) #t]
          [(not (number? (BST-element a))) #f]
          [else #f])))
          
(define BST-element
  (lambda (a)
    (cond [(empty-BST? a) (empty-BST)]
          [else ( car a)])))

(define BST-left
  (lambda (a)
     (cond [(empty-BST? a) (empty-BST)]
           [else (cadr a)])))

(define BST-right
  (lambda (a)
    (cond [(empty-BST? a) (empty-BST)]
           [else (caddr a)])))

(define BST-insert-nodes
  (lambda (a b)
    (cond [(null? b) a ]
          [else (BST-insert-nodes (BST-insert (car b) a) (cdr b)) ])))

(define BST-contains?
  (lambda (a b)
    (cond [(empty-BST? a) #f]
          [(= b (BST-element a)) #t]
          [(< b (BST-element a)) (BST-contains? (BST-left a) b)]
          [else (BST-contains? (BST-right a) b)])))

(define BST-height
  (lambda (a)
    (cond [(empty-BST? a) -1]
          [else (+ 1 (max (BST-height (BST-right a)) (BST-height (BST-left a))))])))

(define let->application
  (lambda (a)
    (append (list (list 'lambda (map first (second a)) (third a)))  (map second (second a)))))

(define let*->let
  (lambda (a)
   (let*tel (second a) (third a))))
(define let*tel
  (lambda (lst thrd)
    (cond [(null? (cdr lst)) (list 'let (list (car lst)) thrd)]
          [else (list 'let (list (car lst)) (let*tel (cdr lst) thrd))])))

(define qsort
  (lambda (a b)
    (cond [(null? b) '()]
          [else (append (qsort a (filter-in (lambda (x)
                                              (a x (car b)))
                                              (cdr b)))
                               (list (car b))
                               (qsort a (filter-out (lambda (x)
                                                      (a x (car b)))
                                                      (cdr b))))])))

(define filter-in
  (lambda (a b)
    (cond [(null? b) '()]
          [(a (car b)) (cons (car b) (filter-in a (cdr b)))]
          [else (filter-in a (cdr b))])))
(define filter-out
  (lambda (a b)
     (cond[(null? b) '()]
          [(a (car b)) (filter-out a (cdr b))]
          [else (cons (car b) (filter-out a (cdr b)))])))

(define sort-list-of-symbols
  (lambda (a)
    (sort a symbol<?)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
