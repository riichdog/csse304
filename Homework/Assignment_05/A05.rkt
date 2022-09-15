#lang racket

(provide minimize-interval-list exists? product replace remove-last)

(define minimize-interval-list
  (lambda (a)
    (mini2 (sort a (lambda (x y)
              (if (= (car x) (car y))
                     (> (second y) (second x))
                     (> (first y) (first x))) )) )


    ))
(define mini
  (lambda ( lst ans)
    (cond [(null? lst) ans]
          [(null? (cdr lst)) (append ans (car lst))]
          [(and (>= (second (first lst)) (first (second lst))) (<= (second (first lst)) (second (second lst)))) (mini (cons (list (first (first lst)) (second (second lst))) (cddr lst)) ans)]
          [(and (>= (second (first lst)) (first (second lst))) (> (second (first lst)) (second (second lst))))  (mini (cons (list (first (first lst)) (second (first lst))) (cddr lst)) ans)]
          [else   (mini (cdr lst) (list ans (car lst)))])))


(define mini2
  (lambda ( lst )
    (cond [(null? lst) '()]
          [(null? (cdr lst))  (list (car lst)) ]
          [(and (>= (second (first lst)) (first (second lst))) (<= (second (first lst)) (second (second lst)))) (mini2 (cons (list (first (first lst)) (second (second lst))) (cddr lst)))]
          [(and (>= (second (first lst)) (first (second lst))) (> (second (first lst)) (second (second lst))))  (mini2 (cons (list (first (first lst)) (second (first lst))) (cddr lst)))]
          [else   (cons (car lst) (mini2 (cdr lst)))])))
        

(define exists?
  (lambda (a b)
    (cond [(member #t (map a b)) #t]
          [else #f])))

(define product
  (lambda (a b)
    (cond [(null? a) '()]
          [else (append (prod (car a) b) (product (cdr a) b))])))

(define prod
  (lambda (el b)
    (cond [(null? b) '()]
          [else (cons (list el (car b)) (prod el (cdr b)))]))) 

(define replace
  (lambda (a b c)
    (cond [(null? c) '()]
          [(equal? a (car c)) (cons b (replace a b (cdr c)))]
          [else (cons (car c) (replace a b (cdr c)))])))

(define remove-last
  (lambda (a b)
    (count-occurence a b b 0)))
(define count-occurence
  (lambda (a lst lol count)
    (cond [(null? lst) (rmv a lol count 1)]
          [(equal? a (car lst)) (count-occurence a (cdr lst) lol (+ count 1))]
          [else (count-occurence a (cdr lst) lol count)])))
(define rmv
  (lambda (a lol count inc)
    (cond [(null? lol) '()]
          [(equal? (list a) lol) '()]
          [(and (equal? (car lol) a) (= count inc)) (cons (cadr lol) (rmv a (cddr lol) count inc))]
          [(equal? (car lol) a) (cons (car lol) (rmv a (cdr lol) count (+ inc 1)))]
          [else (cons (car lol) (rmv a (cdr lol) count inc))])))



;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
