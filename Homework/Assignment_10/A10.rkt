#lang racket

(provide free-vars bound-vars lexical-address un-lexical-address convert-multip-calls convert-multip-lambdas convert-ifs)

(define free-vars
  (lambda (a)
    (cond [(null? a) '()]
          [(eqv? (caar a) 'lambda) (cons (free-check '() (second (car a)) (third (car a))) (cdr a))]
          [(and (or (symbol? (caar a)) (list? (caar a))) (eqv? (cadar a) 'lambda)) (cons (free-check (first (car a)) (second (second (car a))) (third (car a))) (cdr a))]
          [else (cdr a)])))
(define free-check
  (lambda (outside var prime)
    (cond [(null? var) '()]
          [(member (car var) prime) (cons (car var) (free-check outside (cdr var) prime))]
          [(member (car outside) prime) (cons (car var) (free-check outside (cdr var) prime))]
          [else (free-check outside (cdr var) prime)])))
                                   

(define bound-vars
  (lambda (a)
    (nyi)))


(define convert-multip-calls
  (lambda (lcexp)
    (nyi)))


(define convert-multip-lambdas
  (lambda (lcexp)
    (nyi)))

(define convert-ifs
  (lambda (exp)
    (nyi)))



(define lexical-address
  (lambda (a)
    (nyi)))

(define un-lexical-address
  (lambda (a)
    (nyi)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
