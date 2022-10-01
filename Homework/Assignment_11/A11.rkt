#lang racket

(require "../chez-init.rkt")
(provide my-let my-or += return-first bintree? leaf-node interior-node bintree-to-list max-interior parse-exp unparse-exp)

(define-syntax my-let
  (syntax-rules ()
    [(my-let  ((name val) ...) k1 k2 ...)
     ((lambda (name ...) k1 k2 ...) val ...)]
    [(my-let name ((var val) ...) k1 k2 ...)
     (letrec ([name (lambda (var ...) k1 k2 ...)]) (name val ...))]))

(define-syntax my-or
  (syntax-rules ()
    [(my-or) #f]
    [(my-or k1) k1]
    [(my-or k1 k2 k3 ...)
     (let ([x k1])
       (if x
         x
         (my-or k2 k3 ...)))]))
(define-syntax +=
  (syntax-rules ()
    [(+= k1 val) (begin (set! k1 (+ k1 val)) k1)]))

(define-syntax return-first
  (syntax-rules ()
    [(return-first k1 k2 ...) (let ([kn k1])(begin k2 ... kn))]))

(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left-tree bintree?)
   (right-tree bintree?)))

(define bintree-to-list
  (lambda (a)
    (cases bintree a
      [leaf-node (num)
                 (list 'leaf-node
                       num)]
      [interior-node (node left right)
                     (list 'interior-node
                           node
                           (bintree-to-list left)
                           (bintree-to-list right))])))

(define max-interior
  (lambda (a)
    (second (max-help a))))
(define max-help
	(lambda (T)
		(cases bintree T
			[leaf-node (datum) datum]
			[interior-node (node left right)
                                       (cond [(and (number? (max-help left)) (number? (max-help right))) (list (+ (max-help left) (max-help right)) node (+ (max-help left) (max-help right)))]
                                             [(and (not (number? (max-help left))) (number? (max-help right))) (let ([curr (third (max-help left))] [sum (+ (max-help right) (first (max-help left)))])
                                                                                                                 (if (<= curr sum)
                                                                                                                     (list sum node sum)
                                                                                                                     (list sum (second (max-help left)) curr)))]
                                             [(and (number? (max-help left)) (not (number? (max-help right)))) (let ([curr (third (max-help right))] [sum (+ (max-help left) (first (max-help right)))])
                                                                                                                 (if (<= curr sum)
                                                                                                                     (list sum node sum)
                                                                                                                     (list sum (second (max-help right)) curr)))]
                                             [else (let ([curr (if (< (third (max-help right)) (third (max-help left)))
                                                                   (cdr (max-help left))
                                                                   (cdr (max-help right)))]
                                                         [sum (+ (first (max-help left)) (first (max-help right)))])
                                                        (if (<= (second curr) sum)
                                                            (list sum node sum)
                                                            (list sum (first curr) (second curr))))])])))

; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (data number?)]
  [lambda-exp
   (id symbol?)
   (body expression?)]
  [app-exp
   (rator expression?)
   (rand expression?)])

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp         
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(number? datum) (lit-exp datum)]
      [(pair? datum)
       (cond
         [(eqv? (car datum) 'lambda)
          (lambda-exp (car (2nd  datum))
                      (parse-exp (3rd datum)))]
         [else (app-exp (parse-exp (1st datum))
                        (parse-exp (2nd datum)))])]
      [else (error 'parse-exp "bad expression: ~s" datum)])))

(define unparse-exp
  (lambda (exp)
    (nyi)))

; An auxiliary procedure that could be helpful.
(define var-exp?
  (lambda (x)
    (cases expression x
      [var-exp (id) #t]
      [else #f])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
