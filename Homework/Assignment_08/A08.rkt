#lang racket

(provide make-slist-leaf-iterator subst-leftmost)

(define make-slist-leaf-iterator
  (lambda (a)
    (letrec ([lst a] [stack (make-stack)] [ele 0] [iter (lambda() (cond
                                                                    [(null? lst) (if (stack 'empty?) #f (begin (set! lst (stack 'pop)) (iter)))]
                                                                    [(symbol? (car lst)) (begin (set! ele (car lst)) (set! lst (cdr lst)) ele)]
                                                                    [(list? (car lst)) (begin (stack 'push (cdr lst)) (set! lst (car lst)) (iter))]))])
      
      (lambda(command)
        (iter)))))


        

(define make-stack
  (lambda ()
    (let ([stk '()])
      (lambda (msg . args )
        (case msg 
          [(empty?) (null? stk)]
          [(push) (set! stk (cons (car args) stk))]
          [(pop) (let ([top (car stk)]) (set! stk (cdr stk)) top)]
          [else (error 'stack "illegal message to stack object: ~a" msg)])))))

;;-- have a count
;;-- cdr count times
;;-- retunr car
;;-- BOOM
;;-- i wasted so much time
;;-- seems like time is the only thing i wish was mine
;;-- i dont know if im ready to jump
;;-- out the window

(define subst-leftmost
  (lambda (new old lst op)
    (cond [(null? lst) '()]
          [(symbol? (car lst)) (let ([ret (sumbsym new old (car lst) op)]) (if (car ret) (cons (second ret) (cdr lst)) (cons (car lst) (subst-leftmost new old (cdr lst) op))))]
          [else (let ([sublist (subyst new old (car lst) op '())]) (cons (second sublist) (if (car sublist) (cdr lst) (subst-leftmost new old (cdr lst) op))))])))

(define subyst
  (lambda (new old lst op newlst)
    (cond [(null? lst) (list #f newlst)]
          [(symbol? (car lst)) (let ([ret (sumbsym new old (car lst) op)])
                                 (if (car ret)
                                     (list #t (cons (second ret) (cdr lst)))
                                     (subyst new old (cdr lst) op (cons (car lst) newlst))))]
          [else (let ([sublist (subyst new old (car lst) op '())])
                  (if (car sublist)
                      (list (car sublist) (cons (cadr sublist) (cdr lst)))
                      (subyst new old (cdr lst) op (append newlst (car lst)))))])))


(define sumbsym
  (lambda (new old sym op)
    (cond [(op old sym) (list #t new)]
          [else (list #f old)])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
