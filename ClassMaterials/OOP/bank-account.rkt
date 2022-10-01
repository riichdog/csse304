#lang racket

(define make-bank-account
  (lambda (starting-balance)
    (let ([balance starting-balance])
      (lambda (command . args)
        (cond [(eqv? command 'balance) balance]
              [(eqv? command 'deposit) (set! balance (+ balance (car args)))]
              [(eqv? command 'withdraw) (set! balance (- balance (car args)))]
              [else ('error)]
        )))))

; hint, you might find the lambda form with required
; and optional args useful here
; e.g. (lambda (command . extra-args)

(define ba1 (make-bank-account 100))
(define ba2 (make-bank-account 500))

(ba1 'deposit 200)
(ba1 'balance) ; should print 300

(ba2 'withdraw 50)
(ba2 'balance) ; should print 450