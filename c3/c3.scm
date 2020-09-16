#lang sicp

(#%require "../lib.scm")

;; -----------------------------------------------------


(define balance 100)

(define (withdraw amount)
  (if (> amount balance)
      (error "Insufficient Balance")
      (begin (set! balance (- balance amount))
             balance)))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          (error "Insufficient Balance")))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        (error "Insufficient funds"))))


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        (error "Insufficient funds")))
  (define (deposit amount)
    (begin (set! balance (+ amount balance))
           balance))
  (define (dispatch method)
    (cond ((eq? method 'withdraw) withdraw)
          ((eq? method 'deposit) deposit)
          (else (error "No such method"))))
  dispatch
  )


;; Exercise 3.1


(define (make-accumulator value)
  (lambda (increment)
    (begin (set! value (+ value increment))
           value)))


;; Exercise 3.2

(define (make-monitored f)
  (let ((count 0))
    (lambda (v)
      (cond ((eq? v 'how-many-calls?) count)
            ((eq? v 'reset-count) (set! count 0))
            (else (begin (set! count (+ 1 count))
                         (f v)))
            ))))


;; Exercise 3.3

(define (make-account-pwd balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        (error "Insufficient balance")))
  (define (deposit amount)
    (begin (set! balance (+ amount balance))
           balance))
  (define (dispatch pwd method)
    (if (eq? pwd password)
        (cond ((eq? method 'withdraw) withdraw)
              ((eq? method 'deposit) deposit)
              (else (error "No such method"))
              )
        (error "Password does not match")))
  dispatch
  )


;; Exercise 3.4


(define (make-account-pwd-2 balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        (error "Insufficient balance")))
  (define (deposit amount)
    (begin (set! balance (+ amount balance))
           balance))
  (define (call-the-cops) (lambda (x) (display "Calling the cops, you wait there!")))
  (let ((count 0))
    (lambda (pwd method)
      (if (eq? pwd password)
        (cond ((eq? method 'withdraw) (begin (set! count 0)
                                             withdraw))
              ((eq? method 'deposit) (begin (set! count 0)
                                            deposit))
              (else (error "No such method"))
              )
        (if (>= count 7)
            (call-the-cops)
            (begin (set! count (+ 1 count))
                   (error "Password does not match")))))))


;; Section 3.1.2

(define rand (let ((x random-init))
               (lambda ()
                 (set! x (random-update x))
                 x)))































