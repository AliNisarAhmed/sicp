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