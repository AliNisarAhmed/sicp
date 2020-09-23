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

#|


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


|#

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

;; ---- sample functions

(define random-init 0)
(define (random-update x) (+ x 1))

;; ----

(define rand (let ((x random-init))
               (lambda ()
                 (set! x (random-update x))
                 x)))



(define (estimate-pi num-trials)
  (sqrt (/ 6 (monte-carlo num-trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand) 1)))

(define (monte-carlo num-trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed num-trials))
          ((experiment) (iter (- trials-remaining 1) (+ 1 trials-passed)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter num-trials 0))


;; Exercise 3.5


(define (estimate-integral p x1 x2 y1 y2 num-trials)
  (define experiment
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (check-if-in-unit-circle x y)))
  (monte-carlo num-trials experiment)
  )

(define (estimate-pi-2 num-trials)
  (let ((radius 1))
    (/ (estimate-integral check-if-in-unit-circle 0 2 0 2 num-trials) (* radius radius))))

(define (check-if-in-unit-circle x y)
  (let ((center-x 1)
        (center-y 1))
    (<= (+ (square (- x center-x))
           (square (- y center-y)))
        1)))


;; Exercise 3.6

(define rand-2
  (let ((x random-init))
    (define (dispatch command)
      (cond ((eq? command 'generate)
         (begin (set! x (random-update x))
                 x))
        ((eq? command 'reset)
         (lambda (new-value) (set! x new-value)))
        (else (error "Unknown command"))
        ))
    dispatch))



(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))



;; Exercise 3.7


(define (make-account-pwd balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        (error "Insufficient balance")))
  (define (deposit amount)
    (begin (set! balance (+ amount balance))
           balance))
  (define (check-pwd pwd)
    (eq? pwd password))
  (define (dispatch pwd method)
    (if (eq? pwd password)
        (cond ((eq? method 'withdraw) withdraw)
              ((eq? method 'deposit) deposit)
              ((eq? method 'check-password) check-pwd)
              (else (error "No such method"))
              )
        (error "Password does not match")))
  dispatch
  )



(define (make-joint account orig-pwd new-pwd)
  (define acc account)
  (define (dispatch pwd method)
    (if (account orig-pwd 'check-password)
        (if (eq? pwd new-pwd)
            (acc orig-pwd method)
            (error "Wrong password for the account"))
        (error "Wrong password for the account")))
  dispatch)


(define peter-acc (make-account-pwd 100 'abc))
(define paul-acc (make-joint peter-acc 'abc 'xyz))
(define sam-acc (make-joint peter-acc 'abc 'sam))


;; Exercise 3.8

;; (+ (f 0) (f 1)) == 0 LSH == 1 RHS

(define f
  (let ((store 'init))
    (lambda (x)
      (if (eq? store 'init)
          (begin (set! store x)
                 x)
          store))))


;; solved 3.9 & 3.10


;; Exercise 3.13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; (define x (list 'a 'b 'c))
;; (define y (list 'x 'y 'z))


;;

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)


;; Exercise 3.16

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define x '(a))
(define y '(b))
(define z '(c))

(define z1 '(a b c)) ;; 3
(define z4 '((a) b c)) ;; 4
(define z7 (cons z1 z1)) ;; 7































