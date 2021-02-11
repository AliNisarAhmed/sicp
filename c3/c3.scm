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

(define (count-pairs-wrong x)
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
(define z7 '((a b) (c d) e)) ;; 7
(define zn (make-cycle '(a b)))

;; Exercise 3.17

;; do it again 

(define (count-pairs x)
  (let ((y '())
        (fst (car x))
        (snd (cdr x)))
    (cond ((and (not (pair? fst)) (null? snd)) 1)
          ((not (pair? fst)) (+ 1 (count-pairs snd)))
          ((any (lambda (ys) (eq? ys fst)) y)
           (if (null? snd)
               0
               (count-pairs snd)))
          (else (begin (append! fst y)
                       (+ (count-pairs snd)
                          1))))))


;; Exercise 3.18

;; This only caters to a list which does not have non-repeating elements. e.g. (1, 2, 3, 1, 2, 3...).
;; Does not work on the list of form (1, 2, 3, 4, 5, 6, 4, 5, 6...)
(define (cycles? list)
  (define (helper x)
    (let ((fst (car x))
        (snd (cdr x)))
    (cond ((null? snd) false)
          ((eq? snd list) true)
          (else (helper snd)))))
  (helper list))


;; To detect a cycle of the second form, we use Floyd's Tortoise and Hare algorithm

(define (contain-cycle? lst)
  (define (safe-cdr l)
    (if (pair? l)
        (cdr l)
        '()))
  (define (iter a b)
    (cond ((not (pair? a)) false)
          ((not (pair? b)) false)
          ((eq? a b) true)
          ((eq? a (safe-cdr b)) true)
          (else (iter (safe-cdr a) (safe-cdr (safe-cdr b))))
          ))
  (iter (safe-cdr lst) (safe-cdr (safe-cdr list))))

;; Implementing a Queue

;; front and rear pointers
(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))


(define (empty-q? q)
  (null? (front-ptr q)))

(define (make-q)
  (cons '() '()))

(define (front-q queue)
  (if (empty-q? queue)
      (error "FRONT called with an empty Queue")
      (car (front-q queue))))

(define (insert-q! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-q? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-q! queue)
  (cond ((empty-q? queue)
         (error "DELETE! called with an empty queue"))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)
        ))

;; Exercise 3.21

(define (print-q queue)
  (front-ptr queue))


;; Exercise 3.22


(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-q?)
      (null? front-ptr))
    (define (insert-q item)
      (let ((new-item (cons item '())))
        (cond ((empty-q?)
               (set! front-ptr new-item)
               (set! rear-ptr new-item))
              (else
               (set-cdr! rear-ptr new-item)
               (set! rear-ptr new-item)))))
    (define (delete-q)
      (cond ((empty-q?) (error "Can't call delete on emtpy"))
            (else (set! front-ptr (cdr front-ptr)))
            ))
    (define (print)
      front-ptr)
    (define (dispatch m)
      (cond ((eq? m 'empty-q?) (empty-q?))
            ((eq? m 'insert-q) insert-q)
            ((eq? m 'print) (print))
            ((eq? m 'delete-q) (delete-q))
            ))
    dispatch))


(define (empty-q2? q) (q 'empty-q?))
(define (insert-q2 q item) ((q 'insert-q) item))
(define (print-q2 q) (q 'print))
(define (delete-q2 q) (q 'delete-q))

(define my-q (make-queue))


;; Exercise 3.23

;; double ended queue called dequeue in the book

(define (make-dq)
  (cons '() '()))

(define (front-dq q) (car q))
(define (rear-dq q) (cdr q))

(define (set-front-dq! q item)
  (set-car! q item))

(define (set-rear-dq! q item)
  (set-cdr! q item))

(define (empty-dq? q) (null? (front-dq q)))

(define (front-insert-dq! q item)
  (cond ((empty-dq? q)
         (let ((new-item (cons item '())))
           (begin (set-car! q new-item)
                  (set-cdr! q new-item))))
        ((= 1 (length (front-dq q)))
         (let ((new-item (cons item (front-dq q))))
           (set-front-dq! q new-item)
           (set-cdr! q (cons (rear-dq q) item))))
        (else
         (let ((new-item (cons item (front-dq q))))
           (set-car! q new-item)))
        ))

(define (rear-insert-dq! q item)
  (cond ((empty-dq? q)
         (let ((new-item (cons item '())))
           (begin (set-car! q new-item)
                  (set-cdr! q new-item))))
        ((= 1 (length (front-dq q)))
         (let ((new-item (cons item '())))
           (set-cdr! (rear-ptr q) new-item)
           (set-cdr! new-item (rear-ptr q))
           (set-rear-ptr! q new-item)
           ))
        (else
         (let ((new-item (cons item '())))
           (set-cdr! (rear-ptr q) new-item)
           (set-cdr! new-item (rear-ptr q))
           (set-rear-ptr! q new-item)
           ))))

(define (front-delete-dq! q)
  (cond ((empty-dq? q)
         (error "Delete called with empty D-Queue"))
        (else
         (let ((next (cdr (front-dq q))))
          (set-car! q next)))
        ))

(define (rear-delete-dq! q)
  (cond ((empty-dq? q)
         (error "Delete called with empty dq"))
        (else
         (set-car! (rear-dq q) '()))
        ))

(define my-dq (make-dq))

#|

(front-insert-dq! my-dq 'a)
(front-insert-dq! my-dq 'b)
(front-insert-dq! my-dq 'c)
(front-insert-dq! my-dq 'd)


|#










