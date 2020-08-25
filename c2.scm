#lang sicp

(#%require "lib.scm")

;; Exercise 2.4

(define (cons2 x y)
  (lambda (m) (m x y)))

(define (car2 z)
  (z (lambda (p q) p)))

(define (cdr2 z)
  (z (lambda (p q) q)))

;; Exercise 2.5

(define (cons-non-neg a b)
  (* (expt 2 a) (expt 3 b)))

(define (car-non-neg z)
  (define (iter n)
    (if (divides? n 3.0)
        (iter (/ n 3.0))
        n))
  (let ((even-part (iter z)))
    (round (logn 2 even-part))))

(define (cdr-non-neg z)
  (define (iter n)
    (if (divides? n 2)
        (iter (/ n 2.0))
        n))
  (let ((odd-part(iter z)))
    (round (logn 3 odd-part))))

(define v1 (cons-non-neg 4 5))

;; Exercise 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define three (lambda (f) (lambda (x) (f (f (f x))))))


;; This shows the function composition represents multiplication
;; apply n2 to f, then apply n1 to the result of that
(define (church-mult n1 n2) (lambda (f) (n1 (n2 f))))

;; apply f n1 times and get a result, then apply f n2 times
(define (church-add n1 n2) (lambda (f) (lambda (x) ((n2 f) ((n1 f) x)))))



;; Exercise 2.7

(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (upper-bound i) (cdr i))

(define (lower-bound i) (car i))


;; Exercise 2.8


(define (sub-interval x y)
  (add-interval x (negate-interval y)))


;; To negate an interval, we multiply both bounds by -1;
;; but by doing so, the upper bound becomes the lower bound and vice versa
;; so, we need to switch upper and lower bounds
(define (negate-interval y)
  (make-interval (* -1 (upper-bound y)) (* -1 (lower-bound y))))

(define i1 (make-interval 4 10))
(define i2 (make-interval 2 12))
(define diff (sub-interval i1 i2))


;; Exercise 2.9

(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2))













































