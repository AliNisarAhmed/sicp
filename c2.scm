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

(define i1 (make-interval -4 -10))
(define i2 (make-interval -2 -12))
(define diff (sub-interval i1 i2))


;; Exercise 2.9

(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


;; Exercise 2.10

(define (mul-inverse i)
  (let ((x (lower-bound i))
        (y (upper-bound i)))
    (if (or (= x 0) (= y 0))
        (error "Inverse of second interval is not possible, division by zero")
        (make-interval (/ 1.0 y) (/ 1.0 x)))))

(define (div-interval-2 i1 i2)
  (let ((inv (mul-inverse i2)))
    (mul-interval i1 inv)))


;; Exercise 2.11

(define (mult-interval-2 i1 i2)
  (let ((l1 (lower-bound i1))
        (h1 (upper-bound i1))
        (l2 (lower-bound i2))
        (h2 (upper-bound i2)))
    (cond ((and (>= l1 0) (>= h1 0) (>= l2 0) (>= h2 0)) (make-interval (* l1 l2) (* h1 h2)))
          ((and (< l1 0) (< h1 0) (< l2 0) (< h2 0)) (make-interval (* l1 l2) (* h1 h2)))
          ((and (>= l1 0) (>= h1 0))
           (cond ((>= l2 0) (make-interval (* l1 l2) (* h1 l2)))
                 ((>= h2 0) (make-interval (* l1 h2) (* h1 h2)))
                 (else (make-interval (* h1 l2) (* l1 h2)))))
          ((and (<= l1 0) (<= h1 0))
           (cond ((<= l2 0) (make-interval (* l1 l2) (* h1 l2)))
                 ((<= h2 0) (make-interval (* l1 h2) (* h1 h2)))
                 (else (make-interval (* l1 h2) (* l2 h1)))
                 ))
          ((and (>= h2 0) (>= l2 0))
           (cond ((>= l1 0) (make-interval (* l1 l2) (* l1 h2)))
                 ((>= h1 0) (make-interval (* h1 l2) (* h1 h2)))))
          (else
           (let ((p1 (* l1 l2))
                 (p2 (* l1 h2))
                 (p3 (* h1 l2))
                 (p4 (* h1 h2)))
             (make-interval (min p1 p2 p3 p4) (min p1 p2 p3 p4)))))))


;; Exercise 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center cw)
  (/ (+ (lower-bound cw) (upper-bound cw)) 2.0))

(define (width-cw cw)
  (/ (- (upper-bound cw) (lower-bound cw)) 2.0))


(define cw1 (make-center-width 10 2))


(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100.0))) (+ c (* c (/ p 100.0)))))


(define cp1 (make-center-percent 80 0.5))

(define (percent-of-interval cp)
  (let ((c (center cp)))
    (* (/ (- (upper-bound cp) c) c) 100)))


;; Exercise 2.13

(define a1 (make-center-percent 20 0.5))

(define a2 (make-center-percent 20 0.4))

(define prod-a1-a2 (mul-interval a1 a2)) ;; ( 396.408, 403.608 )

(define (percent-tolerance i)
  (* (/ (width-cw i) (center i)) 100))

(define percent-tol-a1-a2 (percent-tolerance prod-a1-a2))
;; The above is 0.89998 which is approx 0.5 + 0.4 ~ 0.9


;; Exercise 2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1) (div-interval one r2)))))



(define pc1 (make-center-percent 100 20))
(define pc2 (make-center-percent 80 15))
(define one-int (make-interval 1 1))




;; ---- Lists ----

(define one-to-four (list 1 2 3 4))



(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))


(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))


(define odds (list 1 3 5 7))


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))


;; Exercise 2.17

(define (last-pair items)
  (let ((remaining (cdr items)))
    (if (null? remaining)
        (car items)
        (last-pair (cdr items)))))


;; Exercise 2.18


(define (reverse items)
  (let ((remaining (cdr items)))
    (if (null? remaining)
        items
        (append (reverse remaining) (list (car items))))))


;; Exercise 2.19

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values) 0))
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) (coin-values) )))))



(define (no-more? items)
 (null? items))

(define (except-first-denomination items)
  (cdr items))

(define (first-denomination items)
  (car items))


(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))



;; Exercise 2.20

(define (filter f items)
  (define (iter new-items old-items)
    (cond ((null? old-items) new-items)
          ((f (car old-items)) (iter (append new-items (list (car old-items))) (cdr old-items)))
          (else (iter new-items (cdr old-items)))))
  (iter '() items)
  )

(define (same-parity . items)
  (let ((first (car items)))
    (if (even? first)
        (filter even? items)
        (filter odd? items))))


;; Exercise 2.21

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-2 items)
  (map square items))


;; Exercise 2.22

(define (for-each f items)
  (cond ((null? items) (newline))
        (else (f (car items))
              (for-each f (cdr items)))))


(define (print-newline items)
  (for-each (lambda (x)
              (newline)
              (display x)) items))


;; Exercise 2.25

(define (count-leaves items)
  (cond ((null? items) 0)
        ((not (pair? items)) 1)
        (else (+ (count-leaves (car items)) (count-leaves (cdr items)) )) ))

(define x (cons (list 1 2) (list 3 4)))

(define x1 (list 1 3 (list 5 7) 9))
(define x2 (list (list 7)))
(define x3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(define a (list 1 2 3))
(define b (list 4 5 6))


;; Exercise 2.27


(define (deep-reverse items)
  (cond ((null? items) nil)
        (else (let ((first (car items))
                    (rest (cdr items)))
                (if (pair? first)
                    (append (deep-reverse rest) (list (deep-reverse first)))
                    (append (deep-reverse rest) (list first)))))))


(define d1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8)))


;; Exercise 2.28

(define (fringe tree)
  (if (null? tree)
      nil
      (let ((first (car tree))
            (rest (cdr tree)))
        (if (pair? first)
            (append (fringe first) (fringe rest))
            (cons first (fringe rest))))))

(define my-tree (list 1 (list 2 (list 3 4) (list 5 6)) (list 7 (list 8))))


(define (fringe-iter tree)
  (define (iter items result)
    (cond ((null? items) result)
          ((pair? items) (iter (car items) (iter (cdr items) result)))
          (else (cons items result))))
  (iter tree nil))


;; Exercise 2.29

(define (make-mobile left right)
  (list left right))


(define (make-branch len structure)
  (list len structure))


(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))



(define (branch-length br) (car br))

(define (branch-structure br) (cadr br))


(define m1 (make-mobile (make-branch 2 20) (make-branch 3 30)))
(define m2 (make-mobile (make-branch 4 40) (make-branch 5 50)))
(define m3 (make-mobile (make-branch 6 m1) (make-branch 7 m2)))

;; (b)


(define (total-weight-m mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (cond ((and (pair? (branch-structure left)) (pair? (branch-structure right)))
           (+ (total-weight (branch-structure left)) (total-weight (branch-structure right))))
          ((pair? (branch-structure left))
           (+ (total-weight (branch-structure left)) (branch-structure right) ))
          ((pair? (branch-structure right))
           (+ (branch-structure left) (total-weight (branch-structure right))))
          (else (+ (branch-structure left) (branch-structure right))))))

(define (total-weight m)
  (cond ((null? m) 0)
        ((not (pair? m)) m)
        (else (+
               (total-weight (branch-structure (left-branch m)))
               (total-weight (branch-structure (right-branch m)))))))






































