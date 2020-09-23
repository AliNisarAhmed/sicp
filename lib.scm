#lang sicp

(#%provide (all-defined))

(define (divides? a b) (= 0 (remainder a b)))

(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))
	)
)

(define (logn n m) (/ (log m) (log n)))

(define (id x) x)

(define (add-100 n) (+ n 100))

(define (compose f g) (lambda (x) (f (g x))))

(define (even? x) (divides? x 2))

(define (odd? x) (not (divides? x 2)))

(define (square x) (* x x))

(define (fib n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))


;; Primality test

(define (divides a b) (= 0 (remainder a b)))

(define (find-divisor n test)
	(cond ((> (square test) n) n)
              ((divides n test) test)
              (else (find-divisor n (+ 1 test)))
	)
)
(define (smallest-divisor n) (find-divisor n 2))


(define (prime? n) (= n (smallest-divisor n)))


(define (for-each proc list) 
   (cond 
    ((null? list) #t) 
    (else (proc (car list)) 
          (for-each proc (cdr list)))))


(define (any f items)
  (cond ((null? items) false)
        ((f (car items)) true)
        (else (any f (cdr items)))))

(define (all f items)
  (cond ((null? items) true)
        ((not (f (car items))) false)
        (else (all f (cdr items)))
        ))


(define (variable? x) (symbol? x))


(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (prepend item times list)
  (append (repeat item times)
          list))

(define (repeat item times)
  (define (iter result count)
    (if (= count 0)
        result
        (iter (cons item result) (- count 1))))
  (iter '() times))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random high))))


(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))