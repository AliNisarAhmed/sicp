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