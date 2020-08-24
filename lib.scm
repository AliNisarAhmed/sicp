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