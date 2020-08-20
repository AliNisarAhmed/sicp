(define nil '())
(define true #t)
(define false #f)
(define (random n)
  (random-integer n))

; https://github.com/biwascheme/biwascheme/issues/110#issuecomment-335869546
(define (date2runtime date)
  ; HACK
  ; wraps around occasionally!
  (+
     (* (date-hour date) 60 60 1000)
     (* (date-minute date) 60 1000)
     (* (date-second date) 1000)
     (date-millisecond date)
  )
)

(define (runtime) (date2runtime (current-date)))


(define (square x) (* x x))

(define (sumOfSquare x y) (+ (square x) (square y)))

(define (abs x)
  (cond ((> x 0) x)
	      ((= x 0) 0)
        ((< x 0) (- x)))
)

(define (sumOfBigTwo x y z)
  (cond ((and (>= x y) (>= y z)) (sumOfSquare x y))
		    ((and (>= y z) (>= z x)) (sumOfSquare y z))
	      ((and (>= z x) (>= x y)) (sumOfSquare z x))))


(define (sqr-iter guess x)
  (if (good-enough? guess x)
	  guess
		(sqr-iter (improve guess x) x)
	)
)

(define (improve guess x)
  (average x (/ x guess))
)

(define (average x y)
  (/ (+ x y) 2)
)

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
)

(define (sqrt x) (sqr-iter 1.0 x))


(define (cube-root guess x)
  (if (good-enough? guess x)
	  guess
		(cube-root (improve2 guess x) x)
	)
)

(define (improve2 guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3.0)
)

(define (cbrt x) (cube-root 1.0 x))


(define (ack x y)
  (cond ((= y 0) 0)
	      ((= x 0) (* 2 y))
	      ((= y 1) 2)
				(else (ack (- x 1) (ack x (- y 1))))
	)
)

;; ------------------

; Count Change

; kinds below is kinds of coins

(define (count-change amount) (cc amount 5))
(define (cc amount kinds)
	(cond ((= amount 0) 1)
				((or (< amount 0) (= kinds 0)) 0)
				(else (+
							  (cc amount (- kinds 1))
								(cc (- amount (first kinds)) kinds)
							)
				)
	)
)
(define (first kinds)
	(cond
		((= kinds 1) 1)
		((= kinds 2) 5)
		((= kinds 3) 10)
		((= kinds 4) 25)
		((= kinds 5) 50)
	)
)

;; ------------------

; Exercise 1.11

(define (f n)
  (if (< n 3)
	  n
		(+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))) )
	)
)

; Iterative version of the above
(define (g n) (g-iter a b c count)
  (define (g-iter a b c count)
    (cond ((< n 3) n)
	        ((<= count 0) a)
				  (else (g-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))
	  )
  )
	(g-iter 2 1 0 (- n 2))
)

; Another iterative version of the above

(define (foo n)
  (define (fooi a b c n1)
    (fooi (+ a (* 2 b) (* 3 c)) a b (- n1 1))
	)
	(if (< n 3)
	  n
		(fooi 2 1 0 n)
	)
)

; Exercise 1.12

(define (pascal row column)
	(cond ((or (<= row 0) (<= column 0) (> column row)) 0)
				((or (= row 1) (= column 1) (= row column)) 1)
				(else (+
								(pascal (- row 1) (- column 1))
								(pascal (- row 1) column)
							)
				)
	)
)


; Exercise 1.14

;; https://codology.net/post/sicp-solution-exercise-1-14/


; Exercise 1.15

;; https://codology.net/post/sicp-solution-exercise-1-15/


;; -----------------------------


;; Linear Recursive Exponentiation

(define (expt b n)
	(if (= n 0)
		1
		(* b (expt b (- n 1)))
	)
)

;; Iterative exponentiation

(define (exp b n)
	(exp-iter b n 1)
)
(define (exp-iter b n product)
	if (= n 0)
		product
		(exp-iter b (- n 1) (* b product))
)


;; fast exponentiation

(define (fast-exp b n)
	(cond ((= n 0) 1)
				((even? n) (square (fast-exp b (/ n 2))))
				(else (* b (fast-exp b (- n 1))))
	)
)

(define (even? n)
	(= (mod n 2) 0)
)

;; iterative fast exponentiation

(define (fast-exp-iter b n)
	(fast-exp-i b n 1)
)
(define (fast-exp-i b n product)
	(cond ((= n 0) product)
				((even? n) (fast-exp-i (square b) (/ n 2) product))
				(else (fast-exp-i b (- n 1) (* b product)))
	)
)


;; Exercise 1.17

(define (m a b)
	(if (= b 0)
		0
		(+ a (m a (- b 1)))
	)
)

(define (double x) (* x 2))

(define (half x) (floor (/ x 2)))

(define (log-mult a b)
	(cond ((= b 1) a)
				((even? b) (log-mult (double a) (half b)))
				(else (+ a (log-mult a (- b 1))))
	)
)

;; iterative version of above

(define (log-mult-i a b result)
	(cond ((= b 0) result)
				((even? b) (log-mult-i (double a) (half b) result))
				(else (log-mult-i a (- b 1) (+ a result)))
	)
)
(define (log-mult-iter a b)
	(log-mult-i a b 0)
)


;;---------------


;; Euclid's Algo for GCD

(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (mod a b))
	)
)

;;---------------

; Testing for Primality

; Searching for divisors

(define (divides a b) (= 0 (mod a b)))

(define (find-divisor n test)
	(cond ((> (square test) n) n)
				((divides n test) test)
				(else (find-divisor n (+ 1 test)))
	)
)
(define (smallest-divisor n) (find-divisor n 2))


(define (prime? n) (= n (smallest-divisor n)))

;; Fermat's Little Theorem for Primality test

(define (expmod base expo m)
	(cond ((= expo 0) 1)
				((even? expo)
					(mod (square (expmod base (/ expo 2) m)) m)
				)
				(else (mod (* base (expmod base (- expo 1) m)) m))
	)
)

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a)
	)
	(try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
	(cond ((= times 0) true)
				((fermat-test n) (fast-prime? n (- times 1)))
				(else false)
	)
)

;; Exercise 1.22
;; Print Time while performing the function below

(define (timed-prime-test n)
	(newline)
	(display n)
	(start-prime-test n (runtime))
	(newline)
)

(define (start-prime-test n start-time)
	(if (prime? n)
		(report-prime (- (runtime) start-time))
	)
)
(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time)
)

(define (search-for-primes start end)
	(if (even? start)
		(search-for-primes (+ 1 start) end)
		(find-three-primes start end 0 (runtime))
	)
)

(define (find-three-primes start end count time)
	(cond ((>= start end) 
					(display "could not find three primes")
				)
				((= count 3) 
						(display "*** final time ***")
						(newline)
						(display (- (runtime) time))
						(newline)
				)
				((prime2 start) 
					(newline)
					(display "Found Prime Number \n")
					(display start)
					(newline)
					(find-three-primes (+ 2 start) end (+ 1 count) time)
				)
				(else (find-three-primes (+ 2 start) end count time))
	)
)

;; Exercise 1.23

(define (next-odd n) 
	(if (= n 2)
		3 
		(+ n 2)
	)
)

(define (new-smallest-divisor n)
	(new-find-divisor n 2)
)

(define (new-find-divisor n test)
	(cond ((> (square test) n) n)
				((divides n test) test)
				(else (new-find-divisor n (next-odd test)))
	)
)

(define (prime2 n) (= n (new-smallest-divisor n)))


;; Exercise 1.24

(define (timed-prime-test-2 n)
	(newline)
	(display n)
	(start-prime-test-2 n (runtime))
	(newline)
)

(define (start-prime-test-2 n time)
	(if ((fast-prime? n 100))
			(report-prime (- (runtime) time))
	)
)

;; Ex: 1.27

(define (carmichael n)
	(define (apply-expmod start end)
		(cond ((>= start end) true)
					((= (expmod start end end) start) (apply-expmod (+ 1 start) end))
					(else false)
		)
	)
	(apply-expmod 1 n)
)

;; Exercise 1.28

; (define (expmod base expo m)
; 	(cond ((= expo 0) 1)
; 				((even? expo)
; 					(mod (square (expmod base (/ expo 2) m)) m)
; 				)
; 				(else (mod (* base (expmod base (- expo 1) m)) m))
; 	)
; )

(define (expmod2 base e m)
	(cond ((= e 0) 1)
				((even? e)
					(cond ((= base 1) 0)
								((= base (- m 1)) 0)
								((= ))
					)
				)
				(else (mod (* base (expmod2 base (- e 1) m))))
	)
)

;; --------------------


;; Section 1.3

(define (cube x)
	(* x (* x x))
)

;; sum of integers from a thru b
(define (sum-ints a b)
	(if (> a b)
		0
		(+ a (sum-ints (+ 1 a) b))
	)
)

;; sum of cube of ints from a thru b
(define (sum-cubes a b)
	(if (> a b)
		0
		(+ (cube a) (sum-cubes (+ 1 a) b))
	)
)

;; sum of sequence 1/(1x3) + 1/(5x7) + 1/(9x11) + ... = pi / 8

(define (pi-sum a b)
	(if (> a b)
		0 
		(+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))
	)
)


;; The above three functions have the same pattern, i.e. summation

(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a) (sum term (next a) next b))
	)
)

;; now we can define the three functions using the sum functions

(define (id x) x)

(define (inc x) (+ x 1))

(define (sum-ints2 a b)
	(sum id a inc b)
)

(define (pi-sum2 a b)
	(define (pi-term x) (/ 1.0 (* x (+ x 2))))
	(define (pi-next y) (+ y 4))
	(sum pi-term a pi-next b) 
)

(define (sum-cubes2 a b)
	(sum cube a inc b)
)

(define (integral f a b dx)
	(define (add-dx x)
		(+ x dx)
	)
	(* (sum f (+ a (/ dx 2.0)) add-dx b) 
		dx
	)
)


;; Exercise 1.29

; (define (sum term a next b)
; 	(if (> a b)
; 		0
; 		(+ (term a) (sum term (next a) next b))
; 	)
; )

(define (simpson f a b n)
	(define h (/ (- b a) n))
	(define (yk k) (f (+ a (* h k))))
	(define (simpson-term k)
		(* (cond ((or (= k 0) (= k n)) 1)
						 ((even? k) 2)
						 (else 4)
			 )
			 (yk k)
		)
	)
	(* (/ h 3) (sum simpson-term 0 inc n))
)

(define (simpson2 f a b n)
	(define (h) (/ (- b a) n))
	(define (add-2h x) (+ (* 2 h) x))
	(* (/ h 3.0) 
		(+ (f a)
			 (* 4.0 (sum f (+ a h) add-2h b))
			 (* 2.0 (sum f (add-2h a) add-2h (- b h)))
			 (f b)
		)
	)
)

;; 1.30 

; Iterative sum 

(define (sum-iter term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ result (term a)))
		)
	)
	(iter a 0)
)

;; 1.31 

;; (a)

(define (product term a next b)
	(if (> a b)
		1		
		(* (term a) (product term (next a) next b))
	)
)

(define (fact-prod n)
	(product id 1 inc n)
)

(define (pi-approx end)
	(define (next-term a) 
		(/ (* a (+ a 2)) (* (+ 1 a) (+ 1 a)))
	)
	(define (add2 x) (+ x 2))
	(* 4 
		(product next-term 2 add2 end)
	)
)

;; 1.31 (b)

(define (product-iter term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (* result (term a)))
		)
	)
	(iter a 1)
)

;; 1.32 (a)

(define (accumulate combiner null-value term a next b)
	(if (> a b)
		null-value
		(combiner (term a) (accumulate combiner null-value term (next a) next b))
	)
)

(define (sum-acc term a next b)
	(accumulate + 0 term a next b)
)

(define (prod-acc term a next b)
	(accumulate * 1 term a next b)
)

;; 1.32 (b)

(define (acc-iter combiner null term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (combiner result (term a)))
		)
	)
	(iter a null)
)

(define (sum-acc-iter term a next b)
	(acc-iter + 0 term a next b)
)

(define (prod-acc-iter term a next b)
	(acc-iter * 1 term a next b)
)


;; 1.33 

(define (filtered-acc combiner null term a next b pred)
	(cond ((> a b) null)
				((pred a) 
					(combiner 
						(term a) 
						(filtered-acc combiner null term (next a) next b pred)
					)
				)
				(else (filtered-acc combiner null term (next a) next b pred))
	)	
)

(define (sum-sqr-primes a b)
	(filtered-acc + 0 square a inc b prime?)
)

(define (prod-rel-prime n)
	(define (pred i) 
		(= (gcd i n) 1)
	)
	(filtered-acc * 1 id 1 inc n pred)
)

;; Section 1.33

; representing f(x,y) = x(1+xy)^2 + y(1-y) + (1+xy)(1-y)

;; a = 1 + xy
;; b = 1 - y
;; f(x,y) = xa^2 + yb + ab

(define (f x y)
	(define (f-helper a b)
		(+ (* x (square a))
		   (* y b)
			 (* a b)
		)
	)
	(f-helper (+ 1 (* x y))
						(- 1 y)
	)
)

(define (f2 x y)
	( (lambda (a b)
			(+ (* x (square a))
				 (* y b)
				 (* a b)
			)
		)
		(+ 1 (* x y))
		(- 1 y)
	)
)

(define (f3 x y)
	(let ( (a (+ 1 (* x y)))
			   (b (- 1 y)) 
			 )
		(+ (* x (square a))
			 (* y b)
			 (* a b)
		)
	)
)