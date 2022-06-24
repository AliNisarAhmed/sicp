; https://stackoverflow.com/questions/903968/how-do-i-execute-a-scm-script-outside-of-the-repl-with-mit-scheme

(define (fib n)
	(if (< n 2)
	1
	(+ (fib (- n 1)) (fib (- n 2)))))

(fib 5)

(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (f x)
  (sum-of-squares (+ x 1) (* x 2)))

(display (square 5))
(display "\n")

(define (abs2 x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

; Ex 1.1

; Ex 1.2

(display (/
 (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
 (* 3 (- 6 2) (- 2 7))))  ; -37/150

; Ex 1.3

(define (sum-square-larger-2 x y z)
  (cond ((and (>= x y) (>= y z)) (sum-of-squares x y))
        ((and (>= x z) (>= y z)) (sum-of-squares x y))
        ((and (>= y x) (>= z x)) (sum-of-squares y z))
        ((and (>= y z) (>= z x)) (sum-of-squares y z))
        (else (sum-of-squares x z))
        ))

(define (sumSquareLarger2 x y z)
  (cond ((and (<= x y) (<= x z)) (sum-of-squares y z))
        ((and (<= y x) (<= y z)) (sum-of-squares x z))
        (else (sum-of-squares x y))))

(display "\n")
(display (sumSquareLarger2 3 4 5))
(display "\n")
(display (sumSquareLarger2 4 3 5))
(display "\n")
(display (sumSquareLarger2 5 4 3))

; Ex: 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;---------------------------------------------------
;---------------------------------------------------
;---------------------------------------------------


(define (average x y) (/ (+ x y) 2))

(define (good_enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square-root x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good_enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))


;---------------------------------------------------
; Ex 1.6
;---------------------------------------------------

(define (new-if pred then-clause else-clause)
  (cond (pred then-clause)
        (else else-clause)))

(define (sqrt-iter-2 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-2 (improve guess x)
                       x)))

;; The sqrt-iter-2 defined in terms of new-if will not work
;; as Scheme uses applicative order evaluation
;; which is why, the new-if procedure will evaluate both then-clause and else-clause
;; since then-clause is recursive for sqrt-iter-2, it will never finish evaluation


;---------------------------------------------------
; Ex 1.7
;---------------------------------------------------

(define (sqrt-iter-2 guess new-guess x)
  (if (good-enough-3? guess new-guess)
      new-guess
      (sqrt-iter-2 new-guess (improve new-guess x) x)))

(define (good-enough-2? guess new-guess)
  (<
   (/
     (abs (- new-guess guess))
     guess)
   0.00005))

; For machine precision, we can ask guess and next-guess to be equal
(define (good-enough-3? guess new-guess)
  (= new-guess guess))

(define (square-root-2 x)
  (sqrt-iter-2 1.0 (improve 1.0 x) x))

;---------------------------------------------------
; Ex 1.8
;---------------------------------------------------

(define (cube-root x)
  (cube-root-iter 1.0 (improve-cube-root 1.0 x) x))

(define (cube-root-iter guess next-guess x)
  (if (good-enough-2? guess next-guess)
      next-guess
      (cube-root-iter next-guess (improve-cube-root next-guess x) x)))

(define (improve-cube-root y x)
  (/
   (+
    (/ x (* y y))
    (* 2 y))
   3))

;---------------------------------------------------
; Ex 1.9
;---------------------------------------------------

(define (pluss a b)
  (if (= a 0)
      b
      (inc (pluss (dec a) b))))

; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

(define (plus a b)
  (if (= a 0)
      b
      (plus (dec a) (inc b))))

; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9


;---------------------------------------------------
; Ex 1.10
;---------------------------------------------------

; Ackerman function

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; (A 1 10)
; (A 0 (A 1 9)
; (A 0 (A 0 (A 1 8))
; (A 0 (A 0 (A 0 (A 1 7)))
; (A 0 (A 0 (A 0 (A 0 (A 1 6))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5)))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 0))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32)))
; (A 0 (A 0 (A 0 (A 0 (A 0 64))
; (A 0 (A 0 (A 0 (A 0 128)))))
; (A 0 (A 0 (A 0 256))))
; (A 0 (A 0 512))
; (A 0 1024)
; 2048

; (A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 (A 0 2)))
; (A 1 (A 1 4))
; (A 1 (A 0 (A 1 3)))
; (A 1 (A 0 (A 0 (A 1 2)))    ;; (A 1 2) = 4
; (A 1 (A 0 (A 0 4)
; (A 1 (A 0 8)
; (A 1 16)
; (A 0 (A 1 15))
; (A 0 (A 0 (A 1 14)))
; ....
; (A 0 32768)
; 65536

; (A 3 3)
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1))
; (A 2 (A 2 2))
; (A 2 (A 1 (A 2 1))
; (A 2 (A 1 2))
; (A 2 (A 0 (A 1 1)))
; (A 2 (A 0 2))
; (A 2 4)
; see above tree!

(define (f n) (A 0 n)) ; double n
(define (g n) (A 1 n)) ; 2 ^ n
(define (h n) (A 2 n)) ; 2 ^ 2 ^ 2 ^ ... (n times)


; ----------------------------------------------------------------------

; Tree recursion

(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1))
                 (fib-rec (- n 2))))))

(define (fib-iter n)
  (define (fib-iter-2 a b count)
    (if (= count 0)
        b
        (fib-iter-2 (+ a b) a (- count 1))))
  (fib-iter-2 1 0 n))

; Count change

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds) ; kinds = kinds of coins
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds 0)) 0)
        (else (+
               (cc amount (- kinds 1))
               (cc (- amount (first-den kinds)) kinds)))))

(define (first-den kinds) ; first-den = first denomination of kinds of coins
  (cond ((= kinds 1) 1)
        ((= kinds 2) 5)
        ((= kinds 3) 10)
        ((= kinds 4) 25)
        ((= kinds 5) 50)))

;---------------------------------------------------
; Ex 1.11
;---------------------------------------------------

; f(n) = n if n < 3
; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3)

; Recursive

(define (fn n)
  (cond ((< n 3) n)
        (else (+
               (fn (- n 1))
               (* 2 (fn (- n 2)))
               (* 3 (fn (- n 3)))))))

; Iterative
(define (fn-2 n)
  (define (f-iter a b c count)
    (cond ((< n 3) n)
          ((<= count 0) a)
          (else (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (f-iter 2 1 0 (- n 2)))

;---------------------------------------------------
; Ex 1.12
;---------------------------------------------------

(define (pascal row col)
  (cond ((<= row col) 1)
        ((= col 1) 1)
        (else (+
               (pascal (- row 1) (- col 1))
               (pascal (- row 1) col)))))


;---------------------------------------------------
; Ex 1.15
;---------------------------------------------------

(define (cube x) (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; sine 12.15
; (p (sine 4.05))
; (p (p (sine 1.35))
; (p (p (p (sine 0.45))))
; (p (p (p (p (sine 0.15)))))
; (p (p (p (p (p (sine 0.05))))
; (p (p (p (p (p 0.05)))))

; How many times p is applied
; we divide angle each time by 3, so if we were reaching for (< 1), the answer would be log3 12.15
; but since we keep dividing till we reach (< 0.1), the answer would be ceil(log 3 (12.15 / 0.1)) = 5

; Space and Tiem growth
; Both are O(log3 a) for (sine a)



;---------------------------------------------------
; Ex 1.16
;---------------------------------------------------

(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp b  (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))

; (define (even? n)
  ; (= (remainder n 2) 0))

; Iterative fast exponentiation 
(define (fast-exp-2 b n)
  (define (iter b n r)
    (cond ((= n 0) r)
          ((even? n) (iter (square b) (/ n 2) r))
          (else (iter b (- n 1) (* r b)))))
  (iter b n 1))


;---------------------------------------------------
; Ex 1.17
;---------------------------------------------------

(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (- b 1)))))

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (mult-fast a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mult-fast a (halve b))))
        (else (+ a (mult a (- b 1))))))


;---------------------------------------------------
; Ex 1.18
;---------------------------------------------------

; iterative fast mult

(define (mult-fast-2 a b)
  (define (iter a b acc)
    (cond ((= b 0) acc)
          ((even? b) (iter (double a) (halve b) acc))
          (else (iter a (- b 1) (+ acc a)))))
  (iter a b 0))