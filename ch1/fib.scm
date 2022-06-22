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