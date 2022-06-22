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
