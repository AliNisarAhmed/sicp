#lang sicp

(#%require "../lib.scm")

(#%require "complex.scm")

;;

(define (add x y) (apply-generic 'add x y))

(define (sub x y) (apply-generic 'sub x y))

(define (mul x y) (apply-generic 'mul x y))

(define (div x y) (apply-generic 'div x y))

;; Ordinary numbers


(define (install-scheme-number)

  (define (tag x) (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))

  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))

  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))

  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))

  (put 'make 'scheme-number (lambda (x) (tag x)))

  (put 'equ '(scheme-number scheme-number) (lambda (x y) (tag (= x y))))

  (put '=zero? 'scheme-number (lambda (x) (= x 0)))

  'done
  )


;; this is how users will make ordinary numbers
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))



;; Rational numbers

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; interface to the rest of the system

  (define (tag x) (attach-tag 'rational x))

  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))

  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))

  (put 'mul '(rational rational) (lambda (X y) (tag (mul-rat x y))))

  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))

  (put 'equ '(rational rational) (lambda (x y) (tag (and (= (numer x) (numer y))
                                                         (= (denom x) (denom y))))))

  (put '=zero? 'rational (lambda (x) (= (numer x) 0)))

  'done
  )


;; this is how users will make rational numbers
(define (make-rational n d)
  ((get 'make 'rational) n d))



;; Complex numbers

(define (install-complex-package)
  ;; imported from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-angle r a)
    ((get 'make-from-mag-angle 'polar) r a))

  ;; internal procedures

  (define (add-complex c1 c2)
    (make-from-real-imag (+ (real-part c1) (real-part c2))
                         (+ (imag-part c1) (imag-part c2))))

  (define (sub-complex c1 c2)
    (make-from-real-mag (- (real-part c1) (real-part c2))
                        (- (imag-part c1) (imag-part c2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ; interface to rest of the system
  
  (define (tag z) (attach-tag 'complex z))

  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))

  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))

  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))

  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'equ '(complex complex) (lambda (x y) (tag (and (= (real-part x) (real-part y))
                                                       (= (imag-part x) (imag-part y))))))

  (put '=zero? 'complex (lambda (x) (= (real-part x) (imag-part x) 0)))

  'done
  )


;; users

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))



;; Exercise 2.79

(define (eq? n1 n2) (apply-generic 'equ n1 n2))

;; Exercise 2.80

(define (=zero? x) (apply-generic '=zero? x))







