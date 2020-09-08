#lang sicp

(#%require "../lib.scm")


(#%provide (all-defined))

;; ---- COMPLEX NUMBERS ----


(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))


(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


;; using type tags for separating rectangular form (x & y) with polar form (magnitude and angle)

(define (attach-tag tag contents)
  (cond ((number? contents) contents)
        ((symbol? contents) contents)
        ((pair? contents) (cons tag contents))
        ))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((symbol? datum) 'symbol)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG " datum))
        ))


(define (contents datum)
  (cond ((number? datum) datum)
        ((symbol? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad datum"))
        ))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))


;; Using Type Tags - Rectangular representation

(define (real-part-rect z) (car z))
(define (imag-part-rect z) (cdr z))

(define (magnitude-rect z)
  (sqrt (+ (square (real-part-rect z))
           (square (imag-part-rect z)))))

(define (angle-rect z)
  (atan (imag-part-rect z) (real-part-rect z)))

(define (make-from-real-imag-rect x y) (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-angle-rect r a)
  (attach-tag 'rectangular
              (cons (* r (cos a))
                    (* r (sin a)))))


;; Using Type Tags - Polar representation

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))


(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (* (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))


;;

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rect (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Type not found"))))


(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rect (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Type not found"))
        ))


(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rect (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Invalid complex number"))
        ))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rect (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        ))



(define (make-from-real-imag x y)
  (make-from-real-imag-rect x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))



;; Data Directed programming

;; In contrast to the above methods which deal explicitly with types themselves
  ;; in Data Directed prog. we work with a "table" of external functions vs types,
    ;; which gives us the function to apply depending on type

;; The key idea of data-directed programming is to handle generic operations in programs
  ;; by dealing explicitly with operation-and-type tables,


;; put :: Operation Name -> type -> Operation -> void (put the operation into the table)
;; get :: Operation Name -> type -> Operation  (get the operation out of the table)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a)) ))

  ;; External interface

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
  'done)


(define (install-polar-package)
;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


;; Message passing style

;; Exercise 2.75

(define (make-from-mag-ang-m r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op"))
          ))
  dispatch)






;;

(define (apply-generic op . args)
  (let ((type-tags) (map type-tag args))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types")))))






