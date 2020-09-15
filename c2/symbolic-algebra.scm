#lang sicp

(#%require "../lib.scm")
;; (#%require "generic-number-system.scm")



;; ---------------------------------------





(define (install-polynomial-package)
  ;; INTERNAL REPRESENTATIONS
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ;; INTERNAL PROCEDURES

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) (term-list p2)))
      (error "Polynomials not in same variable")))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polynomials not in same variable")))


  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term (make-term (order t1) (add (coeff t1) (coeff t2)))
                                  (add-terms (rest-terms L1) (rest-terms L2))))
                   )))
          ))


  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) (l2)))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))


  (define (adjoin-term t term-list)
    (if (=zero? (coeff t))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())

  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? tl) (null? tl))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (zero-poly poly) (empty-termlist? (term-list poly)))

  (define (negate-poly p) (make-poly (variable p)
                                     (map negate-term (term-list p))))

  (define (negate-term t) (make-term (order t)
                                     (negate (coeff t))))



  ;; EXTERNAL INTERFACE

  (define (tag p) (attach-tag 'polynomial p))

  (put 'add '(polynomial polynomial) (lambda (p1 p2) (add-poly p1 p2)))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (mul-poly p1 p2)))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? 'polynomial zero-poly)
  (put 'negate 'polynomial negate-poly)
  (put 'sub 'polynomial (lambda (p1 p2) (add-poly p1 (negate-poly p2))))

  'DONE
  )

;; Exercise 2.90

(define (first-term term-list)
  (make-term (- (len term-list) 1) (car term-list)))

(define (adjoin-term term term-list)
  (cond ((=zero? term) term-list)
        ((=equ? (order term) (length term-list)) (cons (coeff term) term-list))
        (else (adjoin-term term (cons 0 term-list)))))


;; Exercise 2.91

(define (div-terms L1 L2)
  (if (empty-termlist? L1))
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t1) (order t2))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result (div-terms (sub-terms L1
                                                          (mul-term-by-all-terms
                                                           (make-term new-o new-c)
                                                           L2))
                                               L2))
                (list (adjoin-term (make-term new-o new-c) (car rest-of-result))
                      (cadr result-of-result))))))))

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (let ((term-div-result (div-terms (term-list p1) (term-list p2))))
        (let ((quotient-term-list (car term-div-result))
              (remainder-term-list (cadr term-div-result)))
          (list (make-poly (variable p1) quotient-term-list)
                (make-poly (variable p1) remainder-term-list))))
      (error "Poly not in same variable")))



  

















