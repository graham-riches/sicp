#lang racket

;-----------------------------------------------------------------------------------------------------
; Exercise 2.1 -> Make Rational Numbers

(define (sgn x)
    (cond ((< x 0) -1)
          (else 1)))

(define (make-rat n d)
  (cons (* (sgn (* n d)) (abs n)) (abs d)))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;-----------------------------------------------------------------------------------------------------
; Exercise 2.2 -> Midpoint of a Line Segment

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s)
  (define (avg x y)
    (/ (+ x y) 2))
  (make-point (avg (x-point (start-segment s)) (x-point (end-segment s)))
               (avg (y-point (start-segment s)) (y-point (end-segment s)))))

;-----------------------------------------------------------------------------------------------------
; Exercise 2.3 -> Rectangles and Squares

(define (make-rectangle p1 length width)
  (define (make-dimensions l w) (cons l w))
  (cons p1 (make-dimensions length width)))

(define (get-width r)
  (cdr (cdr r)))

(define (get-length r)
  (car (cdr r)))

(define (get-area r)
  (* (get-width r) (get-length r)))

(define (get-perimeter r)
  (+ (* 2 (get-width r))
     (* 2 (get-length r))))


(define (make-square p1 size)
  (make-rectangle p1 size size))

;-----------------------------------------------------------------------------------------------------
; Exercise 2.4 -> Alternative Representation of Pairs
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))


(define (validate-pair-construction p expected)
  (cond ((= (car p) expected) #t)
        (else #f)))
                            
(validate-pair-construction (cons 1 2) 1)


;-----------------------------------------------------------------------------------------------------
; Exercise 2.5 -> Pairs of Non-Negative Numbers as a Combined Integer
(define (power base exp)
  (define (power-recursive tally count)
    (cond ((= exp count) tally)
          (else (power-recursive (* tally base) (+ count 1)))))
  (cond ((= exp 0) 1)
        (else (power-recursive base 1))))

; note: the following assumes that base is a Nth power of root
(define (get-exponent base root)
  (define (get-exp value count)
    (cond ((= value root) count)
          (else (get-exp (/ value root) (+ count 1)))))
  (get-exp base 1))


(define (cons-int a b)
  (* (power 2 a) (power 3 b)))

(define (car-int p)
  (define (car-fact value count)
    (cond ((= 0 (remainder value 2)) (car-fact (/ value 2) (+ count 1)))
          (else (- count 1))))
  (car-fact p 1))

(define (cdr-int p)
  (define (car-fact value count)
    (cond ((= 0 (remainder value 3)) (car-fact (/ value 3) (+ count 1)))
          (else (- count 1))))
  (car-fact p 1))
  
;-----------------------------------------------------------------------------------------------------
; Exercise 2.6 -> Pairs of Non-Negative Numbers as a Combined Integer

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; evaluate via substitution (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x))))) --> ((zero f) x) == x
; (lambda (f) (lambda (x) (f x)))

; evaluate via substitution (add-1 one)
; (lambda (f) (lambda (x) (f ((one f) x)))))
; (lambda (f) (lambda (x) (f (((lambda (a) (lambda (b) (a b))) f) x)))) --> note repeated pattern of lambda (x) (f x)
; (lambda (f) (lambda (x) (f ((lambda (b) (f b)) x))))
; (lambda (f) (lambda (x) (f (f x))))

; addition is multiple applications of the nested lamdba as many times as required:
; i.e. three is -> (lambda (f) (lambda (x) (f (f (f x)))))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))





