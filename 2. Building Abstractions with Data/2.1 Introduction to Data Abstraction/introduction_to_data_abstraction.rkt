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
; Exercise 2.3 -> Rectangls and Squares

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
