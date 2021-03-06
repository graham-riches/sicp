#lang racket

;-----------------------------------------------------------------------------------------------------
; Exercise 1.29 -> Simpson Rule Integration

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (cube x)
  (* x x x))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.01)

(define (round-to-even n)
  (+ n (remainder n 2)))

(define (simpson-integral f a b n)
  (define rounded-n (round-to-even n))
  (define (yk k) (f (+ a (* k h))))
  (define (term k)
    (* (cond ((= k 0) 1)
             ((= k 1) 1)
             ((odd? k) 4)
             (else 2))
       (yk k)))
  (define h (/ (- b a) n))
  (* (/ h 3.0)(sum term 0 inc rounded-n)))


(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)
; NOTE: without the 3.0, the integrals are rationals :D
