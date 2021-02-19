#lang racket

; Exercise 1.1
10

(+ 5 4 3)

(- 9 1)

(/ 6 2)

(+ (* 2 4)(- 4 6))

(define a 3)

(define b (+ a 1))

(+ a b (* a b))

(= a b)

(if (and (> b a) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))


; Exercise 1.2 -> answer should be: -0.2466 or -37/150 :)
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) 
   (* 3 (- 6 2) (- 2 7)))


; Exercise 1.3 -> sum of squares of two larger numbers of a triple
(define (square a) (* a a))
(define (square_sum a b)(+ (square a) (square b)))


(define (sum_square_of_larger_values a b c)
        (cond ((and (>= (+ a b) (+ b c)) (>= (+ a b) (+ a c))) (square_sum a b))
              ((and (>= (+ a c) (+ b c)) (>= (+ a c) (+ b c))) (square_sum a c))
              (else (square_sum b c))
        )
)

(sum_square_of_larger_values 1 2 3)
(sum_square_of_larger_values 1 1 1)
(sum_square_of_larger_values 1 2 2)
(sum_square_of_larger_values 1 1 2)
(sum_square_of_larger_values 1 4 3)


; Exercise 1.4 -> if just changes the +/- procedure that is called
(define (a-plus-abs-b a b)
    ((if (> b 0) + -) a b))

(a-plus-abs-b 1 -2)
(a-plus-abs-b 1 4)

; Exercise 1.5 
(define (p) (p))

(define (test x y)
    (if (= x 0) 0 y))

;(test 0 (p)) -> applicative order never terminates
; normal order will evaluate as all arguments are expanded before evaluation which then short circuits
; the infinite (P) loop