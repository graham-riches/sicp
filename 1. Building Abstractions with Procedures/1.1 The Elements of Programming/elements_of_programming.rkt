#lang racket

;-----------------------------------------------------------------------------------------------------
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

;-----------------------------------------------------------------------------------------------------
; Exercise 1.2 -> answer should be: -0.2466 or -37/150 :)
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) 
   (* 3 (- 6 2) (- 2 7)))

;-----------------------------------------------------------------------------------------------------
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

;-----------------------------------------------------------------------------------------------------
; Exercise 1.4 -> if just changes the +/- procedure that is called
(define (a-plus-abs-b a b)
    ((if (> b 0) + -) a b))

(a-plus-abs-b 1 -2)
(a-plus-abs-b 1 4)

;-----------------------------------------------------------------------------------------------------
; Exercise 1.5 -> applicative vs normal order evaluation and infinite recursion
(define (p) (p))

(define (test x y)
    (if (= x 0) 0 y))

#|
    (test 0 (p)) -> applicative order never terminates
    normal order will evaluate as all arguments are expanded before evaluation which then short 
    circuits the infinite (P) loop
|#

;-----------------------------------------------------------------------------------------------------
; Exercise 1.6 -> square roots
(define (sqrt-iter guess x)
    (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
                x)))              

(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
    (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause)))

#|
(define (sqrt-iter guess x)
    (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
        x)))

    ^^ the above function never teminates because in applicative order evaluation the expression 
    doesn't naturally short-circuit like the special case of if does!! Very interesting :)
|#

;-----------------------------------------------------------------------------------------------------
; Exercise 1.7 -> improving square roots

#|
    don't really need anything special to show that the above predicate for terminating the square
    root calculation will fall on it's face if the square root is less than the hard-coded value in
    the predicate :D
|#

(define (better-good-enough? guess improved_guess)
    (< (abs (- 1.0 (/ guess improved_guess))) 0.001))

(define (better-sqrt-iter guess x)
    (if (better-good-enough? guess (improve guess x))
    guess
    (better-sqrt-iter (improve guess x) x)))

(define (better-sqrt x)
    (better-sqrt-iter 1.0 x))

#|
    this works much better for smaller numbers and for very large numbers as the other sqrt function
    enters an infinite loop and never terminates!
|#

;-----------------------------------------------------------------------------------------------------
; Exercise 1.8 -> Cube roots

(define (cube-root-iter guess x)
    (if (better-good-enough? guess (improve-cube guess x))
    guess
    (cube-root-iter (improve-cube guess x) x)))

(define (improve-cube guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3)
)

(define (cube-root x)
    (cube-root-iter 1.0 x))