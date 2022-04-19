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

;-----------------------------------------------------------------------------------------------------
; Exercise 1.30 -> linear iterative sum

(define (sum-linear term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter (next a) (term a)))

(define (sum-cube-linear a b)
  (sum-linear cube a inc b))

(sum-cube-linear 1 10)

;-----------------------------------------------------------------------------------------------------
; Exercise 1.31 -> product --> NOTE: not doing the recursive part because it's a copy paste

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter (next a) (term a)))

(define (identity x) x)

(define (factorial b)
  (product identity 1 inc b))

(factorial 5)

;-----------------------------------------------------------------------------------------------------
; Exercise 1.32 -> accumulate

; Linear iterative version
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
  (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter (next a) null-value))


(define (new-sum term a next b)
  (accumulate + 0 term a next b))

(define (new-product term a next b)
  (accumulate * 1 term a next b))

; Recursive version
; [1 2 3 4 5]
(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a) (accumulate-recursive combiner null-value term (next a) next b))))

(define (new-sum-recursive term a next b)
  (accumulate-recursive + 0 term a next b))

(new-sum-recursive identity 1 inc 5)  ; should print 15

;-----------------------------------------------------------------------------------------------------
; Exercise 1.33 -> filtered accumulate
(define (filtered-accumulate combiner predicate null-value term a next b)
  (define (get-value a)
    (if (predicate (term a)) (term a)
        null-value))
  (define (iter a result)
  (if (> a b)
      result
      (iter (next a) (combiner result (get-value a)))))
  (iter (next a) null-value))

(define (sum-even a b)
  (filtered-accumulate + even? 0 identity a inc b))

(sum-even 1 10)


;-----------------------------------------------------------------------------------------------------
; Exercise 1.34 -> some basic examples with lambdas and lets

(define (new-pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(new-pi-sum 1 10)

(define (square x)
  (* x x))

(define (f1 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(define (f g)
  (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))


; if you expand this out, you get: g(f(2)) -> (f f) -> (f 2) -> (2 2) -> error


;-----------------------------------------------------------------------------------------------------
; Exercise 1.35 -> finding fixed points
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)
(cos 0.7390822985224024)

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

; phi^2 = phi + 1
; or 1 + (1/x) = x -> x^2 = x + 1 -> they are the same function

;-----------------------------------------------------------------------------------------------------
; Exercise 1.36 - more fixed points
; Note: fixed point function modified above

(define (header x)
  (newline)
  (newline)
  (display x)
  (newline))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(header "exercise 1.36 - no damping")
(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)

; notably faster with average damping added in :)

;-----------------------------------------------------------------------------------------------------
; Exercise 1.37 - continued fractions
; bleh need to get my wrapped around recursion better again!

(define (cont-frac n d k)
  (define (iter result term)
    (if ( = term 0)
        result
        (iter (/ (n term) (+ (d term) result)) (- term 1)))
    )
  (iter 0 k))

(header "exercise 1.37 - iterative")
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 5)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 6)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 7)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 8)

(define (cont-frac-recursive n d k)
  (cond ((= k 0) 0)
        (else (/ (n k) (+ (d k) (cont-frac-recursive n d (- k 1)))))))

(header "exercise 1.37 - recursive")
(cont-frac-recursive (lambda (i) 1.0) (lambda (i) 1.0) 8)
                              

;-----------------------------------------------------------------------------------------------------
; Exercise 1.38 - continued fraction approximation to e

(header "exercise - 1.38")

(define (euler-sequence i)
  (cond ((= i 1) 1.0)
        ((= i 2) 2.0)
        ((= (remainder (- i 2) 3) 1) 1.0)
        ((= (remainder (- i 2) 3) 2) 1.0)
        ((= (remainder (- i 2) 3) 0) (+ 2.0 (* (/ (- i 2) 3.0) 2.0)))))


(define (guess-euler n)
  (+ 2 (cont-frac (lambda (i) 1.0) euler-sequence n)))

(guess-euler 10)

;-----------------------------------------------------------------------------------------------------
; Exercise 1.39 - continued fraction approximation to tan
; Note: needed a bit of a hack to remap the cont-frac to use a subtraction, could be abstracted better

(header "exercise - 1.39")

(define (cont-frac-minus n d k)
  (define (iter result term)
    (if ( = term 0)
        result
        (iter (/ (n term) (- (d term) result)) (- term 1)))
    )
  (iter 0 k))

(define (tan-cf x k)
  (cont-frac-minus
   (lambda (i) (cond ((= i 1) x)
                     (else (square x))))
   (lambda (i) (+ 1.0 (* 2.0 (- i 1))))
   k))

(tan-cf (/ 3.1415926 4) 15)  ; should be 1.0 ish :)


;-----------------------------------------------------------------------------------------------------
; Exercise 1.40 - newtons method approximation
(header "exercise - 1.40")

(define dx 0.00001)

(define (derivative g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((derivative g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (newton-sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(newton-sqrt 2)

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x)  c)))

(newtons-method (cubic 1 2 3) 1)

;-----------------------------------------------------------------------------------------------------
; Exercise 1.41 - more on higher order procedures
(header "exercise - 1.41")

(define (double f)
  (lambda (x) (f (f x))))

((double inc) 1)

(((double (double double)) inc) 5)

; expanding this: +2 -> 2*2 = 4 -> 4*4 = 16: 16 + 5 = 21

;-----------------------------------------------------------------------------------------------------
; Exercise 1.42 - function composition
(header "exercise - 1.42")

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

;-----------------------------------------------------------------------------------------------------
; Exercise 1.43 - repeated function application
(header "exercise - 1.43")

(define (repeated f n)
  (define (iter function term)
    (cond ((= term 1) function)
          (else (iter (lambda (x) (f (function x))) (- term 1)))))
  (iter f n))

((repeated inc 5) 5)
((repeated square 2) 5)

;-----------------------------------------------------------------------------------------------------
; Exercise 1.44 - smoothing function

(header "exercise - 1.44")

(define (smooth f)
  (lambda (x) (/ (+ (f (+ x dx)) (f x) (f (- x dx))) 3.0)))

(define (smooth-n n)
  (repeated smooth n))

;-----------------------------------------------------------------------------------------------------
; Exercise 1.45 - fixed point approximations with average damping experimental

; For now, I'm going to skip this exercise as it has not a huge amount of extra value over what's
; already been covered in this chapter and will take a massive amount of time!

;-----------------------------------------------------------------------------------------------------
; Exercise 1.46 - higher order iterative improve

(header "exercise - 1.46")
(define (iterative-improve good-enough? improve)
  (lambda (guess) (if (good-enough? guess) guess
                      ((iterative-improve good-enough? improve) (improve guess)))))

(define (iterative-sqrt x)
  ((iterative-improve (lambda (y) (< (abs (- (square y) x)) 0.001))
                        (lambda (guess) (average guess (/ x guess)))) 1.0))

(iterative-sqrt 2)

(define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

(define (iterative-fixed-point f first-guess)
  ((iterative-improve
    (lambda (x) (close-enough? x (f x)))
    f)
   first-guess))

 (iterative-fixed-point cos 1.0)