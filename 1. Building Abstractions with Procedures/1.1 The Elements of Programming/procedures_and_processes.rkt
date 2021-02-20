#lang racket

;-----------------------------------------------------------------------------------------------------
; Exercise 1.9

(define (inc a)
    (+ a 1))

(define (dec a)
    (- a 1))

(define (add1 a b)
    (if (= a 0)
        b
        (inc (add1 (dec a) b)))
)

(define (add2 a b)
    (if (= a 0)
        b
        (add2 (dec a) (inc b)))
)

#|
    (add1 4 5) --> ignoring the if statement as it's mainly noise in this context
    ((add1 (3) 5) + 1)
    ((add1 (2) 5) + 1 + 1)
    ((add1 (1) 5) + 1 + 1 + 1)
    ((add1 (0) 5) + 1 + 1 + 1 + 1)
    (5 + 1 + 1 + 1 + 1)
    9


    (add2 4 5)
    (add2 (3) (6))
    (add2 (2) (7))
    (add2 (1) (8))
    (add2 (0) (9))
    9

    First one is linearly recursive and the second one is linearly iterative
|# 


;-----------------------------------------------------------------------------------------------------
; Exercise 1.10

(define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1) (A x (- y 1))))
    )
)

#|
    (A 1 10)
    (A 0 (A 1 9))
    (A 0 (A 0 (A 1 8)))
    ...
    (A 0 (A 0 (... A 0 2)))
    expands to 2^10 = 1024


    (A 2 4)
    (A 1 (A 2 3))
    (A 1 (A 1 (A 2 2)))
    (A 1 (A 1 (A 1 (A 2 1))))
    (A 1 (A 1 (A 1 2)))
    (A 1 (A 1 (A 0 (A 1 1))))
    (A 1 (A 1 (A 0 2)))
    (A 1 (A 1 4)) -> from above we know that A 1 x -> 2^x
    (A 1 16)
    2^16 -> 65536


    (A 3 3)
    (A 2 (A 3 2))
    (A 2 (A 2 (A 3 1)))
    (A 2 (A 2 2))
    (A 2 (A 1 (A 2 1)))
    (A 2 (A 1 2))
    (A 2 4) -> from above we already have the answer to this
    2^16 = 655536
|#


(A 1 10)
(A 2 4)
(A 3 3)


(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))


#|
    Give precise mathematical definitions of the above.

    f(n) = 2n
    g(n) = 2^n
    h(n) = 2 ^ (2 ^ n)
|#

(h 5)