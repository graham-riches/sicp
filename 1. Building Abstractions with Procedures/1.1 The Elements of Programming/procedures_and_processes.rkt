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
    h(n) = 2 ^ (2 ^ ... n - 1 times)
|#

(h 3)
(h 4)

;-----------------------------------------------------------------------------------------------------
; Coin Change (this is a project euler problem I need to finish anyways :P)
; Still have a very hard time seeing how to come up with a solution like this! It's quite a challenge
(define (count-change amount)
    (cc amount 8))

(define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((< amount 0) 0)
          ((= kinds-of-coins 0) 0)
          (else (+ (cc amount 
                        (- kinds-of-coins 1))
                   (cc (- amount (first-denomination kinds-of-coins))
                       kinds-of-coins)
          ))
    )
)

(define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 2)
          ((= kinds-of-coins 3) 5)
          ((= kinds-of-coins 4) 10)
          ((= kinds-of-coins 5) 20)
          ((= kinds-of-coins 6) 50)
          ((= kinds-of-coins 7) 100)
          ((= kinds-of-coins 8) 200)
    )
)

;-----------------------------------------------------------------------------------------------------
; Exercise 1.11

(define (f_recursive n)
    (cond ((< n 3) n)
          (else (+ 
                    (f_recursive (- n 1)) 
                    (* 2 (f_recursive (- n 2))) 
                    (* 3 (f_recursive (- n 3)))
                ))
    )
)

(define (f_iter n)
    (define (f a b c count)
        (cond ((< n 3) n)
              ((<= count 0) a)
              (else (f (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
    (f 2 1 0 (- n 2)))

#|
    (f_iter 3)
    (f 2 1 0 1)
    (f 4 2 1 0)
    4

    (f_iter 5)
    (f 2 1 0 3)
    (f 4 2 1 2)
    (f 11 4 2 1)
    (f 25 11 4 0)
    25
|#

;-----------------------------------------------------------------------------------------------------
; Exercise 1.12

#|
    1
   1 1
  1 2 1
 1 3 3 1 
1 4 6 4 1
|#


(define (pascals row column)
    (cond ((= 0 row) 0)
          ((< column 0) 0)
          ((> column row) 0)  
          ((= row column) 1)
          ((= column 1) 1)
          (else (+ (pascals (- row 1)(- column 1))
                   (pascals (- row 1) column)))
    )
)

