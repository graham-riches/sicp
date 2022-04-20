#lang racket
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length-iter items)
  (define (length-iter-helper a count)
    (if (null? a)
        count
        (length-iter-helper (cdr a) (+ 1 count))))
  (length-iter-helper items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;--------------------------------------------------------------
; Ex. 2.17
(define (last-pair items)
  (let ((tail (cdr items)))
    (if (null? tail)
        items
        (last-pair tail))))

;--------------------------------------------------------------
; Ex. 2.18
(define (reverse items)
  (define (reverse-helper lst rem)
    (let ((tail (cdr rem))
          (head (car rem)))
      (if (null? tail)
          (cons head lst)
          (reverse-helper (cons head lst) tail))))
  (reverse-helper null items))

;--------------------------------------------------------------
; Ex. 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (except-first-denomination coins)
  (cdr coins))

(define (first-denomination coins)
  (car coins))

(define (no-more? coins)
  (if (null? coins)
      true
      false))


(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

; Order of coins does not matter because we are calculating all possible values in both cases and the operations are commutative

;--------------------------------------------------------------
; Ex. 2.20

(define (same-parity first . rest)
    (define (same-parity-helper even lst rem)
      (if (null? rem)
          lst          
          (if (equal? (even? (car rem)) even)
              (same-parity-helper even (cons (car rem) lst) (cdr rem))
              (same-parity-helper even lst (cdr rem)))))
  (reverse (same-parity-helper (even? first) null (cons first rest))))

(same-parity 1 2 3 4 5)
(same-parity 2 3 4 5 6 7 8)

;--------------------------------------------------------------
(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

;--------------------------------------------------------------
; Ex. 2.21
(define (square-list items)
  (if (null? items)
      null
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list-alt items)
  (map (lambda (x) (* x x)) items))

(square-list (list 1 2 3))
(square-list-alt (list 1 2 3))

;--------------------------------------------------------------
; Ex. 2.22
; The order is reversed because he is applying the operation to the first element in the list and
; creating the new list based on that while iterating over the elements in the original list. As
; cons does a prepend, this reverses the list.
; The recursive approach prepends the result of applying the expression to the first item of the list
; to the result of having already applied the procedure to all other items, and is therefore not reversed
; The answer is a list, so he prepends a list onto a list, which results in (() x y z)

;--------------------------------------------------------------
; Ex. 2.23
; this mostly works but returns a list of results (not really required)
(define (for-each proc items)
  (map (lambda (x) (proc x)) items))

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))

;--------------------------------------------------------------
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 2 4)))
(count-leaves x)

;--------------------------------------------------------------
; Ex. 2.24
(list 1 (list 2 (list 3 4)))
; (1 (2 (3 4)))
;
;  [1 | ]
;      |
;     [2 | ]
;         |
;        [3 | ] -> [4 | \]
;
; (Basically just a right-side heavy tree)

;--------------------------------------------------------------
; Ex. 2.25
(define tst1 (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr tst1)))))

(define tst2 (list (list 7)))
(car (car tst2))

(define tst3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(last (flatten tst3))  ; This is so cheating :P

;--------------------------------------------------------------
; Ex. 2.26
(define x1 (list 1 2 3))
(define y1 (list 4 5 6))

(append x1 y1) ; (1 2 3 4 5 6)
(cons x1 y1) ; ((1 2 3) 4 5 6)
(list x1 y1) ; ((1 2 3) (4 5 6))

;--------------------------------------------------------------
; Ex. 2.27

