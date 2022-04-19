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

          