#lang racket

; num-positive - returns the number of positive elements in a list
; implement this in a tail recursive way, similar to fact2
;
; (num-positive '(1 -2 0 100 77)) -> 3

(define num-positive-helper
  (lambda (lon count)
    (cond [(null? lon) count]
          [(equal? 0 (length lon)) count]
          [(< 0 (car lon)) (num-positive-helper (cdr lon) (add1 count))]
          [else (num-positive-helper (cdr lon) count)])))


(define num-positive
  (lambda (lon)
    (num-positive-helper lon 0)))

; you'll want a helper function
;(define num-positive-recur
;  ????)


; second largest - returns the second largest element in a list of numbers
;
; (second-largest '( 7 4 5 3 6 2 1)) -> 6
;
; you can assume the list has 2 elements
; implement this in a tail recursive way with a helper function

(define second-largest
  (lambda (lon)
    'nyi))
