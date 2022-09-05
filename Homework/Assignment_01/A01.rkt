#lang racket

; Evan Sellers - HW 1

(provide interval-contains? interval-intersects? interval-union my-first my-second my-third make-vec-from-points dot-product vector-magnitude distance)

(define interval-contains?
  (lambda (a b)
    (and (<= (car a) b) (>= (cadr a) b))))

(define interval-intersects?
  (lambda (a b)
    [or [or (interval-contains? b (car a)) (interval-contains? b (cadr a))]
        [or (interval-contains? a (car b)) (interval-contains? a (cadr b))]]))

(define interval-union
  (lambda (a b)
    [cond [(interval-intersects? a b) (cons (cons (min (car a) (car b)) (cons (max (cadr a) (cadr b)) '())) '())]
          [else (cons a (cons b '()))]]))

(define my-first
  (lambda (a)
    (car a)))

(define my-second
  (lambda (a)
    (cadr a)))

(define my-third
  (lambda (a)
    (caddr a)))

(define make-vec-from-points
  (lambda (a b)
    [list (- (my-first b) (my-first a) )
          (- (my-second b) (my-second a) )
          (- (my-third b) (my-third a) )]))

(define dot-product
  (lambda (a b)
    (+ [* (my-first b) (my-first a)]
         [* (my-second b) (my-second a)]
         [* (my-third b) (my-third a)])))

(define vector-magnitude
  (lambda (a)
    (sqrt (dot-product a a))))

(define distance
  (lambda (a b)
    (vector-magnitude (make-vec-from-points a b))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
