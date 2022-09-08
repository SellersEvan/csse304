#lang racket

(provide minimize-interval-list exists? product replace remove-last)


; (3 7) (5 6)

(define range-overlap?
  (lambda (a b)
    (cond [(and (<= (car a) (car b)) (>= (cadr a) (car b))) true]
          [(and (<= (car a) (cadr b)) (>= (cadr a) (cadr b))) true]
          [else false])))

(define range-overlap
  (lambda (a b)
    (cond [(and (<= (car a) (car b)) (>= (cadr a) (car b))) (list (car a) (cadr b))]
          [(and (<= (car a) (cadr b)) (>= (cadr a) (cadr b))) (list (car b) (cadr a))]
          [else false])))

(define minimize-interval-list
  (lambda (a)
    (let ((a (sort a (lambda (x y) (< (car x) (car y))))))
      (let minimize-interval-list-helper ((a a))
        (cond [(> 2 (length a)) a]
              [(range-overlap? (car a) (cadr a)) (cons (range-overlap (car a) (cadr a)) (cddr a))]
              [else (cons (car a) (cdr a))])))))

; ((1 2) (3 20) (4 7) (5 6) (5 7) (8 9) (11 14))
; ((1 2) (3 20) (4 7) (5 6) (5 7) (8 9) (11 14))

(define exists?
  (lambda (a b)
    (nyi)))

(define product
  (lambda (a b)
    (nyi)))

(define replace
  (lambda (a b c)
    (nyi)))

(define remove-last
  (lambda (a b)
    (nyi)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
