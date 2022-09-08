#lang racket

; Evan Sellers

(provide matrix-ref matrix? matrix-transpose filter-in invert pascal-triangle)

(define matrix-ref
  (lambda (matrix row col)
    (cond [(and (equal? row 0) (equal? col 0)) (car (car matrix))]
          [(not (equal? row 0)) (matrix-ref (cdr matrix) (- row 1) col)]
          [(not (equal? col 0)) (matrix-ref (list (cdr (car matrix))) row (- col 1))])))


(define non-empty-num-list?
  (lambda (a)
    (cond [(not (list? a)) false]
          [(equal? 0 (length a)) false]
          [(not (number? (car a))) false]
          [(equal? 1 (length a)) true]
          [else (non-empty-num-list? (cdr a))])))


(define matrix?
  (lambda (matrix)
    (cond [(not (list? matrix)) false]
          [(equal? 0 (length matrix)) true]
          [(not (non-empty-num-list? (car matrix))) false]
          [(equal? 1 (length matrix)) true]
          [(not (non-empty-num-list? (cadr matrix))) false]
          [(not (equal? (length (car matrix)) (length (cadr matrix)))) false]
          [else (matrix? (cdr matrix))])))


(define matrix-transpose
  (lambda (a)
    (apply map list a)))


(define filter-in
  (lambda (a b)
    (cond [(equal? 0 (length b)) '()]
          [(not (a (car b))) (filter-in a (cdr b))]
          [else (cons (car b) (filter-in a (cdr b)))])))

(define invert
  (lambda (a)
    (map reverse a)))

(define pascal-triangle-next-row-helper
  (lambda (row)
    (cond [(> 2 (length row)) '(1)]
          [else (cons (+ (car row) (cadr row)) (pascal-triangle-next-row-helper (cdr row)))])))

(define pascal-triangle-next-row
  (lambda (row)
    (cons '1 (pascal-triangle-next-row-helper row))))

(define pascal-triangle-helper
  (lambda (a b)
    (cond [(equal? (add1 a) (length b)) b]
          [else (pascal-triangle-helper a (cons (pascal-triangle-next-row (car b)) b))])))

(define pascal-triangle
  (lambda (a)
    (cond [(> 0 a) '()]
          [else (pascal-triangle-helper a '((1)))])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
