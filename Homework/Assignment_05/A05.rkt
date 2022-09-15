#lang racket
; Evan Sellers
(provide minimize-interval-list exists? product replace remove-last)


(define range-overlap?
  (lambda (a b)
    (cond [(and (<= (car a) (car b)) (>= (cadr a) (car b))) true]
          [(and (<= (car a) (cadr b)) (>= (cadr a) (cadr b))) true]
          [else false])))


(define range-overlap
  (lambda (a b)
    (cond [(range-overlap? a b) (list (min (car a) (car b)) (max (cadr a) (cadr b)))]
          [else '()])))


(define compare-point?
  (lambda (x y)
    (cond [(equal? (car x) (car y)) (< (cadr x) (cadr y))]
          [else (< (car x) (car y))])))


(define minimize-interval-list
   (lambda (points)
     (let ((sorted-points (sort points compare-point?)))
       (let minimize-interval-list-helper ((points sorted-points))
         (cond [(> 2 (length points)) points]
               [(range-overlap? (car points) (cadr points)) (minimize-interval-list-helper (cons (range-overlap (car points) (cadr points)) (cddr points)))]
               [else (cons (car points) (minimize-interval-list-helper (cdr points)))])))))


(define exists?
  (lambda (a b)
    (< 0 (length (filter a b)))))


(define product
  (lambda (a b)
    (cond [(not (or (equal? 0 (length a)) (equal? 0 (length b)))) (cons (list (car a) (car b)) (append (product (cdr a) b) (product a (cdr b))))]
          [else '()])))


(define replace
  (lambda (pattern string list)
    (map (lambda (elm) (cond [(equal? elm pattern) string]
                             [else elm])) list)))


(define remove-last
  (lambda (pattern list)
    (cond [(equal? 0 (length list)) '()]
          [(and (equal? pattern (car list)) (equal? false (index-of (cdr list) pattern))) (cdr list)]
          [else (cons (car list) (remove-last pattern (cdr list)))])))


;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
