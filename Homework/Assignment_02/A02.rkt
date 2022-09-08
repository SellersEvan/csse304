#lang racket

(provide choose sum-of-squares range my-set? union more-positives? nearest-pair)

(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))


(define choose
  (lambda (a b)
    (/ (fact a) (* (fact b) (fact (- a b))))))


(define sum-of-squares
  (lambda (a)
    (cond [(equal? (length a) '0) '0]
          [else (+ (* (car a) (car a)) (sum-of-squares (cdr a)))])))


(define range
  (lambda (a b)
    (cond [(>= a b) '()]
          [else (cons a (range (+ a 1) b))])))


(define contains?
  (lambda (list a)
    (cond [(equal? (index-of list a) false) false]
          [else true])))


(define my-set?
  (lambda (a)
    (cond [(<= (length a) 1) true]
          [else (and (not (contains? (cdr a) (car a))) (my-set? (cdr a)))])))

;(define my-set?
;  (lambda (a)
;    (cond [(equal? 0 (length a)) true]
;          [(equal? 1 (length a)) true]
;          [(equal? (car a) (cadr a)) false]
;          [(equal? 2 (length a)) true]
;          [else (and (my-set? (cdr a)) (my-set? (cons (car a) (cddr a))))])))


(define union
  (lambda (a b)
    (cond [(not (equal? (length b) 0)) (union (append a b) '())]
          [(equal? (length a) 0) '()]
          [(contains? (cdr a) (car a)) (union (cdr a) b)]
          [else (cons (car a) (union (cdr a) b))])))

    
(define more-positives?
  (lambda (lon)
    (cond [(< 0 (apply + (map (lambda (i)
          (cond [(> i 0) 1]
                [else -1])) lon))) true]
          [else false])))


(define delta-pair
  (lambda (lon) (- (cdr lon) (car lon))))


(define chunk-list-pair
  (lambda (lon) (cond [(equal? 2 (length lon)) (list (cons (car lon) (cadr lon)))]
                      [else (cons (cons (car lon) (cadr lon)) (chunk-list-pair (cdr lon)))])))

(define nearest-pair
  (lambda (lon)
    (car (sort (chunk-list-pair (sort lon <))
      (lambda (a b) (< (delta-pair a) (delta-pair b)))))))
           

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
