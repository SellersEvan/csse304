#lang racket

(provide intersection subset? relation? domain reflexive? multi-set? ms-size last all-but-last)

(define contains?
  (lambda (list a)
    (cond [(equal? (index-of list a) false) false]
          [else true])))

(define union
  (lambda (a b)
    (cond [(not (equal? (length b) 0)) (union (append a b) '())]
          [(equal? (length a) 0) '()]
          [(contains? (cdr a) (car a)) (union (cdr a) b)]
          [else (cons (car a) (union (cdr a) b))])))

(define remove-duplicates
  (lambda (a)
    (union a '())))

(define intersection
  (lambda (a b)
    (cond [(equal? (length a) 0) '()]
          [(not (contains? b (car a))) (intersection (cdr a) b)]
          [else (cons (car a) (intersection (cdr a) b))])))

(define subset?
  (lambda (a b)
    (cond [(equal? (length a) 0) true]
          [(not (contains? b (car a))) false]
          [else (subset? (cdr a) b)])))

(define set?
  (lambda (a)
    (cond [(not (list? a)) false]
          [(<= (length a) 1) true]
          [else (and (not (contains? (cdr a) (car a))) (set? (cdr a)))])))

(define relation-help?
  (lambda (a keys)
    (cond [(not (set? a)) false]
          [(equal? 0 (length a)) true]
          [(not (list? (car a))) false]
          [(not (equal? 2 (length (car a)))) false]
          [(equal? (car (car a)) (cadr (car a))) (relation-help? (cdr a) keys)]
          [(contains? keys (car (car a))) false]
          [else (relation-help? (cdr a) (cons (car (car a)) keys))])))

(define relation?
  (lambda (a)
    (relation-help? a '())))

(define domain
  (lambda (a)
    (remove-duplicates (map (lambda (elm) (car elm)) a))))


(define reflexive-vectors
  (lambda (a)
    (cond [(equal? 0 (length a)) '()]
          [(equal? (car (car a)) (cadr (car a))) (cons (car (car a)) (reflexive-vectors (cdr a)))]
          [else (reflexive-vectors (cdr a))])))

(define invert-vectors
  (lambda (a)
    (map (lambda (i)
          (list (cadr i) (car i))) a)))

; a = 2 + 4
; b = plot(a)

; plot(add(2,4))

(define reflexive?
  (lambda (a)
    (subset? (remove-duplicates (append (domain (invert-vectors a)) (domain a))) (remove-duplicates (reflexive-vectors a)))))


(define multi-set-helper?
  (lambda (a keys)
    (cond [(not (list? a)) false]
          [(equal? (length a) 0) true]
          [(not (list? (car a))) false]
          [(not (equal? (length (car a)) 2)) false]
          [(not (integer? (cadr (car a)))) false]
          [(integer? (car (car a))) false]
          [(< (cadr (car a)) 1) false]
          [(contains? keys (car (car a))) false]
          [else (multi-set-helper? (cdr a) (cons (car (car a)) keys))])))


(define multi-set?
  (lambda (a)
    (multi-set-helper? a '())))

(define ms-size
  (lambda (a)
    (apply + (map (lambda (i)
          (cadr i)) a))))

(define last
  (lambda (a)
    (cond [(equal? 1 (length a)) (car a)]
          [else (last (cdr a))])))


(define all-but-last
  (lambda (a)
    (cond [(equal? 1 (length a)) '()]
          [else (cons (car a) (all-but-last (cdr a)))])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
