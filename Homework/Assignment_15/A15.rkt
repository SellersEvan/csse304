#lang racket
(require racket/trace)
(require "../chez-init.rkt")
(provide set-of-cps make-k apply-k 1st-cps map-cps make-cps domain-cps member?-cps andmap-cps free-vars-cps continuation? init-k list-k union-cps remove-cps memoize subst-leftmost)


(define set-of ; removes duplicates to make a set
  (lambda (s)
    (cond [(null? s) '()]
      [(member (car s) (cdr s))
        (set-of (cdr s))]
      [else (cons (car s)
        (set-of (cdr s)))])))


(define set-of-cps
  (lambda (ls k)
    (if (null? ls)
      (apply-k k ls)
      (member?-cps (car ls) (cdr ls)
            (make-k (lambda (isMember)
              (if isMember
                (set-of-cps (cdr ls) k)
                (set-of-cps (cdr ls) (make-k (lambda (rls) (apply-k k (cons (car ls) rls))))))))))))


(define make-k
  (lambda (a)
    a))


(define apply-k
  (lambda (k ls)
    (k ls)))


(define 1st-cps
  (lambda (ls k)
    (apply-k k (car ls))))


(define map-cps
  (lambda (proc ls k)
    (if (null? ls)
      (apply-k k ls)
      (proc (car ls) (make-k (lambda (res) (map-cps proc (cdr ls) (make-k (lambda (rls) (apply-k k (cons res rls)))))))))))


(define make-cps
  (lambda (proc)
    (lambda (res k)
      (apply-k k (proc res)))))


(define domain-cps
  (lambda (ls k)
    (map-cps 1st-cps ls (make-k (lambda (o) (set-of-cps o k))))))


(define member?-cps
  (lambda (el ls k)
    (cond [(null? ls) (apply-k k #f)]
          [(equal? el (car ls)) (apply-k k #t)]
          [else (member?-cps el (cdr ls) k)])))


(define andmap-cps
  (lambda (proc ls k)
    (if (null? ls)
      (apply-k k #t)
      (proc (car ls) (make-k (lambda (res) (if (not res)
                                               (apply-k k #f)
                                               (andmap-cps proc (cdr ls) k))))))))
                                               

(define free-vars-cps
  (lambda (a b)
    (nyi)))

(define-datatype continuation continuation? 
[init-k] 
[list-k])

(define union-cps
  (lambda (a b c)
    (nyi)))

(define remove-cps
  (lambda (a b c)
    (nyi)))

(define memoize
  (lambda (a b c)
    (nyi)))

(define subst-leftmost
  (lambda (a b c d)
    (nyi)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))

(and)