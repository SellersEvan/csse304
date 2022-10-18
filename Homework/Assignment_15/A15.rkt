#lang racket
(require racket/trace)
(require "../chez-init.rkt")
(provide set-of-cps make-k apply-k 1st-cps map-cps make-cps domain-cps member?-cps andmap-cps free-vars-cps continuation? init-k list-k union-cps remove-cps memoize subst-leftmost)

; Question for Part 3
; The savings in time is less drastic for memoize-fib because we are using memoize to build a generalized
; memoization functions. While the fib function from the powerpoint is custom build, all responses are
; saved, this includes the sub calls to fib. The generalized one we built memoize-fib, only saves the
; top level call responses made against memoize-fib.


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
                                               

(define any? (lambda (o) #t))


(define-datatype continuation continuation? 
[init-k] 
[list-k]
[proc-k (proc procedure?) (k continuation?)]
[append-set-k (elm any?) (k continuation?)]
[union-k      (ls list?) (k continuation?)]
[add-k        (elm any?) (k continuation?)]
[remove-k     (elm any?) (k continuation?)]
[free-vars-k  (exp any?) (k continuation?)])


(define apply-k-ds
  (lambda (k res)
	  (cases continuation k
      [init-k () res]
      [list-k () (list res)]
      [proc-k (proc k) (apply-k-ds k (proc res))]
      [append-set-k (elm k)
        (memq-cps elm res (proc-k (lambda (isMember) (if isMember res (cons elm res))) k))]
      [union-k (ls k) (union-cps ls res k)]
      [add-k (elm k) (apply-k-ds k (cons elm res))]
      [remove-k (elm k)
        (cond [(or (null? res) (empty? res)) (apply-k-ds k res)]
              [(equal? elm (car res)) (apply-k-ds k (cdr res))]
              [else (apply-k-ds (remove-k elm (add-k (car res) k)) (cdr res))])]
      [free-vars-k (exp k)
        (cond [(or (null? exp) (empty? exp)) (apply-k-ds k res)]
              [(symbol? exp) (apply-k-ds k (list exp))]
              [(eq? (car exp) 'lambda) (apply-k-ds (free-vars-k (3rd-cps exp (init-k)) (remove-k (car (2nd-cps exp (init-k))) k)) res)]             
              [else (apply-k-ds (free-vars-k (car exp) (union-k res (free-vars-k (cdr exp) k))) res)])])))


(define 2nd-cps
  (lambda (ls k)
    (apply-k-ds (proc-k cdr (proc-k car k)) ls)))


(define 3rd-cps
  (lambda (ls k)
    (apply-k-ds (proc-k cdr (proc-k cdr (proc-k car k))) ls)))


(define memq-cps
  (lambda (sym ls k)
    (cond [(null? ls) (apply-k-ds k #f)]
          [(eq? (car ls) sym) (apply-k-ds k #t)]
          [else (memq-cps sym (cdr ls) k)])))


(define union-cps
  (lambda (ls1 ls2 k)
    (if (or (null? ls1) (empty? ls1))
        (apply-k-ds k ls2)
        (union-cps (cdr ls1) ls2 (append-set-k (car ls1) k))))) 


(define remove-cps
  (lambda (elm ls k)
    (apply-k-ds (remove-k elm k) ls)))


(define free-vars-cps
  (lambda (exp k)
    (apply-k-ds (free-vars-k exp k) '())))


(define memoize
  (lambda (func hash equiv?)
    (let ([memhash (make-custom-hash equiv? hash)])
      (lambda args
        (if (dict-has-key? memhash args)
          (dict-ref memhash args)
          (let ([res (apply func args)])
            (dict-set! memhash args res)
            res))))))


(define-syntax with-values
  (syntax-rules ()
    [(_ expr consumer)
      (call-with-values
        (lambda () expr)
        consumer)]))

(define-syntax mvlet
  (syntax-rules ()
    ((_ ((x ...) e0) e1 e2 ...)
      (with-values e0
        (lambda (x ...) e1 e2 ...)))))


(define subst-leftmost
  (lambda (replace pattern ls predicate)
    (with-values
      (let subst-leftmost-helper ([replace replace] [pattern pattern] [ls ls] [predicate predicate])
        (cond [(symbol? ls)
                (if (predicate pattern ls)
                    (values replace #t)
                    (values ls #f))]
              [(or (null? ls) (empty? ls)) (values ls #f)]
              [else (mvlet ((val hasMatched) (subst-leftmost-helper replace pattern (car ls) predicate))
                (if hasMatched
                  (values (cons val (cdr ls)) #t)
                  (mvlet ((rval rhasMatched) (subst-leftmost-helper replace pattern (cdr ls) predicate))
                    (values (cons val rval) rhasMatched))))]))
      (lambda (x y) x))))



;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))

