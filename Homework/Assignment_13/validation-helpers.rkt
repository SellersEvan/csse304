#lang racket
(require "../chez-init.rkt")
(provide exp-procedure? exp-vector-set exp-vector-ref exp-make-vector min-list-length? valid-exp-params? true? any? lambda-params? let-params? literal?)


; Validate a list of Symbols => a, (a), (a b), (a b c), (a b . c)
(define lambda-params?
  (lambda (o)
    (or (symbol? o) (empty? o) (and (pair? o) ((list-of? symbol?) (flatten o))))))


; Let Paramaters
(define let-params?
  (lambda (validateValue)
    (lambda (o)
      (and (list? o) (andmap (lambda (p)
                               (and (list? p) (equal? 2 (length p)) (symbol? (car p)) (validateValue (cadr p)))) o)))))

; Check to see if literal => a, #t, #f, afdfss, 12.1, 23, (quote (lambda 2))
(define literal?
  (lambda (o)
    (or (eq? #t o) (eq? #f o) (string? o) (number? o) (symbol? o) (and (list? o) (equal? 'quote (car o))))))


; Take Any Type
(define any? (lambda (o) #t))


; Evaluate Result is True
(define true?
    (lambda (val)
      (if (not (equal? val #f)) #t #f)))


; Validate Expression Params
(define valid-exp-params?
  (lambda (name types minParams proc)
    (lambda (params)
      (if (if (list? types) (and (equal? (length types) (length params)) (andmap (lambda (t p) (t p)) types params) ) (and (andmap types params) (<= minParams (length params))))
          (apply proc params)
          (error name "Bad paramaters expected: ~s" types)))))


; Validate Expression List Length
(define min-list-length?
  (lambda (name minLength)
    (lambda (alist)
      (if (and (list? alist) (<= minLength (length alist)))
          #t
          (error name "List invalid length: expected length of atleast ~s" minLength)))))



; Expression Make Vector
(define exp-make-vector
  (lambda (params)
    (cond
      [(and (equal? 2 (length params)) (number? (car params)) (number? (cadr params))) (make-vector (car params) (cadr params))]
      [(and (equal? 1 (length params)) (number? (car params))) (make-vector (car params))]
      [else (error 'make-vector "Bad paramaters expected: (#<procedure:number?>) or (#<procedure:number?> #<procedure:number?>)")])))


; Expression Vector Ref
(define exp-vector-ref
  (lambda (params)
    (if (and (equal? 2 (length params)) (vector? (car params)) (number? (cadr params)))
        (if (< (cadr params) (vector-length (car params)))
            (vector-ref (car params) (cadr params))
            (error 'vector-ref "Out of Bounds: index ~s" (cadr params)))
        (error 'vector-ref "Bad paramaters expected: (#<procedure:vector?> #<procedure:number?>)"))))


; Expression Vector set
(define exp-vector-set
  (lambda (extend-env)
    (lambda (args args-exps)
      (if (and (equal? 3 (length args)) (vector? (car args)) (number? (cadr args)) (number? (caddr args)))
          (if (< (cadr args) (vector-length (car args)))
              (if (equal? 'var-exp (car (car args-exps)))
                  (vector-set! (car args) (cadr args) (caddr args))
                  (error 'vector-set! "Error Immutable Object: ~s" (car args)))
              (error 'vector-set! "Out of Bounds: index ~s" (cadr args)))
          (error 'vector-set! "Bad paramaters expected: (#<procedure:number?> #<procedure:vector?>)")))))


; Expression Check if Procedure
(define exp-procedure?
  (lambda (expression? proc-val?)
    (lambda (exp)
      (cond [(and (proc-val? exp) (equal? 'procedure (car exp))) #t]
            [(and (proc-val? exp) (equal? 'prim-proc (car exp))) #t]
            [else #f]))))

