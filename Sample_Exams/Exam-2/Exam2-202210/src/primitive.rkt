#lang racket
(require "chez-init.rkt")
(provide exp-apply exp-map exp-cr exp-make-vector exp-vector-ref exp-vector-set)


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
    (lambda (args)
      (if (and (equal? 3 (length args)) (vector? (car args)) (number? (cadr args)) (number? (caddr args)))
          (if (< (cadr args) (vector-length (car args)))
              (if (not (immutable? (car args)))
                  (vector-set! (car args) (cadr args) (caddr args))
                  (error 'vector-set! "Error Immutable Object: ~s" (car args)))
              (error 'vector-set! "Out of Bounds: index ~s" (cadr args)))
          (error 'vector-set! "Bad paramaters expected: (#<procedure:number?> #<procedure:vector?>)")))))


; Expression for c**r and c***r
(define exp-cr
  (lambda (name fncs)
    (lambda (clist)
      (let exp-cr-helper ([clist clist] [olist clist] [fncs fncs])
        (cond [(empty? fncs) clist]
              [(or (not (list? clist)) (empty? clist)) (error name "Contract Violation - expected none-empty list: ~s" olist)]
              [else (exp-cr-helper ((car fncs) clist) olist (cdr fncs))])))))


; Expression Map
(define exp-map
  (lambda (arg-procedure? apply-proc)
    (lambda (args env)
      (cond [(or (not (list? args)) (> 2 (length args)) (not (arg-procedure? (car args)))) (error 'map "Bad paramaters expected: (#<procedure:procedure?> #<procedure:list?> ...)")]
            [else (apply map (lambda o (apply-proc (car args) o env)) (cdr args))]))))


; Expression Apply
(define exp-apply
  (lambda (arg-procedure? apply-proc)
    (lambda (args env)
      (cond [(or (not (list? args)) (> 2 (length args)) (not (arg-procedure? (car args)))) (error 'apply "Bad paramaters expected: (#<procedure:procedure?> #<procedure:any?> ...)")]
            [else (apply-proc (car args) (flatten (cdr args)) env)]))))


