#lang racket

(require "../chez-init.rkt")
(provide typecheck)

(define-datatype type type?
  [number-t]
  [boolean-t]
  [proc-t (param type?) (ret type?)])

(define unparse-type
  (lambda (t)
    (if (eqv? t 'unknown-expression)
        'unknown-expression ; just allow our little error type to pass through
        (cases type t
          [number-t () 'num]
          [boolean-t () 'bool]
          [proc-t (p r) (list (unparse-type p) '-> (unparse-type r))]))))
            

(define parse-type
  (lambda (type-exp)
    (cond [(eqv? 'num type-exp) (number-t)]
          [(eqv? 'bool type-exp) (boolean-t)]
          [(and (list? type-exp)
                (= (length type-exp) 3)
                (eqv? '-> (second type-exp)))
           (proc-t (parse-type (first type-exp))
                   (parse-type (third type-exp)))]
          [else (error 'parse-type "unknown type ~s" type-exp)])))

(define-datatype expression expression?
  [var-exp (name symbol?)]
  [lit-exp (val (lambda(x) (or (number? x) (boolean? x))))]
  [if-exp (test-exp expression?) (then-exp expression?) (else-exp expression?)]
  [lam-exp (var symbol?) (ptype type?) (body expression?)]
  [letrec-exp (recurse-var symbol?) (ret-type type?) (lambda lam-exp?) (body expression?)]
  [app-exp (rator expression?) (rand expression?)])

; our letrec expression can only contain lambda initializers
(define lam-exp?
  (lambda (exp)
    (if (expression? exp)
        (cases expression exp
          [lam-exp (var ptype body) #t]
          [else #f])
        #f)))

(define parse
  (lambda (code)
    (cond [(symbol? code) (var-exp code)]
          [(number? code) (lit-exp code)]
          [(boolean? code) (lit-exp code)]
          [(list? code)
           (when (< (length code) 2) (error 'short-app "param list too short ~s" code))
           (if (symbol? (car code))
               (case (car code)
                 [(if) (unless (= (length code) 4) (error 'bad-if "bad if"))
                       
                       (if-exp (parse (second code))
                               (parse (third code))
                               (parse (fourth code)))]
                 [(lambda) (unless (= (length code) 4) (error 'bad-lambda "bad lambda"))
                           (let ([type (second code)]
                                 [param (third code)])
                             (unless (and
                                      (pair? param)
                                      (symbol? (car param)))
                               (error 'bad-param "bad lambda param ~s" (cadr code)))
                             (lam-exp (car param) (parse-type type) (parse (fourth code))))
                                 ]
                 [(letrec) (unless (= (length code) 5) (error 'bad-letrec "wrong length"))
                           (let [(ret (parse-type (second code)))
                                 (var (third code))
                                 (lam (parse (fourth code)))
                                 (body (parse (fifth code)))]
                             (unless (symbol? var) (error 'bad-lectrec "bad var"))
                             (unless (lam-exp? lam) (error 'bad-lectrec "lamdba required"))
                             (letrec-exp var ret lam body))]
                             
                 [else (parse-app code)])
               (parse-app code))]
           )))


(define parse-app
  (lambda (code)
    (app-exp (parse (first code))
                   (parse (second code)))))


(define typecheck
  (lambda (code)
    (unparse-type (typecheck-exp (parse code)))))


(define typecheck-exp
  (lambda (exp)
    (typecheck-exp-env exp (list 'zero? (proc-t (number-t) (boolean-t)) (list '- (proc-t (number-t) (proc-t (number-t) (number-t))) '())))))


(define apply-env
  (lambda (var env)
    (cond [(empty? env) (raise 'unbound-var)]
          [(equal? var (car env)) (cadr env)]
          [else (apply-env var (caddr env))])))


(define typecheck-exp-env
  (lambda (exp env)
    (cases expression exp
      [lit-exp (value) (if (number? value) (number-t) (boolean-t))]
      [lam-exp (var ptype body) (proc-t ptype (typecheck-exp-env body (list var ptype env)))]
      [var-exp (var) (apply-env var env)]
      [if-exp (test-exp then-exp else-exp)
        (cond
          [(not (equal? (boolean-t) (typecheck-exp-env test-exp env))) (raise 'bad-if-test)]
          [(not (equal? (typecheck-exp-env then-exp env) (typecheck-exp-env else-exp env))) (raise 'bad-if-branches)]
          [else (begin (typecheck-exp-env then-exp env) (typecheck-exp-env else-exp env))])]
      [letrec-exp (recurse-var ret-type lambda body)
        (let ([recType (typecheck-exp-env lambda (list recurse-var (proc-t (caddr lambda) ret-type) env))])
          (if (equal? (caddr recType) ret-type)
            (typecheck-exp-env body (list recurse-var recType env))
            (raise 'bad-letrec-types)))]
      [app-exp (rator rand)
        (cases expression rator
          [lam-exp (var ptype body)
            (let ([rptype (typecheck-exp-env rand env)])
              (if (equal? ptype rptype)
                (caddr (typecheck-exp-env rator (list var rptype env)))
                (raise 'bad-parameter)))]
          [app-exp (srator srand)
            (let ([rtype (typecheck-exp-env rand env)] [rptype (typecheck-exp-env rator env)])
              (if (equal? (caddr rptype) rtype)
                (caddr rptype)
                (raise 'bad-parameter)))]
          [var-exp (var)
            (let ([vtype (apply-env var env)] [rtype (typecheck-exp-env rand env)])
              (if (equal? 'proc-t (car vtype))
                (if (equal? (cadr vtype) rtype)
                  (caddr vtype) ; cadr vtype
                  (raise 'bad-parameter))
                (raise 'bad-procedure)))]
          [else (raise 'bad-procedure)]
          )]
      [else 'unknown-expression])))

