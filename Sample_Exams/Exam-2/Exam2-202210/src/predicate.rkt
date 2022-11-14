#lang racket
(require "chez-init.rkt")
(provide exp-closure-params? exp-let-params? arg-literal? arg-any? arg-true? arg-procedure? valid-prim-params?)

;--------------------+
;                    |
; PARSING VALIDATION |
;                    |
;--------------------+

; Validate Closure Parameters
; Single or List of Symbols => a, (a), (a b), (a b c), (a b . c)
(define exp-closure-params?
  (lambda (o)
    (or (symbol? o) (empty? o) (and (pair? o) ((list-of? symbol?) (flatten o))))))


; Validate Let Parameters + Arguments
; List of Pairs: Symbol + Any => ((a 'v))
(define exp-let-params?
  (lambda (expression?)
    (lambda (o)
      (and (list? o) (andmap (lambda (p)
                               (and (list? p) (equal? 2 (length p)) (symbol? (car p)) (expression? (cadr p)))) o)))))



;--------------------+
;                    |
;   ARG VALIDATION   |
;                    |
;--------------------+


; Validate Argument Literal
; Check to see if varible is literal => a, #t, #f, afdfss, 12.1, 23, (quote (lambda 2))
(define arg-literal?
  (lambda (o)
    (or (eq? #t o) (eq? #f o) (string? o) (number? o) (symbol? o) (and (list? o) (equal? 'quote (car o))))))


; Validate Argument Any
; Validate all arguments no matter type
(define arg-any? (lambda (o) #t))


; Validate Argument True
; Validates if argument is true, everything but false
(define arg-true?
    (lambda (val)
      (if (not (equal? val #f)) #t #f)))


; Validates Argument Procedure
; Checks for Closure or Primitive Procedure
(define arg-procedure?
  (lambda (expression? proc-val?)
    (lambda (exp)
      (cond [(and (proc-val? exp) (equal? 'closure (car exp))) #t]
            [(and (proc-val? exp) (equal? 'prim-proc (car exp))) #t]
            [else #f]))))


;--------------------+
;                    |
;  ARGS 4 PRIM PROC  |
;                    |
;--------------------+

; Validate Primitive Procedures Paramaters
(define valid-prim-params?
  (lambda (name types minParams proc)
    (lambda (params)
      (if (if (list? types) (and (equal? (length types) (length params)) (andmap (lambda (t p) (t p)) types params) ) (and (andmap types params) (<= minParams (length params))))
          (apply proc params)
          ; (error name "Bad paramaters expected: ~s => ~s" types params)
          (raise 'bad-paramaters)))))


