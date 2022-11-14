#lang racket

(require "./src/chez-init.rkt")
(require "./src/predicate.rkt")
(require "./src/primitive.rkt")
(require "./src/utilities.rkt")
(provide eval-one-exp)
(require racket/trace)



; Things to FIX
; - Add Erring Tests for Built in functions
; - Remove negative? from proc-procedures

; Function to Add
; - Add list-set, list-ref, member
; - Add member for list
; - Add ^ pow math
; - Add Max to while loop, with way to define max to while loop


;-------------------+
;                   |
;   sec:DATATYPES   |
;                   |
;-------------------+

; Data Type Expression
(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (data arg-any?)]
  [lambda-exp
   (params exp-closure-params?)
   (bodies (list-of? expression?))]
  [app-exp
   (rator expression?)
   (rand (list-of? expression?))]
  [let-exp
   (name (lambda (o) (or (symbol? o) (equal? #f o))))
   (params (list-of? symbol?))
   (args (list-of? expression?))
   (bodies (list-of? expression?))]
  [let*-exp
   (params (list-of? symbol?))
   (args (list-of? expression?))
   (bodies (list-of? expression?))]
  [letrec-exp
   (params (list-of? symbol?))
   (args (list-of? expression?))
   (bodies (list-of? expression?))]
  [if-exp
   (cond-exp expression?)
   (true-exp expression?)
   (false-exp (lambda (o) (or (expression? o) (equal? #f o))))]
  [cond-exp
   (conditions (list-of? expression?))
   (bodies (list-of? (list-of? expression?)))]
  [while-exp
   (cond-exp expression?)
   (bodies (list-of? expression?))]
  [or-exp
   (conditions (list-of? expression?))]
  [and-exp
   (conditions (list-of? expression?))]
  [simplecase-exp
    (condition expression?)
    (case-cond (list-of? symbol?))
    (body-cond (list-of? expression?))]
  [make-namespace-exp
    (params (list-of? symbol?))
    (args (list-of? expression?))]
  [use-namespace-exp
    (space expression?)
    (bodies (list-of? expression?))]
)	


;; environment type definitions
(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms exp-closure-params?)
   (vals (list-of? arg-any?))
   (env environment?)]
  [extended-env-record-recursively
   (syms exp-closure-params?)
   (vals (list-of? arg-any?))
   (env environment?)])


; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
  [prim-proc (name symbol?)]
  [closure (params exp-closure-params?)
           (bodies (list-of? expression?))
           (env environment?)])


;-------------------+
;                   |
; sec:PARSER-HELPER |
;                   |
;-------------------+


; Parsing Error
(define parse-error
  (lambda (datum message)
    ; (error 'parse-exp message datum)
    (raise 'parse-exp)))


; Parse Lambda
(define parse-lambda
  (lambda (parse-exp)
    (lambda (datum)
      (if (and (< 2 (length datum)) (exp-closure-params? (cadr datum)))
          (lambda-exp (cadr datum) (map parse-exp (cddr datum)))
          (parse-error datum "Bad Lambda Syntax \"lambda (symbol ..) body ..\" => ~s")))))


; Parse Let
(define parse-let
  (lambda (parse-exp)
    (lambda (datum)
      (cond [(and (< 3 (length datum)) (symbol? (cadr datum)) ((exp-let-params? parse-exp) (caddr datum)))
             (let-exp (cadr datum) (map car (caddr datum)) (map (lambda (o) (parse-exp (cadr o))) (caddr datum)) (map parse-exp (cdddr datum)))]
            [(and (< 2 (length datum)) ((exp-let-params? parse-exp) (cadr datum)))
             (let-exp #f (map car (cadr datum)) (map (lambda (o) (parse-exp (cadr o))) (cadr datum)) (map parse-exp (cddr datum)))]
            [else (parse-error datum "Bad Let Syntax \"let [name] ((symbol expression) ..) body ..\" => ~s")]))))


; Parse Let*
(define parse-let*
  (lambda (parse-exp)
    (lambda (datum)
      (if (and (< 2 (length datum)) ((exp-let-params? parse-exp) (cadr datum)))
          (let*-exp (map car (cadr datum)) (map (lambda (o) (parse-exp (cadr o))) (cadr datum)) (map parse-exp (cddr datum)))
          (parse-error datum "Bad Let Syntax \"let* ((symbol expression) ..) body ..\" => ~s")))))


; Parse Letrec
(define parse-letrec
  (lambda (parse-exp)
    (lambda (datum)
      (if (and (< 2 (length datum)) ((exp-let-params? parse-exp) (cadr datum)))
          (letrec-exp (map car (cadr datum)) (map (lambda (o) (parse-exp (cadr o))) (cadr datum)) (map parse-exp (cddr datum)))
          (parse-error datum "Bad Let Syntax \"letrec ((symbol expression) ..) body ..\" => ~s")))))


; Parse if
(define parse-if
  (lambda (parse-exp)
    (lambda (datum)
      (cond [(and (equal? 3 (length datum)) (parse-exp (cadr datum)) (parse-exp (caddr datum))) ; FIX REUSE
             (if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) #f)]
            [(and (equal? 4 (length datum)) (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum)))  ; FIX REUSE
             (if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum)))]
            [else (parse-error datum "Bad If Syntax \"if expression true-body false-body ..\" => ~s")]))))


; Parse Set!
(define parse-set!
  (lambda (parse-exp)
    (lambda (datum)
      (if (and (equal? 3 (length datum)) (symbol? (cadr datum)) (parse-exp (caddr datum)))
          (app-exp (parse-exp (car datum)) (parse-exp (cadr datum)) (parse-exp (caddr datum)))
          (parse-error datum "Bad Set Syntax \"set! symbol new-value\" => ~s")))))


; Parse Cond
(define parse-cond
  (lambda (parse-exp)
    (lambda (datum)
      (if (andmap (lambda (o) (and (list? o) (< 1 (length o)))) (cdr datum))
          (cond-exp (map parse-exp (map (lambda (o) (if (equal? 'else (car o)) #t (car o))) (cdr datum))) (map (lambda (o) (map parse-exp o)) (map cdr (cdr datum))))
          (parse-error datum "Bad While Syntax: ~s")))))


; Parse While
(define parse-while
  (lambda (parse-exp)
    (lambda (datum)
      (if (not (empty? (cdr datum)))
          (while-exp (parse-exp (cadr datum)) (map parse-exp (cddr datum)))     
          (parse-error datum "Bad While Syntax: ~s")))))


; Parse Simple Case
(define parse-simplecase
  (lambda (datum)
    (simplecase-exp (parse-exp (car datum)) (map car (cdr datum)) (map parse-exp (map cadr (cdr datum))))))


; Parse Make Namespace
(define parse-make-namespace
  (lambda (datum)
    (make-namespace-exp (map car (car datum)) (map parse-exp (map cadr (car datum))))))


; Parse Make Namespace
(define parse-use-namespace
  (lambda (datum)
    (use-namespace-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))))


;-------------------+
;                   |
;    sec:PARSER     |
;                   |
;-------------------+


; Helper procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)


; Set Parse Expression
(define parse-exp
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(arg-literal? datum) (lit-exp (if (list? datum) (cadr datum) datum))]
      [(list? datum) (cond
                       [(equal? (car datum) 'lambda) ((parse-lambda parse-exp) datum)]
                       [(equal? (car datum) 'let) ((parse-let parse-exp) datum)]
                       [(equal? (car datum) 'let*) ((parse-let* parse-exp) datum)]
                       [(equal? (car datum) 'letrec) ((parse-letrec parse-exp) datum)]
                       [(equal? (car datum) 'while) ((parse-while parse-exp) datum)]
                       [(equal? (car datum) 'if) ((parse-if parse-exp) datum)]
                       [(equal? (car datum) 'set!) ((parse-set! parse-exp) datum)]
                       [(equal? (car datum) 'cond) ((parse-cond parse-exp) datum)]
                       [(equal? (car datum) 'and) (and-exp (map parse-exp (cdr datum)))]
                       [(equal? (car datum) 'or) (or-exp (map parse-exp (cdr datum)))]
                       [(equal? (car datum) 'begin) (let-exp #f '() '() (map parse-exp (cdr datum)))]
                       [(equal? (car datum) 'simplecase) (parse-simplecase (cdr datum))]
                       [(equal? (car datum) 'make-namespace) (parse-make-namespace (cdr datum))]
                       [(equal? (car datum) 'use-namespace) (parse-use-namespace (cdr datum))]
                       [else (let ([sym (parse-exp (car datum))] [parms (map parse-exp (cdr datum))])
                               (if (and sym (andmap (lambda (o) o) parms))
                                   (app-exp sym parms)
                                   (parse-error datum "bad expression: ~s")))])]
      [else (parse-error datum "bad expression: ~s")])))


;-------------------+
;                   |
; sec:ENVIRONMENTS  |
;                   |
;-------------------+


; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and 2.3


(define empty-env
  (lambda ()
    (empty-env-record)))


(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))


(define extend-env-recursively
  (lambda (syms vals env)
    (extended-env-record-recursively syms vals env)))


(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
            [(equal? sym los) #t]
            [(not (pair? los)) #f]
            [(eq? sym (car los)) pos]
            [else (loop (cdr los) (add1 pos))]))))


(define apply-env
  (lambda (env sym) 
    (cases environment env 
      [empty-env-record ()      
        (error 'env "variable ~s not found." sym)
        ; (raise 'varible-not-found)
        ]
      [extended-env-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (cond [(equal? sym syms) vals]
                [(equal? #t pos) (list-tail vals (sub1 (length (flatten syms))))]
                [(number? pos) (list-ref vals pos)]
                [else (apply-env env sym)]))]
      [extended-env-record-recursively (syms vals old-env)
        (let ((pos (list-find-position sym syms)))
          (cond [(number? pos)
                  (let ([body (list-ref vals pos)])
                    (if (and (list? body) (not (empty? body)) (equal? 'lambda-exp (car body)))
                      (closure (cadr body) (caddr body) env)
                      (list-ref vals pos)))]
                [else (apply-env old-env sym)]))])))



;-----------------------+
;                       |
;  sec:SYNTAX EXPANSION |
;                       |
;-----------------------+


; (let name ([a 1] [b 2])
;   (...))
; (letrec ([name (lambda (a b) (...))]) (name 1 2))

; Syntex Expand Let
(define syntax-expand-let
  (lambda (name params args bodies)
    (if name
      (letrec-exp (list name) (list (lambda-exp params (map syntax-expand bodies))) (list (app-exp (var-exp name) args)))
      (app-exp (lambda-exp params (map syntax-expand bodies)) (map syntax-expand args)))))


; Syntex Expand Let*
(define syntax-expand-let*
  (lambda (params args bodies)
    (cond [(equal? 0 (length args)) (app-exp (lambda-exp '() (map syntax-expand bodies)) '())]
				  [(equal? 1 (length args)) (app-exp (lambda-exp (list (car params)) (map syntax-expand bodies)) (list (syntax-expand (car args))))]
          [else (app-exp (lambda-exp (list (car params)) (list (syntax-expand-let* (cdr params) (cdr args) bodies))) (list (syntax-expand (car args))))])))


; Syntex Expand Cond
(define syntax-expand-cond
  (lambda (conditions bodies)
    (cond [(equal? 1 (length conditions)) (if-exp (syntax-expand (car conditions)) (app-exp (lambda-exp '() (map syntax-expand (car bodies))) '()) #f)]
          [else (if-exp (syntax-expand (car conditions)) (app-exp (lambda-exp '() (map syntax-expand (car bodies))) '()) (syntax-expand-cond (cdr conditions) (cdr bodies)))])))


; Syntex Expand Or
(define syntax-expand-or
  (lambda (conditions)
    (cond [(equal? 0 (length conditions)) (lit-exp #f)]
          [else (app-exp (lambda-exp (list 'condition) (list (if-exp (var-exp 'condition) (var-exp 'condition) (syntax-expand-or (cdr conditions))))) (list (syntax-expand (car conditions))))])))


; Syntex Expand And
(define syntax-expand-and
  (lambda (conditions)
    (cond [(equal? 0 (length conditions)) (lit-exp #t)]
					[(equal? 1 (length conditions)) (app-exp (lambda-exp (list 'condition) (list (if-exp (var-exp 'condition) (var-exp 'condition) (lit-exp #f)))) (list (syntax-expand (car conditions))))]
          [else (if-exp (syntax-expand (car conditions)) (syntax-expand-and (cdr conditions)) (lit-exp #f))])))


; Syntex Expand Simple Case
(define syntax-expand-simplecase
  (lambda (condition case-cond body-cond)
    ; (app-exp (lambda-exp '(eval-condition) (list (app-exp (var-exp 'equal?) (list (var-exp 'eval-condition) (lit-exp 'a))))) (list (syntax-expand condition)))))
    (app-exp (lambda-exp '(eval-condition) (map (lambda (cs bd)
      (if-exp (app-exp (var-exp 'equal?) (list (lit-exp cs) (var-exp 'eval-condition))) (syntax-expand bd) #f)) case-cond body-cond)) (list (syntax-expand condition)))))


; Syntex Expander
(define syntax-expand
  	(lambda (datum)
		(if (equal? datum #f) #f
			(cases expression datum
			[lambda-exp (params bodies) (lambda-exp params (map syntax-expand bodies))]
			[if-exp (cond-exp true-exp false-exp) (if-exp (syntax-expand cond-exp) (syntax-expand true-exp) (syntax-expand false-exp))]
			[let-exp (name params args bodies) (syntax-expand-let name params args bodies)]
			[let*-exp (params args bodies) (syntax-expand-let* params args bodies)]
      [letrec-exp (params args bodies) (letrec-exp params (map syntax-expand args) (map syntax-expand bodies))]
			[app-exp (rator rand) (app-exp (syntax-expand rator) (map syntax-expand rand))]
			[cond-exp (conditions bodies) (syntax-expand-cond conditions bodies)]
      [while-exp (cond-exp bodies) (while-exp (syntax-expand cond-exp) (map syntax-expand bodies))]
      [or-exp (conditions) (syntax-expand-or conditions)]
			[and-exp (conditions) (syntax-expand-and conditions)]
      [simplecase-exp (condition case-cond body-cond) (syntax-expand-simplecase condition case-cond body-cond)]
      [make-namespace-exp (params args) (make-namespace-exp params (map syntax-expand args))]
      [use-namespace-exp (name bodies) (use-namespace-exp name (map syntax-expand bodies))]
			[else datum]))))


;---------------------------------------+
;                                       |
; sec:CONTINUATION DATATYPE and APPLY-K |
;                                       |
;---------------------------------------+

; To be added in assignment 18a.


;-------------------+
;                   |
;  sec:INTERPRETER  |
;                   |
;-------------------+

; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form)))

; eval-exp is the main component of the interpreter


; While Loop Defintion
(define while
  (lambda (condition body)
    (let ((isValid (condition)))
      (if isValid
        (let () (body) (while condition body))
        (void)))))


; Evalue Expression
; Could Possible return an envirenment
(define eval-exp
  (lambda (exp . env)
    (let ([env (if (not (empty? env)) (car env) init-env)])
      (cases expression exp
        [lit-exp (datum) datum]
        [var-exp (id)
                 (apply-env env id)]
        [app-exp (rator rands)
                 (let ([proc (eval-exp rator env)]
                       [args (eval-rands rands env)])
                   (apply-proc proc args env))]
        [if-exp (cond-exp true-exp false-exp)
                (if (arg-true? (eval-exp cond-exp env)) (eval-exp true-exp env) (if (equal? #f false-exp) (void) (eval-exp false-exp env)))]
        [lambda-exp (params bodies) (closure params bodies env)]
        [while-exp (cond-exp bodies)
                       (while (lambda () (arg-true? (eval-exp cond-exp env))) (lambda () (eval-bodies bodies env)))]
        [letrec-exp (params args bodies)
                    (eval-bodies bodies (extend-env-recursively params args env))]
        [make-namespace-exp (params args)
                    (map (lambda (param arg) (list param (eval-exp arg))) params args)]
        [use-namespace-exp (space bodies)
                    (let ([namespace (eval-exp space env)])
                      (eval-bodies bodies (extend-env (map car namespace) (map cadr namespace) env)))]
        [else
          ; (error 'eval-exp "Bad abstract syntax: ~a" exp)
          (raise 'eval-exp)]))))


; evaluate the list of operands, putting results into a list
(define eval-rands
  (lambda (rands env)
    (map (lambda (o) (eval-exp o env)) rands)))


; Eval Multiple Bodies
(define eval-bodies
  (lambda (bodies env-org)
    (let ([res (let eval-body-helper ([bodies bodies] [env env-org])
                 (if (empty? bodies) '()
                     (let ([result (eval-exp (car bodies) env)])
                       (if (not (void? result))
                           (append (eval-body-helper (cdr bodies) env) (list result))
                           (eval-body-helper (cdr bodies) env)))))])
      (if (not (empty? res)) (car res) (void)))))


;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
(define apply-proc
  (lambda (proc-value args env)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args env)]
      [closure (params bodies env) (apply-closer bodies params args env)]
      ; You will add other cases
      [else
            ; (error 'apply-proc
            ;        "Attempt to apply bad procedure: ~s" 
            ;        proc-value)
            (raise 'apply-proc)])))


; Primitive Procedures
(define *prim-proc-names* '(+ - * add1 sub1 cons = / zero? not = > < >= <= quotient
                              length vector->list list->vector vector make-vector vector-ref vector-set! assq map apply
                              car cdr caar cadr cdar cddr caaar caadr cadar cdaar caddr cdadr cddar cdddr list
                              null? list? empty? pair? vector? atom? number? symbol? equal? procedure? eq? negative? positive?
                              display newline list-tail eqv? append))


(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*     ; a value (not an expression) with an identifier.
   (map prim-proc      
        *prim-proc-names*)
   (empty-env)))


; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.


; Apply Lambabda
(define apply-closer
  (lambda (bodies params args-vals env)
      (eval-bodies bodies (extended-env-record params args-vals env))))


; Apply Primitive Procedures
(define apply-prim-proc
  (lambda (prim-proc args env)
      (case prim-proc

        ; Basic Math
        [(+) ((valid-prim-params? '+ number? 0 +) args)]
        [(-) ((valid-prim-params? '- number? 1 -) args)]
        [(*) ((valid-prim-params? '* number? 0 *) args)]
        [(/) ((valid-prim-params? '/ number? 1 /) args)]
        [(=) ((valid-prim-params? '= number? 1 =) args)]
        [(>) ((valid-prim-params? '> number? 1 >) args)]
        [(<) ((valid-prim-params? '< number? 1 <) args)]
        [(>=) ((valid-prim-params? '>= number? 1 >=) args)]
        [(<=) ((valid-prim-params? '<= number? 1 <=) args)]
        [(add1) ((valid-prim-params? 'add1 (list number?) -1 (lambda (o) (+ o 1))) args)]
        [(sub1) ((valid-prim-params? 'sub1 (list number?) -1 (lambda (o) (- o 1))) args)]
        [(zero?) ((valid-prim-params? 'zero? (list number?) -1 (lambda (o) (equal? o 0))) args)]
        [(not) ((valid-prim-params? 'not (list arg-any?) -1 (lambda (o) (if (arg-true? o) #f #t))) args)]

        ; Basic List
        [(list) ((valid-prim-params? 'list arg-any? 0 list) args)]
        [(length) ((valid-prim-params? 'length (list list?) -1 length) args)]
        [(list->vector) ((valid-prim-params? 'list->vector (list list?) -1 list->vector) args)]
        [(vector->list) ((valid-prim-params? 'vector->list (list vector?) -1 vector->list) args)]
        [(vector) ((valid-prim-params? 'vector arg-any? 0 vector) args)]
        [(make-vector) (exp-make-vector args)]
        [(vector-ref) (exp-vector-ref args)]
        [(vector-set!) ((exp-vector-set extend-env) args)]
        [(assq) ((valid-prim-params? 'assq (list arg-any? (list-of? list?)) -1 assq) args)]

        ; Advanced List
        [(map) ((exp-map (arg-procedure? expression? proc-val?) apply-proc) args env)]
        [(apply) ((exp-apply (arg-procedure? expression? proc-val?) apply-proc) args env)]

        ; List Based Access ; FIXME Checks for c**r and c***r
        [(cons) ((valid-prim-params? 'cons (list arg-any? arg-any?) -1 cons) args)]
        [(car) ((valid-prim-params? 'car (list list?) -1 (exp-cr 'car (list car))) args)]
        [(cdr) ((valid-prim-params? 'cdr (list list?) -1 (exp-cr 'car (list cdr))) args)]
        [(cddr) ((valid-prim-params? 'cddr (list list?) -1 (exp-cr 'cddr (list cdr cdr))) args)]
        [(cadr) ((valid-prim-params? 'cadr (list list?) -1 (exp-cr 'cadr (list cdr car))) args)]
        [(cdar) ((valid-prim-params? 'cdar (list list?) -1 (exp-cr 'cdar (list car cdr))) args)]
        [(caar) ((valid-prim-params? 'caar (list list?) -1 (exp-cr 'caar (list car car))) args)]
        [(caaar) ((valid-prim-params? 'caaar (list list?) -1 (exp-cr 'caaar (list car car car))) args)]
        [(caadr) ((valid-prim-params? 'caadr (list list?) -1 (exp-cr 'caadr (list cdr car car))) args)]
        [(cadar) ((valid-prim-params? 'cadar (list list?) -1 (exp-cr 'cadar (list car cdr car))) args)]
        [(cdaar) ((valid-prim-params? 'cdaar (list list?) -1 (exp-cr 'cdaar (list car car cdr))) args)]
        [(caddr) ((valid-prim-params? 'caddr (list list?) -1 (exp-cr 'caddr (list cdr cdr car))) args)]
        [(cdadr) ((valid-prim-params? 'cdadr (list list?) -1 (exp-cr 'cdadr (list cdr car cdr))) args)]
        [(cddar) ((valid-prim-params? 'cddar (list list?) -1 (exp-cr 'cddar (list car cdr cdr))) args)]
        [(cdddr) ((valid-prim-params? 'cdddr (list list?) -1 (exp-cr 'cdddr (list cdr cdr cdr))) args)]

        ; Predicates
        [(null?) ((valid-prim-params? 'null? (list arg-any?) -1 null?) args)]
        [(list?) ((valid-prim-params? 'list? (list arg-any?) -1 list?) args)]
        [(empty?) ((valid-prim-params? 'empty? (list arg-any?) -1 empty?) args)]
        [(pair?) ((valid-prim-params? 'pair? (list arg-any?) -1 pair?) args)]
        [(vector?) ((valid-prim-params? 'vector? (list arg-any?) -1 vector?) args)]
        [(symbol?) ((valid-prim-params? 'symbol? (list arg-any?) -1 symbol?) args)]
        [(number?) ((valid-prim-params? 'number? (list arg-any?) -1 number?) args)]
        [(atom?) ((valid-prim-params? 'atom? (list arg-any?) -1 (lambda (o) (not (or (list? o) (pair? o) (vector? o) (null? o))))) args)] ; anything but list, pair, null
        [(equal?) ((valid-prim-params? 'equal? (list arg-any? arg-any?) -1 equal?) args)]
        [(eq?) ((valid-prim-params? 'eq? (list arg-any? arg-any?) -1 eq?) args)]
        [(procedure?) ((valid-prim-params? 'equal? (list arg-any?) -1 (arg-procedure? expression? proc-val?)) args)]

        ; Other
        [(display) ((valid-prim-params? 'display (list arg-any?) -1 display) args)]
        [(newline) (newline)]

				; FIXME - REMOVE ONCE WE HAVE DEFINE + MULTI EXECTURE SETUP AS WE CAN DECLARE IN CODE
				[(negative?) ((valid-prim-params? 'negative? (list number?) -1 negative?) args)]
        [(positive?) ((valid-prim-params? 'positive? (list number?) -1 positive?) args)]
        [(quotient) ((valid-prim-params?  'quotient number? 1 quotient) args)]
        [(list-tail) ((valid-prim-params? 'list-tail (list list? number?) -1 list-tail) args)]
        [(eqv?) ((valid-prim-params? 'eqv? (list arg-any? arg-any?) -1 eqv?) args)]
        [(append) ((valid-prim-params? 'append (list list? list?) -1 append) args)]
        [else
              ; (error 'apply-prim-proc 
              ;        "Bad primitive procedure name: ~s"
              ;        prim-proc)
              (raise 'apply-prim-proc)])))


(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let* ([cli (read)] [answer (if (equal? cli '(exit)) cli (top-level-eval (parse-exp cli)))])
      (cond [(equal? answer '(exit)) (exit)]
            [(or (proc-val? answer)) (display '<interpreter-procedure>)]
            [(and (list? answer) (ormap proc-val? answer)) (pretty-print (map (lambda (o) (if (proc-val? o) '<interpreter-procedure> o)) answer))]
            [else (pretty-print answer)])
      (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.


(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))


(eval-one-exp '(make-namespace ((a 1) (b 2))))
(eval-one-exp '(use-namespace (make-namespace ((a 1) (b 2))) a))