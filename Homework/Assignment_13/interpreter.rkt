#lang racket

(require "../chez-init.rkt")
(require "./validation-helpers.rkt")
(provide eval-one-exp)
(require racket/trace)



; Things to FIX
; - c**r c***r
; - lambda fncs tests


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
   (data any?)]
  [lambda-exp
   (params lambda-params?)
   (body (list-of? expression?))]
  [app-exp
   (rator expression?)
   (rand (list-of? expression?))]
  [let-exp
   (name (lambda (o) (or (symbol? o) (equal? #f o))))
   (params (let-params? expression?))
   (body (list-of? expression?))]
  [let*-exp
   (name (lambda (o) (or (symbol? o) (equal? #f o))))
   (params (let-params? expression?))
   (body (list-of? expression?))]
  [letrec-exp
   (name (lambda (o) (or (symbol? o) (equal? #f o))))
   (params (let-params? expression?))
   (body (list-of? expression?))]
  [if-exp
   (if-exp expression?)
   (true-exp expression?)
   (false-exp (lambda (o) (or (expression? o) (equal? #f o))))]
)	

;; environment type definitions
(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms (list-of? symbol?))
   (vals (list-of? any?))
   (env environment?)])


; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
  [prim-proc (name symbol?)]
  [procedure (exp expression?)
             (env environment?)])


;-------------------+
;                   |
; sec:PARSER-HELPER |
;                   |
;-------------------+


; Parsing Error
(define parse-error
  (lambda (datum message)
    (error 'parse-exp message datum)))


; Parse Lambda
(define parse-lambda
  (lambda (parse-exp)
    (lambda (datum)
      (if (and (< 2 (length datum)) (lambda-params? (cadr datum)))
          (lambda-exp (cadr datum) (map parse-exp (cddr datum)))
          (parse-error datum "Bad Lambda Syntax \"lambda (symbol ..) body ..\" => ~s")))))


; Parse Let
(define parse-let
  (lambda (parse-exp let-type)
    (lambda (datum)
      (cond [(and (< 3 (length datum)) (symbol? (cadr datum)) ((let-params? parse-exp) (caddr datum)))
             (let-type (cadr datum) (map (lambda (o) (list (car o) (parse-exp (cadr o)))) (caddr datum)) (map parse-exp (cdddr datum)))]
            [(and (< 2 (length datum)) ((let-params? parse-exp) (cadr datum)))
             (let-type #f (map (lambda (o) (list (car o) (parse-exp (cadr o)))) (cadr datum)) (map parse-exp (cddr datum)))]
            [else (parse-error datum "Bad Let Syntax \"let [name] ((symbol expression) ..) body ..\" => ~s")]))))


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
      [(literal? datum) (lit-exp (if (list? datum) (cadr datum) datum))]
      [(list? datum) (cond
                       [(equal? (car datum) 'lambda) ((parse-lambda parse-exp) datum)]
                       [(equal? (car datum) 'let) ((parse-let parse-exp let-exp) datum)]
                       [(equal? (car datum) 'let*) ((parse-let parse-exp let*-exp) datum)]
                       [(equal? (car datum) 'letrec) ((parse-let parse-exp letrec-exp) datum)]
                       [(equal? (car datum) 'if) ((parse-if parse-exp) datum)]
                       [(equal? (car datum) 'set!) ((parse-set! parse-exp) datum)]
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


(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
            [(eq? sym (car los)) pos]
            [else (loop (cdr los) (add1 pos))]))))


(define apply-env
  (lambda (env sym) 
    (cases environment env 
      [empty-env-record ()      
                        (error 'env "variable ~s not found." sym)]
      [extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym)))])))


;-----------------------+
;                       |
;  sec:SYNTAX EXPANSION |
;                       |
;-----------------------+

; To be added in assignment 14.

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
                 (let ([proc-val  (eval-exp rator env)]
                       [args-vals (eval-rands rands env)]
                       [args-exps rands])
                   (apply-proc proc-val args-vals args-exps env))]
        [if-exp (if-exp true-exp false-exp)
                (if (true? (eval-exp if-exp)) (eval-exp true-exp) (eval-exp false-exp))]
        [lambda-exp (params body) (procedure exp env)]
        [let-exp (name params body)
                 (let* ([syms (map car params)]
                        [vals (eval-rands (map cadr params) env)]
                        [letenv (extend-env syms vals env)])
                   (eval-bodies body letenv))]
        [else (error 'eval-exp "Bad abstract syntax: ~a" exp)]))))


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
  (lambda (proc-value args-vals args-exps env)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args-vals args-exps env)]
      [procedure (exp env) (apply-lambda exp args-vals args-exps env)]
      ; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                   proc-value)])))

(define *prim-proc-names* '(+ - * add1 sub1 cons = / zero? not = > < >= <=
                              length vector->list list->vector vector make-vector vector-ref vector-set! assq
                              car cdr caar cadr cdar cddr caaar caadr cadar cdaar caddr cdadr cddar cdddr list
                              null? list? empty? pair? vector? atom? number? symbol? equal? procedure? eq?
                              display newline ))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*   ;  a value (not an expression) with an identifier.
   (map prim-proc      
        *prim-proc-names*)
   (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.


; Apply Lambabda
(define apply-lambda
  (lambda (op args args-exps env)
      (cases expression op
        [lambda-exp (params body) (eval-bodies body (extended-env-record params args env))]
        [else (error 'apply-lambda "Unable to apply lambda: ~s"  op)])))


; Apply Primitive Procedures
(define apply-prim-proc
  (lambda (prim-proc args args-exps env)
      (case prim-proc

        ; Basic Math
        [(+) ((valid-exp-params? '+ number? 0 +) args)]
        [(-) ((valid-exp-params? '- number? 1 -) args)]
        [(*) ((valid-exp-params? '* number? 0 *) args)]
        [(/) ((valid-exp-params? '/ number? 1 /) args)]
        [(=) ((valid-exp-params? '= number? 1 =) args)]
        [(>) ((valid-exp-params? '> number? 1 >) args)]
        [(<) ((valid-exp-params? '< number? 1 <) args)]
        [(>=) ((valid-exp-params? '>= number? 1 >=) args)]
        [(<=) ((valid-exp-params? '<= number? 1 <=) args)]
        [(add1) ((valid-exp-params? 'add1 (list number?) -1 (lambda (o) (+ o 1))) args)]
        [(sub1) ((valid-exp-params? 'sub1 (list number?) -1 (lambda (o) (- o 1))) args)]
        [(zero?) ((valid-exp-params? 'zero? (list number?) -1 (lambda (o) (equal? o 0))) args)]
        [(not) ((valid-exp-params? 'not (list any?) -1 (lambda (o) (if (true? o) #f #t))) args)]

        ; Basic List
        [(list) ((valid-exp-params? 'list any? 0 list) args)]
        [(length) ((valid-exp-params? 'length (list list?) -1 length) args)]
        [(list->vector) ((valid-exp-params? 'list->vector (list list?) -1 list->vector) args)]
        [(vector->list) ((valid-exp-params? 'vector->list (list vector?) -1 vector->list) args)]
        [(vector) ((valid-exp-params? 'vector any? 0 vector) args)]
        [(make-vector) (exp-make-vector args)]
        [(vector-ref) (exp-vector-ref args)]
        [(vector-set!) ((exp-vector-set extend-env) args args-exps)]
        [(assq) ((valid-exp-params? 'assq (list any? (list-of? list?)) -1 assq) args)]

        ; List Based Access ; FIXME Checks for c**r and c***r
        [(cons) ((valid-exp-params? 'cons (list any? any?) -1 cons) args)]
        [(car) ((valid-exp-params? 'car (list (min-list-length? 'car 1)) -1 car) args)]
        [(cdr) ((valid-exp-params? 'cdr (list (min-list-length? 'cdr 1)) -1 cdr) args)]
        [(cddr) ((valid-exp-params? 'cddr (list (min-list-length? 'cddr 2)) -1 cddr) args)]
        [(cadr) ((valid-exp-params? 'cadr (list (min-list-length? 'cadr 2)) -1 cadr) args)]
        [(cdar) ((valid-exp-params? 'cdar (list (min-list-length? 'cdar 1)) -1 cdar) args)]
        [(caar) ((valid-exp-params? 'caar (list (min-list-length? 'caar 1)) -1 caar) args)]
        [(caaar) ((valid-exp-params? 'caaar (list (min-list-length? 'caaar 1)) -1 caaar) args)]
        [(caadr) ((valid-exp-params? 'caadr (list (min-list-length? 'caadr 2)) -1 caadr) args)]
        [(cadar) ((valid-exp-params? 'cadar (list (min-list-length? 'cadar 1)) -1 cadar) args)]
        [(cdaar) ((valid-exp-params? 'cdaar (list (min-list-length? 'cdaar 1)) -1 cdaar) args)]
        [(caddr) ((valid-exp-params? 'caddr (list (min-list-length? 'caddr 3)) -1 caddr) args)]
        [(cdadr) ((valid-exp-params? 'cdadr (list (min-list-length? 'cdadr 2)) -1 cdadr) args)]
        [(cddar) ((valid-exp-params? 'cddar (list (min-list-length? 'cddar 1)) -1 cddar) args)]
        [(cdddr) ((valid-exp-params? 'cdddr (list (min-list-length? 'cdddr 4)) -1 cdddr) args)]

        ; Predicates
        [(null?) ((valid-exp-params? 'null? (list any?) -1 null?) args)]
        [(list?) ((valid-exp-params? 'list? (list any?) -1 list?) args)]
        [(empty?) ((valid-exp-params? 'empty? (list any?) -1 empty?) args)]
        [(pair?) ((valid-exp-params? 'pair? (list any?) -1 pair?) args)]
        [(vector?) ((valid-exp-params? 'vector? (list any?) -1 vector?) args)]
        [(symbol?) ((valid-exp-params? 'symbol? (list any?) -1 symbol?) args)]
        [(number?) ((valid-exp-params? 'number? (list any?) -1 number?) args)]
        [(atom?) ((valid-exp-params? 'atom? (list any?) -1 (lambda (o) (not (or (list? o) (pair? o) (vector? o) (null? o))))) args)] ; anything but list, pair, null
        [(equal?) ((valid-exp-params? 'equal? (list any? any?) -1 equal?) args)] ; FIXME LATER
        [(eq?) ((valid-exp-params? 'eq? (list any? any?) -1 eq?) args)] ; FIXME LATER
        [(procedure?) ((valid-exp-params? 'equal? (list any?) -1 (exp-procedure? expression? proc-val?)) args)]

        ; Other
        [(display) ((valid-exp-params? 'display (list any?) -1 display) args)]
        [(newline) (newline)]
        
        [else (error 'apply-prim-proc 
                     "Bad primitive procedure name: ~s"
                     prim-proc)])))



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
  (lambda (x) (top-level-eval (parse-exp x))))

(rep)

;(eval-one-exp '((lambda (a) (+ a 1)) 1))
;(eval-one-exp '(((lambda (f) ((lambda (x) (f (lambda (y) ((x x) y)))) (lambda (x) (f (lambda (y) ((x x) y)))))) (lambda (g) (lambda (n) (if (zero? n) 1 (* n (g (- n 1))))))) 6))
;(eval-one-exp '(let ([compose2 (lambda (a) (+ a 1))]) (compose2 1)))
;(eval-one-exp '(let ([compose2 (lambda (a) (lambda (b) (+ a b)))]) ((compose2 1) 1)))
;(eval-one-exp '(let ([compose2 (lambda (f g) (lambda (x) (f (g x))))]) (let ([h  (let ([g (lambda (x) (+ 1 x))] [f (lambda (y) (* 2 y))]) (compose2 g f))]) (h 4))))