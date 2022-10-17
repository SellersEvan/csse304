#lang racket
(require racket/trace)
(require "../chez-init.rkt")
(provide my-let my-or += return-first bintree? leaf-node interior-node bintree-to-list max-interior parse-exp unparse-exp)

(define-syntax my-let
  (syntax-rules ()
    [(my-let args ...)
     (let args ...)]))

(define-syntax my-or
  (syntax-rules ()
    [(_) #f]
    [(_ exp) exp]
    [(_ exp args ...)
     (let ((res exp))
       (if res res (my-or args ...)))]))

(define-syntax +=
  (syntax-rules ()
    [(_ namespace value)
     (let ((result (+ namespace value)))
       (set! namespace result)
       result)]))

(define-syntax return-first
  (syntax-rules ()
    [(_ namespace) namespace]
    [(_ namespace fnc)
     (let ((value namespace))
       fnc
       value)]
    [(_ namespace fnc fncs ...)
     (let ((value namespace))
       fnc
       (return-first value fncs ...)
       value)]))

(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left-tree bintree?)
   (right-tree bintree?)))

(define bintree-to-list
  (lambda (a)
    (cases bintree a
      [leaf-node (datum)
               (list 'leaf-node datum)]
      [interior-node (key left-tree right-tree)
               (list 'interior-node key (bintree-to-list left-tree) (bintree-to-list right-tree))])))


(define >f
  (lambda (e1 e2)
    (cond [(and e1 e2) (> e1 e2)]
          [(and e1 (not e2)) #t]
          [(and e2 (not e1)) #f]
          [else #f])))


(define <f
  (lambda (e1 e2)
    (not (>f e1 e2))))


; (maxElm maxCount curCount)
(define max-interior
  (lambda (a)
    (car (let bt-max-interior-helper ([tree a])
           (cases bintree tree
             [leaf-node (datum) (list #f #f datum)]
             [interior-node (key left-tree right-tree)
                            (let* ([right (bt-max-interior-helper left-tree)]
                                   [left (bt-max-interior-helper right-tree)]
                                   [curTotal (+ (caddr left) (caddr right))])
                              (cond [(and (>f curTotal (cadr left)) (>f curTotal (cadr right))) (list key curTotal curTotal)]
                                    [(<f (cadr right) (cadr left)) (list (car left) (cadr left) curTotal)]
                                    [(<f (cadr left) (cadr right)) (list (car right) (cadr right) curTotal)]
                                    [else (list (car left) (cadr left) curTotal)]))])))))



; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.


; List of Specific Type
(define list-of?
  (lambda (predicate)
    (lambda (o)
      (and (list? o) (andmap predicate o)))))


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
    (or (eq? #t o) (eq? #t o) (number? o) (symbol? o) (and (list? o) (not (empty? o)) (equal? 'quote (car o))))))


; DataType for Expression
(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (data (lambda (o) #t))]
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


; Parse Literal
(define parse-literal
  (lambda (parse-exp)
    (lambda (datum)
      (if (and (list? datum) (not (vector? datum)) (not (empty? datum)) (equal? 'quote (car datum)))
          (cadr datum)
          datum))))


; Set Parse Expression
(define parse-exp
  (lambda (datum)    
    (cond
      [(symbol? datum) (var-exp datum)]
      [(literal? datum) (lit-exp ((parse-literal parse-exp) datum))]
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

(parse-exp (quote '()))
(parse-exp (quote '#(1 2)))

; Unparse Let
(define unparse-let
  (lambda (unparse-exp let-type)
    (lambda (name params body)
      (if (not (equal? #f name))
          (append (list let-type name (map (lambda (o) (list (car o) (unparse-exp (cadr o)))) params)) (map unparse-exp body))
          (append (list let-type (map (lambda (o) (list (car o) (unparse-exp (cadr o)))) params)) (map unparse-exp body))))))



; Unparse If
(define unparse-if
  (lambda (unparse-exp)
    (lambda (if-exp true-exp false-exp)
      (if (equal? #f false-exp)
          (list 'if (unparse-exp if-exp) (unparse-exp true-exp))
          (list 'if (unparse-exp if-exp) (unparse-exp true-exp) (unparse-exp false-exp))))))


; Unparse
(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [lit-exp (data) data]
      [lambda-exp (params body) (append (list 'lambda params) (map unparse-exp body))]
      [app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))]
      [let-exp (name params body) ((unparse-let unparse-exp 'let) name params body)]
      [let*-exp (name params body) ((unparse-let unparse-exp 'let*) name params body)]
      [letrec-exp (name params body) ((unparse-let unparse-exp 'letrec) name params body)]
      [if-exp (if-exp true-exp false-exp) ((unparse-if unparse-exp) if-exp true-exp false-exp)]
      [else (parse-error exp "bad expression: ~s")])))


; An auxiliary procedure that could be helpful.
(define var-exp?
  (lambda (x)
    (cases expression x
      [var-exp (id) #t]
      [else #f])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))





(define foo (make-vector 1 2))
(display foo)
(let ((a foo))
  (vector-set! a 0 0)
  (display a))
(display foo)





