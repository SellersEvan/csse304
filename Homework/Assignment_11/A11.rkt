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

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (data number?)]
  [lambda-exp
   (id (lambda (o) (or (symbol? o) (and (list? o) (andmap symbol? o)))))
   (body expression?)]
  [app-exp
   (rator expression?)
   (rand (lambda (o) (and (list? o) (andmap expression? o))))]
  )


; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)


(define parse-exp         
  (lambda (datum)
   (cond
      [(symbol? datum) (var-exp datum)]
      [(number? datum) (lit-exp datum)]
      [(list? datum) (cond [(equal? (car datum) 'lambda)
                            (if (and (< 2 (length datum)) (or (symbol? (2nd datum)) (andmap symbol? (2nd datum))))
                               (lambda-exp (2nd datum) (parse-exp (cddr datum)))
                                (error 'parse-exp "bad expression: ~s" datum))]
                           [(equal? (car datum) 'if)
                            '(var-exp xxx)]
                           [(equal? (car datum) 'let)
                            '(var-exp xxx)]
                           [(equal? (car datum) 'letrec)
                            '(var-exp xxx)]
                           [(equal? (car datum) 'let*)
                            '(var-exp xxx)]
                           [(equal? (car datum) 'set!)
                            '(var-exp xxx)]
                           [else (let ([sym (parse-exp (1st datum))] [parms (map parse-exp (cdr datum))])
                                                   (if (and sym (andmap (lambda (o) o) parms))
                                                       (app-exp sym parms)
                                                       (error 'parse-exp "bad expression: ~s" datum)))])]
      [else (error 'parse-exp "bad expression: ~s" datum)])))



;(parse-exp '(+ 2 3))
;(parse-exp (quote (lambda (x) (+ 2 3))))
(parse-exp (quote (lambda x y z)))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [else 'x])))



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
