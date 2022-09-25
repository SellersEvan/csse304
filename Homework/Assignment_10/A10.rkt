#lang racket
(require racket/trace)
(provide free-vars bound-vars lexical-address un-lexical-address convert-multip-calls convert-multip-lambdas convert-ifs)
; Evan Sellers

(define free-vars
  (lambda (a)
    (remove-duplicates (let free-vars-helper ([var #false] [exp a])
      (cond [(symbol? exp) (if (equal? var exp)
                               '()
                               (list exp))]
            [(equal? (car exp) 'lambda) (append '() (free-vars-helper (car (cadr exp)) (caddr exp)))]
            [else (append (free-vars-helper var (cadr exp)) (free-vars-helper var (car exp)) (free-vars-helper #false (cadr exp)))])))))



(define bound-vars
  (lambda (a)
    (remove-duplicates (let bound-vars-helper ([var #false] [exp a])
      (cond [(symbol? exp) (if (equal? var exp)
                               (list exp)
                               '())]
            [(equal? (car exp) 'lambda) (append (bound-vars-helper var (caddr exp)) (bound-vars-helper (car (cadr exp)) (caddr exp)))]
            [else (append (bound-vars-helper var (car exp)) (bound-vars-helper var (cadr exp)) (bound-vars-helper #false (cadr exp)))])))))


(define convert-multip-calls
  (lambda (exp)
    (cond [(symbol? exp) exp]
          [(equal? (car exp) 'lambda) (list 'lambda (cadr exp) (convert-multip-calls (caddr exp)))]
          [(equal? 1 (length exp)) (convert-multip-calls (car exp))]
          [(equal? 2 (length exp)) (list (convert-multip-calls (car exp)) (convert-multip-calls (cadr exp)))]
          [else (convert-multip-calls (cons (list (convert-multip-calls (car exp)) (convert-multip-calls (cadr exp))) (cddr exp)))])))


(define convert-multip-lambdas
  (lambda (exp)
    (cond [(symbol? exp) exp]
          [(empty? exp) '()]
          [(equal? (car exp) 'lambda) (if (equal? 1 (length (cadr exp)))
                                          exp
                                          (list 'lambda (list (caadr exp)) (convert-multip-lambdas (list 'lambda (cdr (cadr exp)) (convert-multip-lambdas (caddr exp))))))]
          [else (cons (convert-multip-lambdas (car exp)) (convert-multip-lambdas (cdr exp)))])))


(define convert-ifs
  (lambda (exp)
    (cond [(symbol? exp) exp]
          [(equal? exp #true) '(lambda (thenval elseval) thenval)]
          [(equal? exp #false) '(lambda (thenval elseval) elseval)]
          [(empty? exp) exp]
          [(equal? 'if (car exp)) (list (convert-ifs (cadr exp)) (convert-ifs (caddr exp)) (convert-ifs (cadddr exp)))]
          [(equal? 'lambda (car exp)) (list 'lambda (cadr exp) (convert-ifs (caddr exp)))]
          [else (cons (convert-ifs (car exp)) (convert-ifs (cdr exp)))])))


(define index-in-list
  (lambda (alist item)
    (let index-in-list-helper ([alist alist] [item item] [index 0])
      (cond [(null? alist) '(-1)]
            [(not (list? alist)) '(-1)]
            [(equal? item (car alist)) (list index)]
            [(and (list? (car alist)) (< -1 (car (index-in-list (car alist) item)))) (cons index (index-in-list (car alist) item))]
            [else (index-in-list-helper (cdr alist) item (add1 index))]))))


(define lexical-address
  (lambda (a)
    (let lexical-address-helper ([exp a] [params '()] [depth -1])
    (cond [(symbol? exp) (let ([indexInList (index-in-list params exp)])
                           (cond [(equal? -1 (car indexInList)) (list ': 'free exp)]
                                 [else (list ': (car indexInList) (cadr indexInList))]))]
          [(empty? exp) '()]
          [(equal? 'if (car exp)) (cons 'if (lexical-address-helper (cdr exp) params depth))]
          [(equal? 'let (car exp)) (list 'let
                                         (map (lambda (o) (list (car o) (lexical-address-helper (cadr o) params depth))) (cadr exp))
                                         (lexical-address-helper (caddr exp) (cons (map car (cadr exp)) params) depth))]
          [(equal? 'lambda (car exp)) (list (car exp) (cadr exp) (lexical-address-helper (caddr exp) (cons (cadr exp) params) (add1 depth)))]
          [else (cons (lexical-address-helper (car exp) params depth) (lexical-address-helper (cdr exp) params depth))]))))


(define un-lexical-address
  (lambda (a)
    (let un-lexical-address-helper ([exp a] [params '()])
    (cond [(or (symbol? exp) (empty? exp)) exp]
          [(equal? ': (car exp)) (if (equal? 'free (cadr exp)) (caddr exp) (list-ref (list-ref params (cadr exp)) (caddr exp)))]
          [(equal? 'if (car exp)) (cons 'if (un-lexical-address-helper (cdr exp) params))]
          [(equal? 'let (car exp)) (list 'let
                                         (map (lambda (o) (list (car o) (un-lexical-address-helper (cadr o) params))) (cadr exp))
                                         (un-lexical-address-helper (caddr exp) (cons (map car (cadr exp)) params)))]
          [(equal? 'lambda (car exp)) (list (car exp) (cadr exp) (un-lexical-address-helper (caddr exp) (cons (cadr exp) params)))]
          [else (cons (un-lexical-address-helper (car exp) params) (un-lexical-address-helper (cdr exp) params))]))))


(define make-counter
  (lambda ()
  (let ([counter 0])
    (lambda ()
      (set! counter (add1 counter))
      counter))))

(define c1 (make-counter))
(define c2 (make-counter))

(c1)
(c2)
(c1)
;(apply and (map symbol? '(1 2 3 foo)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
