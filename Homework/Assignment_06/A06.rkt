#lang racket
 (require racket/trace)

(provide curry2 curried-compose compose make-list-c reverse-it map-by-position empty-BST empty-BST? BST-insert BST-inorder BST? BST-element BST-left BST-right BST-insert-nodes BST-contains? BST-height let->application let*->let qsort sort-list-of-symbols)

(define curry2
  (lambda (fnc)
    (lambda (a)
      (lambda (b)
        (fnc a b)))))

(define curried-compose
  (lambda (fnc1)
    (lambda (fnc2)
      (lambda (a)
        (fnc1 (fnc2 a))))))

(define compose
  (lambda fncs
    (cond [(equal? '1 (length fncs))
           (lambda (a)
             ((car fncs) a))]
          [else (lambda (a)
             ((car fncs) ((apply compose (cdr fncs)) a)))])))

(define make-list-c
  (lambda (len)
    (cond [(< len 1) (lambda (a) '())]
          [(equal? '1 len) (lambda (a) (list a))]
          [else (lambda (a) (cons a ((make-list-c (- len 1)) a)))])))


(define reverse-it-helper
  (lambda (a b)
    (cond [(equal? 0 (length a)) b]
          [else (reverse-it-helper (cdr a) (cons (car a) b))])))


(define reverse-it
  (lambda (a)
    (reverse-it-helper a '())))


(define map-by-position
  (lambda (a b)
    (map (lambda (c d) (c d)) a b)))

(define empty-BST
  (lambda () '()))

(define empty-BST?
  (lambda (a)
    (cond [(equal? '0 (length a)) true]
         [else false])))

(define BST-insert
  (lambda (num tree)
    (cond [(empty-BST? tree) (list num '() '())]
          [(< num (BST-element tree)) (list (BST-element tree) (BST-insert num (BST-left tree)) (BST-right tree))]
          [else (list (BST-element tree) (BST-left tree) (BST-insert num (BST-right tree)))])))

(define BST-inorder
  (lambda (a)
    (cond [(empty-BST? a) '()]
          [(and (not (empty-BST? (BST-left a))) (equal? (BST-element a) (BST-element (BST-left a)))) (append (BST-inorder (BST-left a))  (BST-inorder (BST-right a)))]
          [(and (not (empty-BST? (BST-right a))) (equal? (BST-element a) (BST-element (BST-right a)))) (append (BST-inorder (BST-left a))  (BST-inorder (BST-right a)))]
          [else (append (BST-inorder (BST-left a)) (cons (BST-element a) (BST-inorder (BST-right a))))])))




(define BST-helper?
  (lambda (a treeMin treeMax)
    (cond [(not (list? a)) false]
          [(empty-BST? a) true]
          [(not (equal? 3 (length a))) false]
          [(not (integer? (BST-element a))) false]
          [(not (list? (BST-left a))) false]
          [(not (list? (BST-right a))) false]
          [(< treeMax (BST-element a)) false]
          [(> treeMin (BST-element a)) false]
          [else (and (BST-helper? (BST-left a) treeMin (BST-element a)) (BST-helper? (BST-right a) (BST-element a) treeMax))])))

(define BST?
  (lambda (a)
    (BST-helper? a -999999999 999999999)))

(define BST-element
  (lambda (a)
    (car a)))

(define BST-left
  (lambda (a)
    (cadr a)))

(define BST-right
  (lambda (a)
    (caddr a)))


(define BST-insert-nodes
  (lambda (tree nums)
    (cond [(equal? 0 (length nums)) tree]
          [else (BST-insert-nodes (BST-insert (car nums) tree) (cdr nums))])))

(define BST-contains?
  (lambda (tree num)
    (cond [(empty-BST? tree) false]
          [(equal? (BST-element tree) num) true]
          [(< num (BST-element tree)) (BST-contains? (BST-left tree) num)]
          [else (BST-contains? (BST-right tree) num)])))


(define BST-height-helper
  (lambda (tree height)
    (cond [(empty-BST? tree) height]
          [else (max (BST-height-helper (BST-left tree) (add1 height)) (BST-height-helper (BST-right tree) (add1 height)))])))

(define BST-height
  (lambda (a)
    (BST-height-helper a -1)))


(define let->application
  (lambda (a)
    (cons (list 'lambda (map car (cadr a)) (caddr a)) (map cadr (cadr a)))))


(define let*->let-helper
  (lambda (args value)
    (cond [(equal? 1 (length args)) (list 'let (list (car args)) value)]
          [else (list 'let (list (car args)) (let*->let-helper (cdr args) value))])))

(define let*->let
  (lambda (a)
    (let*->let-helper (cadr a) (caddr a))))


(define qsort
  (lambda (predicate items)
    (cond [(> 2 (length items)) items]
          [else (let ((pivot (car items)) (items (cdr items)))
                  (append (qsort predicate (filter (lambda (o) (predicate o pivot)) items))
                          (cons pivot '())
                          (qsort predicate (filter (lambda (o) (not (predicate o pivot))) items))))])))

(define sort-list-of-symbols
  (lambda (a)
    (map string->symbol (qsort string<? (map symbol->string a)))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
