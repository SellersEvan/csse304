#lang racket

(require racket/trace)

(provide sn-list-recur sn-list-sum sn-list-map sn-list-paren-count sn-list-reverse sn-list-occur sn-list-depth bt-recur bt-sum bt-inorder)

(define sn-list-recur
  (lambda (baseValue isListProcedure isNotListProcedure)
    (lambda (snList)
      (let sn-list-recur-helper ([snList snList] [baseValue baseValue] [isListProcedure isListProcedure] [isNotListProcedure isNotListProcedure])
        (cond [(null? snList) (baseValue)]
              [(or (symbol? snList) (number? snList)) (isNotListProcedure snList)]
              [else (isListProcedure (sn-list-recur-helper (car snList) baseValue isListProcedure isNotListProcedure) (sn-list-recur-helper (cdr snList) baseValue isListProcedure isNotListProcedure))])))))


(define sn-list-sum
  (lambda (snList)
    (define snListProc (sn-list-recur
                        (lambda () 0)
                        (lambda (a b) (+ a b))
                        (lambda (a) a)))
    (snListProc snList)))


(define sn-list-map
  (lambda (proc snList)
    (define snListProc (sn-list-recur
                        (lambda () '())
                        (lambda (a b) (cons a b))
                        (lambda (a) (proc a))))
    (snListProc snList)))


(define sn-list-paren-count
  (lambda (snList)
    (define snListProc (sn-list-recur
                        (lambda () '2)
                        (lambda (a b) (+ a b))
                        (lambda (a) '0)))
    (snListProc snList)))



(define sn-list-reverse
  (lambda (snList)
    (define snListProc (sn-list-recur
                        (lambda () '())
                        (lambda (a b) (append b (list a)))
                        (lambda (a) a)))
    (snListProc snList)))


(define sn-list-occur
  (lambda (pattern snList)
    (define snListProc (sn-list-recur
                        (lambda () '0)
                        (lambda (a b) (+ a b))
                        (lambda (a) (if (equal? a pattern) '1 '0))))
    (snListProc snList)))


(define sn-list-depth
  (lambda (snList)
    (define snListProc (sn-list-recur
                        (lambda () '1)
                        (lambda (a b) (max (+ a 1) b))
                        (lambda (a) '0)))
    (snListProc snList)))



(define bt-recur
  (lambda (isTreeProcedure isNumberProcedure)
    (lambda (tree)
      (let bt-recur-helper ([tree tree] [isTreeProcedure isTreeProcedure] [isNumberProcedure isNumberProcedure])
        (cond [(number? tree) (isNumberProcedure tree)]
              [else (isTreeProcedure (car tree) (bt-recur-helper (cadr tree) isTreeProcedure isNumberProcedure) (bt-recur-helper (caddr tree) isTreeProcedure isNumberProcedure))])))))

(define bt-sum
  (lambda (tree)
    (define treeProc (bt-recur
                        (lambda (elm tl tr) (+ tl tr) )
                        (lambda (num) num)))
    (treeProc tree)))


(define bt-inorder
  (lambda (tree)
    (define treeProc (bt-recur
                        (lambda (elm tl tr) (append tl (list elm) tr) )
                        (lambda (num) '())))
    (treeProc tree)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
