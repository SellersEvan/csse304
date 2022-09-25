#lang racket
(require racket/trace)
;Evan Sellers
(provide make-slist-leaf-iterator subst-leftmost)



(define make-stack
  (lambda (stack)
    (let ([stk stack])
      (lambda (msg . args ) 
        (case msg
          [(empty?) (null? stk)]
          [(push) (set! stk (cons (car args) stk))]
          [(pop) (let ([top (car stk)])
                   (set! stk (cdr stk))
                   top)]
          [else (display 'stack "illegal message to stack object: ~a" msg)])))))


(define make-slist-leaf-iterator
  (lambda (slist)
    (let ([slistStack (make-stack slist)])
      (lambda (msg . args)
        (case msg
          [(next) (let slist-cdr-helper ([slistStack slistStack])
                     (if (slistStack 'empty?) false
                       (let ([item (slistStack 'pop)])
                         (cond [(not (list? item)) item]
                               [(equal? 0 (length item)) (slist-cdr-helper slistStack)]
                               [else (slistStack 'push (cdr item))
                                     (slistStack 'push (car item))
                                     (slist-cdr-helper slistStack)]))))])))))


(define subst-leftmost
  (lambda (replace pattern slist predicate)
    (car (let subst-leftmost-helper ([replace replace] [pattern pattern] [slist slist] [predicate predicate])
           (cond [(symbol? slist)
                  (if (predicate pattern slist)
                      (list replace #t)
                      (list slist #f))]
                 [(null? slist) (list slist #f)]
                 [else (let* ([replace replace] [pattern pattern] [predicate predicate] [next-obj (subst-leftmost-helper replace pattern (car slist) predicate)] [carSym (car next-obj)] [hasMatch? (cadr next-obj)])
                         (cond [hasMatch? (list (cons carSym (cdr slist)) #t)]
                               [else (let* ([carSym carSym] [next-obj (subst-leftmost-helper replace pattern (cdr slist) predicate)] [cdrSym (car next-obj)] [hasMatch? (cadr next-obj)])
                                       (cond [hasMatch? (list (cons carSym cdrSym) #t)]
                                             [else (list (cons carSym cdrSym) #f)]))]))])))))


;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
 