#lang racket
(require racket/trace)
;Evan Sellers
(provide make-slist-leaf-iterator subst-leftmost)



;(define slist-car
;  (lambda (a)
;    (cond [(not (list? a)) a]
;          [(equal? 0 (length a)) false]
;          [(slist-car (car a)) (slist-car (car a))]
;          [else (slist-car (cdr a))])))


;(define slist-not-empty?
;  (lambda (a)
;    (cond [(null? a) false]
;          [(not (list? a)) true]
;          [(equal? 0 (length a)) false]
;          [else (or (slist-not-empty? (car a)) (slist-not-empty? (cdr a)))])))


;(define slist-cdr
;  (lambda (a)
;    (cond [(equal? 0 (length a)) '()]
;          [(not (list? (car a))) (cdr a)]
;          [(equal? 0 (length (car a))) (slist-cdr (cdr a))]
;          [(slist-not-empty? (car a)) (append (slist-cdr (car a)) (cdr a))]
;          [else (slist-cdr (cdr a))])))

;(define make-slist-leaf-iterator
;  (lambda (a)
;      (lambda (cmd)
;        (cond [(equal? 'next cmd)
;               (define result (slist-car a))
;               (set! a (slist-cdr a)) result]
;              [else false]))))


(define make-stack
  (lambda (stack)
    (let ([stk stack])
      (lambda (msg . args ) 
        (case msg ; Scheme's case is a similar to switch in some other languages.
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


(define subst-leftmost-helper
  (trace-lambda (replace pattern slist predicate)
    (cond [(symbol? slist)
           (if (predicate pattern slist)
               (list replace #t)
               (list slist #f))]
          [else (let* ([replace replace] [pattern pattern] [predicate predicate] [next-obj (subst-leftmost-helper replace pattern (car slist) predicate)] [carSym (car next-obj)] [hasMatch? (cadr next-obj)])
                  (cond [hasMatch? (list (cons carSym (cdr slist)) #t)]
                        [else (let* ([carSym carSym] [next-obj (subst-leftmost-helper replace pattern (car slist) predicate)] [cdrSym (car next-obj)] [hasMatch? (cadr next-obj)])
                                (cond [hasMatch? (list (cons carSym cdrSym) #t)]
                                      [else (list (cons carSym cdrSym) #f)]))]))])))


(subst-leftmost-helper 'a 'b '(c a b) equal?)


(define subst-leftmost
  (lambda (replace pattern slist predicate)
    (nyi)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
 