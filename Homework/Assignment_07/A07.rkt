#lang racket
(require racket/trace)

; Evan Sellers

(provide vector-append-list group-by-two group-by-n bt-leaf-sum bt-inorder-list bt-max bt-max-interior slist-map slist-reverse slist-paren-count slist-depth slist-symbols-at-depth path-to make-c...r)



(define list-fill-vector
  (lambda (sourceList newList sourceIndex newIndex)
    (cond [(<= (vector-length sourceList) sourceIndex) false]
          [(<= (vector-length newList) newIndex) false]
          [(not (or (vector? (vector-ref sourceList 0)) (list? (vector-ref sourceList 0))))
             (vector-set! newList newIndex (vector-ref sourceList sourceIndex))
             (list-fill-vector sourceList newList (add1 sourceIndex) (add1 newIndex))]
          [else
             (vector-set! newList newIndex (vector->list (vector-append-list (vector-ref sourceList sourceIndex) '())))
             (list-fill-vector sourceList newList (add1 sourceIndex) (add1 newIndex))])))


(define list-fill-list
  (lambda (sourceList newList newIndex)
    (cond [(equal? 0 (length sourceList)) false]
          [(null? (car sourceList)) false]
          [(<= (vector-length newList) newIndex) false]
          [(not (or (vector? (car sourceList)) (list? (car sourceList))))
             (vector-set! newList newIndex (car sourceList))
             (list-fill-list (cdr sourceList) newList (add1 newIndex))]
          [else
             (vector-set! newList newIndex (vector->list (vector-append-list #() (car sourceList))))
             (list-fill-list (cdr sourceList) newList (add1 newIndex))])))


(define vector-append-list
  (lambda (a b)
    (let* ([newList (make-vector (+ (vector-length a) (length b)))])
      (list-fill-vector a newList 0 0)
      (list-fill-list b newList (vector-length a))
      newList)))


(define group-by-two
  (lambda (a)
    (group-by-n a 2)))

(define group-by-n
  (lambda (a n)
    (let group-by-two-helper ([sourceList a] [n n] [index 0])
      (cond [(equal? sourceList '()) '()]
            [(null? (cdr sourceList)) (list sourceList)]
            [(= (modulo (add1 index) n) 0) (cons (list (car sourceList)) (group-by-two-helper (cdr sourceList) n (add1 index)))]
            [else (let ([next (group-by-two-helper (cdr sourceList) n (add1 index))])
                                      (cons (cons (car sourceList) (car next)) (cdr next)))]))))


(define bt-leaf-sum
  (lambda (a)
    (cond [(number? a) a]
          [else (+ (bt-leaf-sum (cadr a)) (bt-leaf-sum (caddr a)))])))

(define bt-inorder-list
  (lambda (a)
    (cond [(not (list? a)) '()]
          [(and (number? (cadr a)) (number? (caddr a))) (list (car a))]
          [else (append (bt-inorder-list (cadr a)) (list (car a)) (bt-inorder-list (caddr a)))])))


(define bt-max
  (lambda (a)
    (cond [(number? a) a]
          [else (max (bt-max (cadr a)) (bt-max (caddr a)))])))


; (maxElm maxCount curCount)
(define bt-max-interior
  (lambda (a)
    (car (let  bt-max-interior-helper ([tree a])
      (if (not (list? tree)) (list -1 -999999999 tree)
          (let* ([elm (car tree)]
                 [right (bt-max-interior-helper (caddr tree))]
                 [left (bt-max-interior-helper (cadr tree))]
                 [curTotal (+ (caddr left) (caddr right))])
                   (cond [(and (> curTotal (cadr left)) (> curTotal (cadr right))) (list elm curTotal curTotal)]
                         [(< (cadr right) (cadr left)) (list (car left) (cadr left) curTotal)]
                         [(< (cadr left) (cadr right)) (list (car right) (cadr right) curTotal)]
                         [else (display elm) (list (car left) (cadr left) curTotal)])))))))


(define slist-map
  (lambda (a b)
    (let slist-map-helper ([predicate a] [items b])
      (map (lambda (item)
             (cond [(list? item) (slist-map-helper predicate item)]
                   [else (predicate item)])) items))))


(define slist-reverse
  (lambda (a)
    (let slist-reverse-helper ([slist a])
      (cond [(not (list? slist)) slist]
            [(> 2 (length slist)) slist]
            [else (append (slist-reverse-helper (cdr slist)) (list (slist-reverse-helper (car slist))))]))))


(define slist-paren-count
  (lambda (a)
    (let slist-paren-count-helper [(slist a)]
      (cond [(not (list? slist)) 0]
            [else (+ 2 (apply + (map slist-paren-count-helper slist)))]))))


(define slist-depth
  (lambda (a)
    (let slist-depth-helper [(slist a)]
      (cond [(not (list? slist)) 0]
            [else (+ 1 (apply max (map slist-depth-helper (cons '1 slist))))]))))


(define slist-symbols-at-depth
  (lambda (a b)
    (let slist-symbols-at-depth-helper [(slist a) (depth b)]
      (cond [(not (list? slist)) '()]
            [(= depth 1) (filter (lambda (o) (not (list? o))) slist)]
            [(< depth 1) '()]
            [else (apply append (map (lambda (item)
                                     (slist-symbols-at-depth-helper item (- depth 1))) slist))]))))


(define path-to
  (lambda (a b)
    (define path (let path-to-helper ([slist a] [pattern b])
      (cond [(equal? pattern slist) '()]
            [(not (list? slist)) '(#f)]
            [(equal? slist '()) '(#f)]
            [else (let ([chain (path-to-helper (car slist) pattern)])
                    (cond [(andmap (lambda (o) (not (equal? o false))) chain) (append '(car) (path-to-helper (car slist) pattern))]
                          [else (append '(cdr) (path-to-helper (cdr slist) pattern))]))])))
    (let ([isValidPath (andmap (lambda (o) (not (equal? o false))) path)][path path])
      (cond [(equal? true isValidPath) path]
            [else false]))))


(define make-c...r
  (lambda (a)
    (let make-c...r-helper ([pattern (string->list a)] [procedure (lambda (buffer) buffer)])
      (cond [(equal? 0 (length pattern)) procedure]
            [(char=? '#\a (car pattern)) (make-c...r-helper (cdr pattern) (lambda (buffer) (procedure (car buffer))))]
            [else (make-c...r-helper (cdr pattern) (lambda (buffer) (procedure (cdr buffer))))]))))



;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
