#lang racket

; To use these tests:
; Click "Run" in the upper right
; (r)

; If you find errors in your code, fix them, save your file, click the "Run" button again, and type (r)
; You can run a specific group of tests using (run-tests group-name)

(require "../testcode-base.rkt")
(require "interpreter.rkt")
(provide get-weights get-names individual-test test)



(define test (make-test ; (r)
  (literals equal? ; (run-test literals)
    [(eval-one-exp ''()) '() 1] ; (run-test literals 1)
    [(eval-one-exp #t) #t 1] ; (run-test literals 2)
    [(eval-one-exp #f) #f 1] ; (run-test literals 3)
    [(eval-one-exp "") '"" 1] ; (run-test literals 4)
    [(eval-one-exp "test") '"test" 1] ; (run-test literals 5)
    [(eval-one-exp ''#(a b c)) #(a b c) 1] ; (run-test literals 6)
    [(eval-one-exp ''#5(a)) #5(a) 1] ; (run-test literals 7)

    ; Added Tests
    [(eval-one-exp ''(1 2 3 4 (a b c (#t #f)))) '(1 2 3 4 (a b c (#t #f))) 1] ; (run-test literals 8)
    [(eval-one-exp 999) 999 1] ; (run-test literals 9)
    [(eval-one-exp -98) -98 1] ; (run-test literals 10)
    [(eval-one-exp 1.2) 1.2 1] ; (run-test literals 11)
  )

  (quote equal? ; (run-test quote)
    [(eval-one-exp '(quote ())) '() 1] ; (run-test quote 1)
    [(eval-one-exp '(quote a)) 'a 1] ; (run-test quote 2)
    [(eval-one-exp '(quote (car (a b)))) '(car (a b)) 1] ; (run-test quote 3)
    [(eval-one-exp '(quote (lambda (x) (+ 1 x)))) '(lambda (x) (+ 1 x)) 1] ; (run-test quote 4)
  )

  (if equal? ; (run-test if)
    [(eval-one-exp '(if #t 5 6)) 5 1] ; (run-test if 1)
    [(eval-one-exp '(if 2 (if #f 3 4) 6)) 4 1] ; (run-test if 2)
    [(eval-one-exp '(if #f 5 6)) 6 1] ; (run-test if 3)
    [(eval-one-exp '(if 1 2 3)) 2 1] ; (run-test if 4)
    [(let ((x (if #f 2 3))) (+ x 7)) 10 1] ; (run-test if 5)

    ; Added Tests
    [(eval-one-exp '(if "foo" #t #f)) (if "foo" #t #f) 1] ; (run-test if 6)
    [(eval-one-exp '(if "false" #t #f)) (if "false" #t #f) 1] ; (run-test if 7)
    [(eval-one-exp '(if 1.1 #t #f)) (if 1.1 #t #f) 1] ; (run-test if 8)
    [(eval-one-exp '(if -2 #t #f)) (if -2 #t #f) 1] ; (run-test if 9)
    [(eval-one-exp '(if 0 #t #f)) (if 0 #t #f) 1] ; (run-test if 10)
    [(eval-one-exp '(if '() #t #f)) (if '() #t #f) 1] ; (run-test if 11)
    [(eval-one-exp '(if '(1 2) #t #f)) (if '(1 2) #t #f) 1] ; (run-test if 12)
    [(eval-one-exp '(if '(#f) #t #f)) (if '(#f) #t #f) 1] ; (run-test if 13)
    [(eval-one-exp '(if '#() #t #f)) (if '#() #t #f) 1] ; (run-test if 14)
    [(eval-one-exp '(if (if #t 21 #f) 22 23)) (if (if #t 21 #f) 22 23) 1] ; (run-test if 15)
    [(eval-one-exp '(if (if #f 21 #f) (if #t 21 #f) 0)) (if (if #f 21 #f) (if #t 21 #f) 0) 1] ; (run-test if 16)
    [(eval-one-exp '(if (if #f 21 #f) 0 (if #t 21 #f))) (if (if #f 21 #f) 0 (if #t 21 #f)) 1] ; (run-test if 17)
  )

  (primitive-procedures equal? ; (run-test primitive-procedures)

                        
    [(eval-one-exp '(+ (+ 1 2) 3 4)) 10 1] ; (run-test primitive-procedures 1)
    [(eval-one-exp '(- 10 1 (- 5 3))) 7 1] ; (run-test primitive-procedures 2)
    [(eval-one-exp '(* 2 (* 3 4) 2)) 48 1] ; (run-test primitive-procedures 3)
    [(eval-one-exp '(/ 6 2)) 3 1] ; (run-test primitive-procedures 4)
    [(eval-one-exp '(sub1 (add1 10))) 10 1] ; (run-test primitive-procedures 5)
    [(eval-one-exp '(not (zero? 3))) #t 1] ; (run-test primitive-procedures 6)
    [(eval-one-exp '(= 3 4)) #f 1] ; (run-test primitive-procedures 7)
    [(eval-one-exp '(>= 4 3)) #t 1] ; (run-test primitive-procedures 8)
    [(eval-one-exp '(cons 'a 'b)) '(a . b) 1] ; (run-test primitive-procedures 9)
    [(eval-one-exp '(car (cdr '(a b c)))) 'b 1] ; (run-test primitive-procedures 10)
    [(eval-one-exp '(list 'a 'b 'c)) '(a b c) 1] ; (run-test primitive-procedures 11)
    [(eval-one-exp '(null? '())) #t 1] ; (run-test primitive-procedures 12)
    [(eval-one-exp '(eq? 'a 'a)) #t 1] ; (run-test primitive-procedures 13)
    [(eval-one-exp '(equal? 'a 'a)) #t 1] ; (run-test primitive-procedures 14)
    [(eval-one-exp '(length '(a b c d e))) 5 1] ; (run-test primitive-procedures 15)
    [(eval-one-exp '(list->vector '(a b c))) #(a b c) 1] ; (run-test primitive-procedures 16)
    [(eval-one-exp '(list? 'a)) #f 1] ; (run-test primitive-procedures 17)
    [(eval-one-exp '(pair? '(a b))) #t 1] ; (run-test primitive-procedures 18)
    [(eval-one-exp '(vector->list '#(a b c))) '(a b c) 1] ; (run-test primitive-procedures 19)
    [(eval-one-exp '(vector? '#(a b c))) #t 1] ; (run-test primitive-procedures 20)
    [(eval-one-exp '(number? 5)) #t 1] ; (run-test primitive-procedures 21)
    [all-or-nothing 2 ; (run-test primitive-procedures 22)
      ((eval-one-exp '(symbol? 'a)) #t)
      ((eval-one-exp '(symbol? 5)) #f)]
    [(eval-one-exp '(caar '((a b) c))) 'a 1] ; (run-test primitive-procedures 23)
    [(eval-one-exp '(cadr '((a b) c))) 'c 1] ; (run-test primitive-procedures 24)
    [(eval-one-exp '(cadar '((a b) c))) 'b 1] ; (run-test primitive-procedures 25)
    [(eval-one-exp '(list (procedure? list) (procedure? (lambda (x y) (list (+ x y)))) (procedure? 'list))) '(#t #t #f) 3] ; (run-test primitive-procedures 26)

    ; Added Tests
    [(eval-one-exp '(+)) 0 1] 
    [(eval-one-exp '(+ 1)) 1 1]
    [(eval-one-exp '(+ 3 4)) 7 1]
    [(eval-one-exp '(+ (+ 1 2) 4)) 7 1]
    [(eval-one-exp '(+ (+ 1 2) 3 4)) 10 1]
    [(eval-one-exp '(- 2)) -2 1]
    [(eval-one-exp '(*)) 1 1]
    [(eval-one-exp '(* 2)) 2 1]
    [(eval-one-exp '(/ 2)) 1/2 1]
    [(eval-one-exp '(not 1)) #f 1]
    [(eval-one-exp '(not #t)) #f 1]
    [(eval-one-exp '(not #f)) #t 1]
    [(eval-one-exp '(not 0)) #f 1]
    [(eval-one-exp '(not "foo")) #f 1]
    [(eval-one-exp '(not '(#f))) #f 1]
  )

  (let equal? ; (run-test let)
    [(eval-one-exp '(let ((a 3)(b 5)) (+ a b))) 8 3] ; (run-test let 1)
    [(eval-one-exp '(let ((a 3)) (let ((b 2) (c (+ a 3)) (a (+ a a))) (+ a b c)))) 14 5] ; (run-test let 2)
    [(eval-one-exp '(let ((a 3)) (let ((a (let ((a (+ a a))) (+ a a)))) (+ a a)))) 24 6] ; (run-test let 3)
    [(eval-one-exp '(let ((a (vector 0 1 2 3))) (vector-set! a 1 38) (vector-set! a 2 (vector-ref a 1)) (vector-ref a 2))) 38 6] ; (run-test let 4)
    [(eval-one-exp '(let ([compose2 (lambda (f g) (lambda (x) (f (g x))))]) (let ([h  (let ([g (lambda (x) (+ 1 x))] [f (lambda (y) (* 2 y))]) (compose2 g f))]) (h 4)))) 9 6] ; (run-test let 5)
  )

  (lambda equal? ; (run-test lambda)
    [(eval-one-exp '((lambda (x) (+ 1 x)) 5)) 6 5] ; (run-test lambda 1)
    [(eval-one-exp '((lambda (x) (+ 1 x) (+ 2 (* 2 x))) 5)) 12 7] ; (run-test lambda 2)
    [(eval-one-exp '((lambda (a b) (let ((a (+ a b)) (b (- a b))) (let ((f (lambda (a) (+ a b)))) (f (+ 3 a b))))) 56 17)) 154 15] ; (run-test lambda 3)
    ;[(eval-one-exp '(((lambda (f) ((lambda (x) (f (lambda (y) ((x x) y)))) (lambda (x) (f (lambda (y) ((x x) y)))))) (lambda (g) (lambda (n) (if (zero? n) 1 (* n (g (- n 1))))))) 6)) 720 11] ; (run-test lambda 4)
    ;[(eval-one-exp '(let ((Y (lambda (f) ((lambda (x) (f (lambda (y) ((x x) y)))) (lambda (x) (f (lambda (y) ((x x) y))))))) (H (lambda (g) (lambda (x) (if (null? x) '() (cons (procedure? (car x)) (g (cdr x)))))))) ((Y H) (list list (lambda (x) x) 'list)))) '(#t #t #f) 11] ; (run-test lambda 5)
  )
))

(implicit-run test) ; run tests as soon as this file is loaded
