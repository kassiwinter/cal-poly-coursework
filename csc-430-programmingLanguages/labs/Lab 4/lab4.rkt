#lang racket
 (require rackunit)

; Exercise 3 - A function that takes in a number, and returns
; another function that adds the num with another number
; Ex: (number -> (number -> number))
(define (curried-add a)
  (lambda (b)
    (+ a b))
  )

(define add+ (curried-add 1))
(check-equal? (add+ 2) 3)

(define add0 (curried-add 0))
(check-equal? (add0 1) 1)

(define add- (curried-add -2))
(check-equal? (add- 0) -2)


; Exercise 4 - A function that takes in a function of two arguments
; and produces another function of a function
; Ex: (All (a b c) ((a b -> c) -> (a -> (b -> c))))
(define (curry2 fun)
  (lambda (a)
    (lambda (b)
      (fun a b))))

(define add2 (lambda (x y) (+ x y)))
(define curry2-add (curry2 add2))

(check-equal? ((curry2-add 1) 2) 3)
(check-equal? ((curry2-add 0) 1) 1)
(check-equal? ((curry2-add 0) -1) -1)


; Exercise 5 - A function that takes in a function of three arguments
; and produces another function of a function of a function
; Ex: (All (a b c d) ((a b c -> d) -> (a -> (b -> (c -> d)))))
(define (curry3 fun)
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (fun a b c)))))

(define add3 (lambda (x y z) (+ x y z)))
(define curry3-add (curry3 add3))

(check-equal? (((curry3-add 1) 2) 3) 6)
(check-equal? (((curry3-add 0) 0) 0) 0)
(check-equal? (((curry3-add -1) -1) -1) -3)

; Exercise 6 -
; A. A function that consumes a list and a symbol, returning
; true when the symbol occurs in the list
(define (contains? lst sym)
  (cond
    [(empty? lst) #f]
    [(eq? (first lst) sym) #t]
    [else (contains? (rest lst) sym)]))

(check-equal? (contains? '() 1) #f)
(check-equal? (contains? '(1 2 3) 1) #t)
(check-equal? (contains? '(1 2 3) 0) #f)
(check-equal? (contains? '(hi hey hello) 'hi) #t)
(check-equal? (contains? '(hi hey hello) 'hoyeah) #f)

; B. A function that consumes a source list of symbols and list of query
; symbols, returning a list of booleans indicating corresponding elements
(define (in-list-many? source-list query-list)
  (map (lambda (query)(contains? source-list query)) query-list))
;

(define source-list (list 'a 'b 'c))
(define query-list (list 'a 'd))
(check-equal? (in-list-many? source-list query-list) (list #t #f))
