#lang racket
 (require rackunit)

; 2.1
; A function that accepts a function and an arg and applies the function to the result of
; applying the function to the argument
(define (two f x)
  (f (f x)))


; A function that adds one to an input variable; for testing purposes only
(define (add-one x)
  (+ x 1))
(check-equal? (two add-one 3) 5)
(check-equal? (two add-one 0) 2)
(check-equal? (two add-one -5) -3)

; 2.2
; A function that accepts a function and an arg and returns the arg
(define (zero f x)
  x)
(check-equal? (zero add-one 3) 3)
(check-equal? (zero add-one 0) 0)
(check-equal? (zero add-one -5) -5)


; 2.3 
; A function that accepts a function and returns a new function that does the
; function an additional time
(define (add1 num-func)
  (lambda (func_arg arg)
    (func_arg (num-func func_arg arg))))

(check-equal? ((add1 two) (lambda (a) (+ a 3)) 3) 12)
(check-equal? ((add1 zero) (lambda (a) (+ a 3)) 3) 6)

; 2.4
; A function ’ that accepts two functions and returns a function that applies
; its first argument to its second argument a number of times that corresponds
; to the sum of the two ’numbers’ it was given
(define (add f1 f2)
  (lambda (f x)
    (f1 f (f2 f x))))

(define three (add zero two))
(check-equal? (three add-one 3) 5)
(check-equal? (three add-one 5) 7)
(check-equal? (three add-one 0) 2)
(check-equal? (three (lambda (x) (* x 2)) 2) 8)

; 2.5
; A function that accepts two arguments and returns the first one

(define (tru x y)
  x)

(check-equal? (tru 3 5) 3)
(check-equal? (tru "apple" "banana") "apple")
(check-equal? (tru 0 42) 0)
(check-equal? (tru 'a 'b) 'a)

; 2.6
; A function that accepts two arguments and returns the second one

(define (fals x y)
  y)

(check-equal? (fals 3 5) 5)
(check-equal? (fals "apple" "banana") "banana")
(check-equal? (fals 0 42) 42)
(check-equal? (fals 'a 'b) 'b)

; 2.7
; A function that acepts three arguments, returns the first option if the
; condition is tru and the seconf if the condtion is fals

(define (if function arg1 arg2)
  (function arg1 arg2))

(check-equal? (if tru 42 3) 42)
(check-equal? (if fals 42 3) 3)

; 2.8
; Confused

