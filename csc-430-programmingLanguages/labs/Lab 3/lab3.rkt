#lang typed/racket
(require typed/rackunit)

; -- 2. Exercises -- 

; 2.1 A function that accepts an s-expression and uses a single match pattern to return true
; for s-expressions that are a list containing a number, the symbol 'chris, and a symbol
; (in that order), false otherwise.

(define (parse000 [exp : Sexp]) : Boolean
  (match exp
    [(list (? number? n) 'chris (? symbol? s)) #t]
    [_ #f]))

(check-equal? (parse000 (list 5 'chris 'symbol)) #t)
(check-equal? (parse000 (list 5 'steve 'symbol)) #f)

; 2.2 A function that accepts an s-expression and uses the same pattern as above, but returns
; the symbol in case of success, and #f otherwise.

(define (parse001 [exp : Sexp]) : (U Symbol #f)
  (match exp
    [(list (? real? n) 'chris (? symbol? s)) s]
    [else #f]))

(check-equal? (parse001 (list 5 'chris 'symbol)) 'symbol)
(check-equal? (parse001 (list 5 'steve 'symbol)) #f)

; 2.3 A function that returns a list of numbers if the given s-expressions is of length three
; and has a list as its second element. On failure, it should return #f.

(define (parse002 [exp : Sexp]) : (U (Listof Real) #f)
  (match exp
    [(list _ (list (? real? nums) ...)  _) (cast nums (Listof Real))]
    [else #f]))

(check-equal? (parse002 (list 5 (list 6 5 4) 'symbol)) (list 6 5 4))
(check-equal? (parse002 (list 5 'joe 'symbol)) #f)

; 2.4 A function that accepts a value and returns the symbol 'okay if the input is a number,
; and uses error to signal.

(define (ohno [val : Any]) : 'okay
  (match val
    [(? number? n) 'okay]
    [else
     (error 'ohno "Expected a number, got ~e" val)]))
    
(check-exn (regexp (regexp-quote "Expected a number, got 'symbol")) (lambda () (ohno 'symbol)))
(check-equal? (ohno 3) 'okay)

; 2.5 Define the Arith language described in the textbook in Chapter 3
  (define-type ArithC (U numC plusC multC))
  (struct numC ([n : Real]) #:transparent)
  (struct plusC ([l : ArithC] [r : ArithC]) #:transparent)
  (struct multC ([l : ArithC] [r : ArithC]) #:transparent)

; 2.6 An evaluation method that evaluates an ArithC expression

(define (interp [a : ArithC]) : Real
      (match a
        [(numC n) n]
        [(plusC l r) (+ (interp l) (interp r))]
        [(multC  l r) (* (interp l) (interp r))]))

(check-equal? (interp (plusC (multC (numC 4) (numC 5)) (numC 3))) 23)

; 2.7 A function that accepts an ArithC, returning the number of additions it contains.

(define (num-adds [a : ArithC]) : Real
  (printf "~v~n" a)
  (match a
    [(numC n) 0]
    [(plusC l r) (+ 1 (+ (num-adds l) (num-adds r)))]
    [(multC  l r) (+ (num-adds l) (num-adds r))]))

(check-equal? (num-adds (plusC (multC (numC 4) (numC 5)) (numC 3))) 1)

; 2.8 A parser for the Arith Language

(define-type ArithS (U NumS PlusS MinusS MultS NegS))
(struct NumS ([n : Real]) #:transparent)
(struct PlusS ([l : ArithS] [r : ArithS]) #:transparent)
(struct MinusS ([l : ArithS] [r : ArithS]) #:transparent)
(struct MultS ([l : ArithS] [r : ArithS]) #:transparent)
(struct NegS ([a : ArithS]) #:transparent)

; A. parse1: maps Sexp to ArithS

(define (parse1 [l : Sexp]) : ArithS
  (match l
    [(? real? n) (NumS n)]
    [(list '+ a b) (PlusS (parse1 a) (parse1 b))]
    [(list '* a b) (MultS (parse1 a) (parse1 b))]
    [(list '- a b) (MinusS (parse1 a) (parse1 b))]
    [(list '- a) (NegS (parse1 a))]))

(check-equal? (parse1 '{+ {- {- 5 4}} {* 4 3}}) (PlusS (NegS (MinusS (NumS 5) (NumS 4))) (MultS (NumS 4) (NumS 3))))

; B. desugar: maps ArithS to ArithC

(define (desugar [exp : ArithS]) : ArithC
  (match exp
    [(NumS n) (numC n)]
    [(PlusS a b) (plusC (desugar a) (desugar b))]
    [(MultS a b) (multC (desugar a) (desugar b))]
    [(MinusS a b) (plusC (desugar a) (multC (numC -1) (desugar b)))]
    [(NegS a) (multC (numC -1) (desugar a))]))

(check-equal? (desugar (PlusS (NegS (MinusS (NumS 5) (NumS 4))) (MultS (NumS 4) (NumS 3)))) (plusC (multC (numC -1) (plusC (numC 5) (multC (numC -1) (numC 4)))) (multC (numC 4) (numC 3) )))

; 2.9 A function that accepts an s-expression, calling the parser, desugar, then interp function.
(define (top-interp [in : Sexp] )
 (interp(desugar(parse1 in))))

; 2.10 A function that maps an s-expressions directly to ArithC forms, by performing the
; desugaring in the parse function.

(define (parse2 [in : Sexp]) : ArithC
  (match in
    [(? real? n) (numC n)]
    [(list '+ a b) (plusC (parse2 a) (parse2 b))]
    [(list '* a b) (multC (parse2 a) (parse2 b))]
    [(list '- a b) (plusC (parse2 a) (multC (numC -1) (parse2 b)))]
    [(list '- a) (multC (numC -1) (parse2 a))]))

(check-equal? (parse2 '{+ {- {- 5 4}} {* 4 3}}) (plusC (multC (numC -1) (plusC (numC 5) (multC (numC -1) (numC 4)))) (multC (numC 4) (numC 3)
                                                                                                                            )))

; The approch that makes the most sense for this lab would make the most sense is 'parse2' because it combines the parsing and
; desugaring into one function rather than two. 