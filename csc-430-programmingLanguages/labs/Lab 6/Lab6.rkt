#lang typed/racket
 (require typed/rackunit)

(define-type ExprC (U NumC AppC IdC IfC LamC StrC))
(struct NumC ([n : Real])#:transparent)
(struct StrC ([s : String]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)])#:transparent)
(struct IdC ([sym : Symbol])#:transparent)
(struct IfC ([a : ExprC] [t : ExprC] [f : ExprC])#:transparent)
(struct LamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)

; 1. A function that produces a random symbol from a fixed set
(define symbol-set '(a b c d e f g h))
(define (random-symbol) : Symbol
  (list-ref symbol-set (random 8)))

; 2. A function that produces a random expression that does not contain any other expressions
(define (random-base-term) : ExprC
  (match (random 3)
    [0 (NumC (random 10))]
    [1 (IdC (random-symbol))]
    [2 (StrC "string")]))

; 3. A function that accepts a max depth and produces an expression tree whose depth does
;    not exceed the given one
(define (random-term [max_depth : Integer]) : ExprC
  (cond
    [(<= max_depth 0) (random-base-term)]
    [else (match (random 4)
        [0 (random-base-term)]
        [1 (IfC (random-term (- max_depth 1))
                 (random-term (- max_depth 1))
                 (random-term (- max_depth 1)))]
        [2 (LamC (list (random-symbol))
                 (random-term (- max_depth 1)))]
        [3 (AppC (random-term (- max_depth 1))
                 (list(random-term (- max_depth 1))))]
    )]
  ))

; 4. A function that takes a parsed expression and produces concrete syntax that corresponds
(define (unparse [expr : ExprC]) : Sexp
  (match expr
    [(NumC n) (n)]
    [(IdC id) (id)]
    [(StrC s) (s)]
    [(AppC fun args) ((unparse fun) (map unparse args))]
    [(IfC a t f) ((unparse a) (unparse t) (unparse f))]
    [(LamC args body) ((map symbol->string args) (unparse body))]))

; 5. A function that generates a random term, prints its concrete syntax, and returns the term
(define (quiz)
  (define random-expression (random-term 4))
  (define syntax (unparse random-expression))
  
  (displayln "Generated Expression Unparsed:")
  (displayln syntax)
  
  random-expression)

; Random Test Cases:
(define expr1 (NumC 42))
(unparse expr1)

(define expr2 (StrC "hello"))
(unparse expr2)

(define expr3 (IfC (NumC 1) (NumC 2) (NumC 3)))
(unparse expr3)






