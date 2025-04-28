#lang typed/racket
(require typed/rackunit)

; Assignment 5
; Jacob Malone
; Arthur Zhao
; Kassi Winter

; ExprC Type Definitions
(define-type ExprC (U NumC BinopC AppC IdC ifleq0C))
(struct NumC ([n : Real])#:transparent)
(struct AppC ([fun : Symbol] [args : (Listof ExprC)])#:transparent)
(struct IdC ([sym : Symbol])#:transparent)
(struct BinopC ([op : Symbol] [l : ExprC] [r : ExprC])#:transparent)
(struct ifleq0C ([a : ExprC] [t : ExprC] [f : ExprC])#:transparent)
(struct Binding ([name : Symbol] [val : Real]))
(define-type Env (Listof Binding))
(define mt-env '())
(define extend-env cons) 

; Binop Functions and Hashmap
(define (Add [l : Real] [r : Real]) : Real
              (+ l r))
(define (Sub [l : Real] [r : Real]) : Real
              (- l r))
(define (Mult [l : Real] [r : Real]) : Real
              (* l r))
(define (Div [l : Real] [r : Real]) : Real
  (if (= r 0)
      (error 'Division "PAIG: divide by 0")
  (/ l r)))
(define BinopMap (hash
                  '+ Add
                  '- Sub
                  '* Mult
                  '/ Div))
(define (is-binop? [sym : Symbol]) : Boolean
  (hash-has-key? BinopMap sym))

(define keywords (append (hash-keys BinopMap) '(ifleq0? fun)))
(define (is-keyword? [sym : Symbol]) : Boolean
  (cond
    [(member sym keywords) #t]
    [else #f]))

; FunDef Definition
(struct FundefC [(name : Symbol) (args : (Listof Symbol)) (body : ExprC)] #:transparent)

; parse: takes in a s-expression and returns the s-expression parsed as an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    ; number
    [(? real? num) (NumC num)]
    ; variable
    [(? symbol? sym)
     (if (not (is-keyword? sym))
         (IdC sym)
         (error 'parse "PAIG: cannot use keyword as variable ~v" sym))]
    ; function call
    [(list (? symbol? id) (list args ...))
     ; check that id's not a keyword
     (if (not (is-keyword? id))
     (AppC id (map parse args))
     (error 'parse "PAIG: Cannot use keyword [~v] as function call" id))]
    ; Binop
    [(list (? symbol? op) a b)
     (if (is-binop? op)
     (BinopC op (parse a) (parse b))
     (error 'parse "PAIG: Operator not supported ~v" op)
     )]
    ; ifleq0?
    [(list 'ifleq0? a ': t 'else: f) (ifleq0C (parse a) (parse t) (parse f))]
    ; not supported
    [idk (error 'parse "PAIG: improperly formatted expression ~v" idk)]))

; parse-fundef: takes in a s-expression and parses the resultant FundefC
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'fun (list (? symbol? name) (list (? symbol? args) ...)) body)
     (cond
       ; function name is keyword
       [(is-keyword? name) (error 'parse-fundef "PAIG: cannot use keyword as name for function [~v]" name)]
       ; function has no args
       [(null? args) (FundefC name '() (parse body)) ]
       ; function argument is keyword
       [(andmap (lambda ([arg : Symbol]) (is-keyword? arg)) (cast args (Listof Symbol)))
        (error 'parse-fundef "PAIG: cannot use keyword [~v] as argument for function" args)]
       ; duplicate arguments
       [(check-duplicates args)
        (error 'parse-fundef "PAIG: cannot have duplicate arguments for function  [~v]" args)]
       ; good to make fundefC
       [else (FundefC name (cast args (Listof Symbol)) (parse body))])]
    [idk (error 'parse-fundef "PAIG: improperly formatted function definition ~v" idk)]))

; parse-prog: takes in a whole program as a s-expression and returns a list of FundefC
; TODO: perhaps check if function is already made? Call member on tail to check.
; Will run in n^2 time, but generally functions don't have too many args
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]
    ; at end of program
    ['() '()]
    ; not a proper program
    [other (error 'parse-prog "PAIG: improperly formatted program ~v" other)]))

; subst: given the Expr to sub, the var to replace, and the Expr to check for the var in,
; replaces the var with the expr
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
    (match in
    [(NumC n) in]
    [(IdC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(ifleq0C a t f) (ifleq0C (subst what for a) (subst what for t) (subst what for f))]
    [(AppC f args) (AppC f (map (lambda ([a : ExprC]) (subst what for a)) args))]
    [(BinopC sym l r) (BinopC sym (subst what for l)
                        (subst what for r))]))

; subst-args: given a list of symbols, a list of Exprs, and an Expr to check for vars,
; calls subst repeatedly to sub out all vars
(define (subst-args [exprs : (Listof ExprC)]  [vars : (Listof Symbol)]  [in : ExprC]) : ExprC
  (match* (vars exprs) 
    [((cons var r)(cons exp s)) (subst-args s r (subst exp var in )) ]
    [('() '()) in ]))

; interp-fns: takes in a list of FundefC and interprets the function main
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (match (get-fundef 'main funs)
    [(FundefC name arg body) (interp (subst (NumC 0) 'init body) funs)]))

; get-fundef: takes in a list of Fundefs and an id symbol and returns the Fundef that has that id
(define (get-fundef [id : Symbol] [funs : (Listof FundefC)]) : FundefC
  (match funs
    [(cons (FundefC name arg body) r)
     (if (equal? id name)
         (FundefC name arg body)
         (get-fundef id r))]
    [other (error 'search-fns "PAIG: no function found with given name ~v" other)]))

; interp: takes in an ExprC and returns the evaluated result
(define (interp [e : ExprC] [fds : (Listof FundefC)] [env : Env]) : Real
    (match e
      ; number
      [(NumC n) n]

      ;function call
      [(AppC id (list args ...))
       (define fd (get-fundef id fds))
       (if (equal? (length (FundefC-args fd)) (length args))
           ; we need to perform subst into the function body with each arg
           (interp (subst-args (map (lambda ([arg : ExprC]) (NumC(interp arg fds))) args)
                               (FundefC-args fd)
                               (FundefC-body fd))
                        fds)
           (error 'interp "PAIG: arity mismatch (function ~v requires ~v arguments, got ~v)"
                  id (length (FundefC-args fd)) (length args)))]

      [(BinopC op l r) ((hash-ref BinopMap op) (interp l fds) (interp r fds))]
      [(ifleq0C a t f) (if (<= (interp a fds)  0)
                         (interp t fds)
                         (interp f fds))]
      [(IdC sym) (lookup sym env)]))

; top-interp: combines parsing and evaluation
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

; looks up for a symbol in the enviroment
(define (lookup [for : symbol] [env : Env]) : number
    (match env
      ['() (error 'lookup "name not found: ~e" for)]
      [(cons (Binding name val) rest)
       (cond
          [(symbol=? for name) val]
          [else (lookup for rest)])]))

; Test Cases woooooo
(check-equal? (top-interp '{{fun {f (x)} {+ x 14}}
                             {fun {main (init)} {f (2)}}}) 16)
(check-equal? (top-interp '{{fun {f (x)} {* x 2}}
                             {fun {main (init)} {f (2)}}}) 4)
(check-equal? (top-interp '{{fun {f (x)} {/ x 1}}
                             {fun {main (init)} {f (2)}}}) 2)
(check-equal? (top-interp
               '{
                 {fun {main (init)}
                      {round (3.7)}}
                 {fun {round(num)}
                      {ifleq0? (- num 0.5)
                               : 0
                               else: {+ 1 {round((- num 1))}}
                           }}}) 4)
(check-equal? (subst (NumC 3.7) 'x (ifleq0C
   (BinopC '- (IdC 'num) (NumC 0.5))
   (NumC 0)
   (BinopC '+ (NumC 1) (AppC 'round (list (BinopC '- (IdC 'num) (NumC 1))))))) (ifleq0C
   (BinopC '- (IdC 'num) (NumC 0.5))
   (NumC 0)
   (BinopC '+ (NumC 1) (AppC 'round (list (BinopC '- (IdC 'num) (NumC 1)))))))

(check-equal? (parse-fundef '{fun {five ()} 5}) (FundefC 'five '() (NumC 5)))

; errors
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{fun and cool string})))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{{fun {f(x)} {{test x y}}}
{fun {main (init)} {f (2)}}})))
(check-exn (regexp "PAIG.*") (lambda () (parse-prog 2)))
(check-exn (regexp "PAIG.*") (lambda () (interp (BinopC '/ (NumC 2) (NumC 0)) '())))
(check-exn (regexp "PAIG.*") (lambda () (parse-fundef '(fun {+(-)} {+ 1 2}))))
(check-exn (regexp "PAIG.*") (lambda () (parse '(+ + 3))))
(check-exn (regexp "PAIG.*") (lambda () (parse '(a b c))))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{{fun {f (x)} {+ x 14}}})))
(check-exn (regexp "PAIG.*") (lambda () (interp (BinopC '+ (IdC 'c) (NumC 14)) '())))
(check-exn (regexp "PAIG.*") (lambda () (parse-fundef '(fun {f(-)} {+ 1 2}))))
(check-exn (regexp "PAIG.*") (lambda () (parse-fundef '(fun {f(a a)} {+ 1 2}))))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{{fun {f(x y)} {test (x y)}}
{fun {main (init)} {f ()}}})))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{{fun {f (x)} {+ x 14}}})))
(check-exn (regexp "PAIG.*") (lambda () (parse '(ifleq0? ((x (4)) then 3 else (+ 2 9) 3)))))