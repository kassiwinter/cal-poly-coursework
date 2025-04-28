#lang typed/racket
 (require typed/rackunit)
; Kassi - Assignment 3
; How Far I Got: Completely Implemented (to the best of my knowledge)

; -- Structures --
  (define-type ExprC (U NumC BinopC IdC AppC ifleq0))
  (struct NumC ([n : Real])  #:transparent)
  (struct IdC ([s : Symbol])  #:transparent)
  (struct AppC ([fun : Symbol] [arg : (Listof ExprC)])  #:transparent)
  (struct BinopC ([sym : Symbol] [l : ExprC] [r : ExprC])  #:transparent)
  (struct FundefC ([name : Symbol] [param : (Listof Symbol)] [body : ExprC])  #:transparent)
  (struct ifleq0 ([a : ExprC] [b : ExprC] [c : ExprC])#:transparent)



; -- Binop Functions --
(define (PlusC [l : Real] [r : Real]) : Real (+ l r))
(define (MinusC [l : Real] [r : Real]) : Real(- l r))
(define (MultC [l : Real] [r : Real]) : Real(* l r))
(define (DivC [l : Real] [r : Real]) : Real
  (if (zero? r)
      (error 'DivC "PAIG Error. Division by zero is not allowed.")
      (/ l r)))
  (define Binop-Map
    (hash '+ PlusC
          '- MinusC
          '* MultC
          '/ DivC))


; -- Parsers --
; - A. Expression Parser: A function that takes in a s-expression, parses it, and returns an ExprC
#;(define (parse [s : Sexp]) : ExprC)
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(cons (? real? n) r) (NumC n)]
    [(? symbol? sym)
     (cond
       [(or (hash-has-key? Binop-Map sym)
          (equal? sym 'fun)
          (equal? sym 'ifleq0?))
        (error 'parse "PAIG Error. Invalid function definition.")]
       [else (IdC sym)])]
    [(list (? symbol? fun) (list args ...))
     (AppC fun (map parse args))]
    [(list (? symbol? sym) l r)
     (cond
       [(hash-has-key? Binop-Map sym) (BinopC sym (parse l) (parse r))]
       [else (error 'parse (format "PAIG Error. Unsupported binary operator: '~a'" sym))])]
    [(list 'ifleq0? a _ b _ c) (ifleq0 (parse a) (parse b) (parse c))]
    [other (error 'parse "PAIG Error. Unsupported S-Expression: '(unsupported-expression)'")]
    ))


; - B. Function Definition Parser : A function that takes in a s-expression, parses it, and returns a FundefC
#; (define (parse-fundef [s : Sexp]) : FundefC)
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
   [(list 'fun (list (? symbol? id) (list (? symbol? arg))) body)
    (cond
     [(or (hash-has-key? Binop-Map id)
          (hash-has-key? Binop-Map arg)
          (equal? id 'fun)
          (equal? id 'ifleq0?))
      (error 'parse-fundef "PAIG Error. Invalid function definition.")]
     [else (FundefC id (list arg) (parse body))])]
    [other (error 'parse-fundef "PAIG Error. Invalid function definition")]))


; - C. Program Parser: A function that takes in a program as a s-expression and returns a list of FundefCs
#; (define (parse-prog [s : Sexp]) : (Listof FundefC))
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]
    ['() '()]
    [other (error 'parse-prog "PAIG Error. No S-Expression found." other)]))


; -- Interpreter --
; - A. Main Interpreter : A function that interprets the main
#; (define (interp-fns [fds : (Listof FundefC)]) : Real)
(define (interp-fns [fds : (Listof FundefC)]) : Real
  (match (get-fundef 'main fds)
    [(FundefC name arg body) (interp (subst (NumC 0) 'init body) fds)]))

; - B. Expression Interpreter
#; (define (interp [e : ExprC][fds : (Listof FundefC)]) : Real)
(define (interp [e : ExprC][fds : (Listof FundefC)]) : Real
  (match e
    [(NumC n) n]
    [(AppC fun (list args ...))
        (define fd (get-fundef fun fds))
        (if (equal? (length (FundefC-param fd)) (length args))
             (interp (subst-args (map (lambda ([arg : ExprC]) (NumC(interp arg fds))) args)
                               (FundefC-param fd)
                               (FundefC-body fd))
                        fds)
           (error 'parse-prog "PAIG Error. Incorrect number of arguments for given function."))
          ]

    [(BinopC symbol l r)
        (define operator (hash-ref Binop-Map symbol))
        (operator (interp l fds) (interp r fds))
       ]
     [(ifleq0 a b c) (if (<= (interp a fds)  0)
                     (interp b fds)
                     (interp c fds))]
    [(IdC _) (error 'interp "PAIG Error. I'm not really sure why but thats what you said in class.")]
    )
  )

; - C. Combining parsing and evaluating
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

; -- Helper Functions --
; - A. Get Function Definitions: A function that takes in a list of Fundefs and a symbol, returning the Fundef
#; (define (get-fundef [sym : Symbol] [funs : (Listof FundefC)]) : FundefC)
(define (get-fundef [sym : Symbol] [funs : (Listof FundefC)]) : FundefC
  (match funs
    [(cons (FundefC name arg body) r)
     (if (equal? sym name)
         (FundefC name arg body)
         (get-fundef sym r))]
    ['() (error 'search-fns "PAIG Error. No function has been found.")]))

; - B. Substitute Function : A function that substitutes an expression for a symbol
#; (define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC)
  (define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
    (match in
    [(NumC n) in]
    [(IdC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(ifleq0 a b c) (ifleq0 (subst what for a) (subst what for b) (subst what for c))]
    [(AppC f a) (AppC f (map (lambda ([a : ExprC]) (subst what for a)) a))]
    [(BinopC sym l r) (BinopC sym (subst what for l)
                        (subst what for r))])
  )

; - C. Substitute Arguments Function: A function that substitutes multiple arguments for sybmols
#; (define (subst-args [exprs : (Listof ExprC)]  [vars : (Listof Symbol)]  [in : ExprC]) : ExprC)
(define (subst-args [what : (Listof ExprC)]  [for : (Listof Symbol)]  [in : ExprC]) : ExprC
  (match* (for what) 
    [((cons var r)(cons exp s)) (subst-args s r (subst exp var in )) ]
    [('() '()) in ]))

; -- Test Cases --
(check-equal? (top-interp '{{fun {f (x)} {+ x 1}}
                             {fun {main (init)} {f (2)}}}) 3)
(check-equal? (top-interp
               '{
                 {fun {main (init)}
                      {+ 2 3}
                 }
               }) 5)

(check-equal? (top-interp
               '{
                 {fun {main (init)}
                      {- 5 3}
                      }
                 }) 2)

(check-equal? (top-interp
               '{
                 {fun {main (init)}
                      {* 4 6}
                      }
                 }) 24)

(check-equal? (top-interp
               '{
                 {fun {main (init)}
                      {+ {+ 2 3} {+ 4 5}}
                 }
               }) 14)


(check-equal? (parse '(+ 1 2)) (BinopC '+ (NumC 1) (NumC 2)))
(check-equal? (parse '(- 3 4)) (BinopC '- (NumC 3) (NumC 4)))
(check-equal? (parse '(* 5 6)) (BinopC '* (NumC 5) (NumC 6)))
(check-equal? (parse '(/ 7 8)) (BinopC '/ (NumC 7) (NumC 8)))

(check-equal? (DivC 4 2) 2)

(check-equal? (subst (NumC 1) 'x (IdC 'x)) (NumC 1))
(check-equal? (subst (NumC 2) 'x (NumC 3)) (NumC 3))
(check-equal? (subst (NumC 4) 'x (IdC 'y)) (IdC 'y))
(check-equal? (subst (NumC 42) 'a (ifleq0 (IdC 'a) (NumC 2) (NumC 3)))
                (ifleq0 (NumC 42) (NumC 2) (NumC 3)))

;(check-equal?(parse-fundef '{fun {f (x)} {+ x 1}})(FundefC 'f ('x) (BinopC '+ (IdC 'x) (NumC 1))))




(check-equal? (parse-prog '()) '())
(check-exn (regexp (regexp-quote "PAIG Error. Unsupported S-Expression")) (lambda ()
                                          (parse '(unsupported-expression))))
(check-exn (regexp (regexp-quote "PAIG Error. Invalid function definition")) (lambda ()
                                         (parse-fundef '(fun (invalid-definition)))))
(check-exn (regexp (regexp-quote "PAIG Error. I'm not really sure why but thats what you said in class."))
           (lambda () (interp (IdC 'someId) '())))
(check-exn (regexp (regexp-quote "PAIG Error. No function has been found."))
           (lambda () (get-fundef 'nonexistent-fun '())))

(check-exn exn:fail?
    (lambda ()
      (parse-prog 'unexpected-input)))
(check-equal? (parse '(ifleq0? 1 _ 2 _ 3))
                (ifleq0 (NumC 1) (NumC 2) (NumC 3)))

 (check-equal? (interp (ifleq0 (NumC 3) (NumC 5) (NumC 7)) '())
                7)
  (check-equal? (interp (ifleq0 (NumC -2) (NumC 5) (NumC 7)) '())
                5)

 (check-equal? (parse '(12 . (3 4 5))) (NumC 12))
(check-exn
  (regexp (regexp-quote "PAIG Error. Unsupported binary operator: 'unsupported-operator'"))
  (lambda () (parse '(unsupported-operator 1 2)))
)
(check-exn (regexp (regexp-quote "PAIG Error. Invalid function definition."))
  (lambda ()
    (parse-fundef '(fun (+ (x)) 13))))

(define (one f x)
  (f x))

(define (add f1 f2)
  (lambda (f x)
    (f1 f (f2 f x))))

(define (two f x)
  (f (f x)))

(define three (add one (add one two)))

