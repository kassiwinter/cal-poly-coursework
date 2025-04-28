#lang typed/racket
(require typed/rackunit)

; Assignment 7
; Kassi Winter

; --- Setting the Variables ---
; ExprC Type Definitions
(define-type ExprC (U NumC AppC IdC IfC LamC StrC))
(struct NumC ([n : Real])#:transparent)
(struct StrC ([s : String]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)])#:transparent)
(struct IdC ([sym : Symbol])#:transparent)
(struct IfC ([a : ExprC] [t : ExprC] [f : ExprC])#:transparent)
(struct LamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct Array ([size : Real] [values : (Listof ExprC)]) #:transparent)

; Type Definitions & Structures
(define-type Value (U Real String Boolean Closure PrimOp 'Error))
(struct Closure ([args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct PrimOp ([func : (-> Value Value Value)]) #:transparent)

; PrimOp Functions
(define (Add [l : Value] [r : Value]) : Value
  (match* (l r)
    [((? real? l) (? real? r)) (+ l r)]
    [(_ _) (error '+ "PAIG: Cannot add non-numbers ~v ~v" l r)]))
(define (Sub [l : Value] [r : Value]) : Value
  (match* (l r)
    [((? real? l) (? real? r)) (- l r)]
    [(_ _) (error '- "PAIG: Cannot sub non-numbers ~v ~v" l r)]))
(define (Mult [l : Value] [r : Value]) : Value
  (match* (l r)
    [((? real? l) (? real? r)) (* l r)]
    [(_ _) (error '* "PAIG: Cannot mult non-numbers ~v ~v" l r)]))
(define (Div [l : Value] [r : Value]) : Value
  (match* (l r)
    [((? real? l) (? real? r))
     (if (= 0 r)
         (error '/ "PAIG: Cannot divide ~v by 0" l)
     (/ l r))]
    [(_ _) (error '/ "PAIG: Cannot div non-numbers ~v ~v" l r)]))
(define (LessEq [l : Value] [r : Value]) : Value
  (match* (l r)
    [((? real? l) (? real? r)) (<= l r)]
    [(_ _) (error '<= "PAIG: Cannot <= non-numbers ~v ~v" l r)]))
(define (Eq [l : Value] [r : Value]) : Value
  (equal? l r))

; --- Setting up the Enviroment ---
; Environment Definitions
(struct Binding ([name : Symbol] [val : Value]) #:transparent)
(define-type Env (Listof Binding))
(define mt-env '())
(define top-env (list (Binding 'init 0) (Binding 'true #t) (Binding 'false #f)
                       (Binding '+ (PrimOp Add))
                       (Binding '- (PrimOp Sub))
                       (Binding '* (PrimOp Mult))
                       (Binding '/ (PrimOp Div))
                       (Binding '<= (PrimOp LessEq))
                       (Binding 'equal? (PrimOp Eq))
                       (Binding 'error 'Error)))
(define extend-env cons)

; A. Lookup: A function that looks up names/symbols within the Enviroment
(define (lookup [for : Symbol] [env : Env]) : Value
  (println env)
   (match env
      ['() (error 'lookup "PAIG: name not found: ~e" for)]
      [(cons (Binding name val) r) (cond
                    [(symbol=? for name) val]
                    [else (lookup for r)])]))

; B. Extend: A function that extends the Enviroment to include variables within scope
(define (extend [vars : (Listof Symbol)] [vals : (Listof ExprC)] [env : Env]) : Env
  (print vars)
  (print vals)
  (match* (vars vals)
    [('() '()) env]
    [((cons var r)(cons val s))
     (extend-env (Binding var (interp val env)) (extend r s env))]))

; C. Is-Keyword: A function that makes sure that no binding/name being stored in the enviroment is a keyword
(define Keywords '(not ? else: with as blam))
 (define (is-keyword? [sym : Symbol]) : Boolean
  (cond
    [(member sym Keywords) #t]
    [else #f]))

; --- Mutation ---
; A. Make-Array: Creates a fresh array of the given size, with all cells filled with the given value
(define (make-array [size : Real] [value : ExprC]) : Array
  (cond
    [(<= size 0) (error 'make-array "PAIG: array size must be greater than zero")]
    [else
     (if (zero? value)
      (error 'make-array "PAIG: array must have at least one value")
      (Array size (cons value (make-array (sub1 size) value))))])) ; NOTE: look into self calling recursion


; B. Array: Creates a fresh array containg the given values
(define (array [values : (Listof ExprC)]) : Array
  (let ((size (length values)))
   (cond
    [(<= size 0) (error 'make-array "PAIG: array size must be greater than zero")]
    [else
        (Array size (apply append (map list values)))])))

; C. Aref: Returns an element of an array


; --- Language Processing & Development ---
; A. Parse: A function that takes in a s-expression and returns the s-expression parsed as an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? num) (NumC num)] ; -> number
    [(list 'blam (list (? symbol? args) ...) body) ; -> function definition
      (if (check-duplicates args) ; NOTE: checking for duplicate variables in the function def
         (error 'parse "PAIG: duplicate variables")
         (LamC (cast args (Listof Symbol)) (parse body)))]
    [(list a '? t 'else: f) (IfC (parse a) (parse t) (parse f))] ; -> if statment
    [(? symbol? sym) ; -> variable
     (if (is-keyword? sym)
         (error 'parse "PAIG: cannot use ~v as var name" sym)
         (IdC sym))]
    [(? string? str) (StrC str)] ; -> string
    [(list 'with (list defs 'as vars) ... ': body) ; -> with / variables
     (if (check-duplicates vars) ; NOTE: checking for duplicate variables within the statment declaration
         (error 'parse "PAIG: duplicate variables")
         (AppC (LamC (cast vars (Listof Symbol)) (parse body)) (map parse (cast defs (Listof Sexp)))))] ; DESUGAR
    [(list fun args ...) ; -> function call
     (AppC (parse fun) (map parse args))]))

; B. Top-Interp: A function that combines parsing and evaluation
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

; C. Interp: A function that takes in an ExprC and returns the evaluated result
(define (interp [e : ExprC] [env : Env]) : Value
    (match e
      [(NumC n) n]
      [(StrC s) s]
      [(IdC id )
       (lookup id env)] ; NOTE: lookup id in environment
      [(LamC arg body) (Closure arg body)]
      [(AppC fun (list vals ...))
       (define fd (interp fun env))
       (match fd
         [(PrimOp op) (op (interp (first vals) env) (interp (second vals) env))]
         [(Closure args body)
          (if (equal? (length args) (length vals))
           (interp (Closure-body fd)  ; NOTE: adding each arg to the environment
                   (extend args vals env))
           (error 'interp "PAIG: arity mismatch (function ~v requires ~v arguments, got ~v)"
                  fun (length (Closure-args fd)) (length args)))]
         ['Error (error 'interp "PAIG: user-error ~v" (serialize (interp (first vals) env)))]
         [_ (error 'interp "PAIG:unknown interpret error")])]
       [(IfC a t f)
        (define test (interp a env))
        (if (boolean? test)
            (if test
                (interp t env)
                (interp f env))
            (error 'interp "PAIG: arg doesn't evaluate to a boolean value" ))]))

; --- Helper Functions ---
; A. Serialize: A function that accepts any PAIG5 value, and returns a string (testing functions)
(define (serialize [val : Value]) : String
  (match val
    [(? real? num) (~v num)]
    [(? boolean? bool)
     (if bool
         "true"
         "false"
         )]
    [(? string? str) str]
    [(struct* Closure ([args args] [body body])) "#<procedure>"]
    [(struct* PrimOp ([func func])) "#<primop>"]))


; --- Mountains of Test Cases ---
(check-equal? (top-interp '{{blam {x} {+ x 14}} 2}) "16")
(check-equal? (top-interp '{with [{+ 9 14} as z]
      [98 as y] :
      {+ z y}}) "121")
(check-equal? (top-interp '{+ 1 2}) "3")
(check-equal? (top-interp '{- 1 2}) "-1")
(check-equal? (top-interp '{* 1 2}) "2")
(check-equal? (top-interp '{/ 2 1}) "2")
(check-equal? (top-interp '{/ 2 1}) "2")
(check-equal? (top-interp '{<= 1 2}) "true")
(check-equal? (top-interp '{<= 2 1}) "false")
(check-equal? (top-interp '{equal? 1 1}) "true")
(check-equal? (top-interp '{equal? 1 2}) "false")
(check-equal? (top-interp '{{blam {x} {{<= x 5} ? 2 else: 3}} 3}) "2")
(check-equal? (top-interp '{{blam {x} {{<= x 5} ? 2 else: 3}} 6}) "3")
(check-equal? (serialize (Closure '(b c) (NumC 2))) "#<procedure>")
(check-equal? (serialize (PrimOp Add)) "#<primop>")
(check-equal? (serialize "hi") "hi")


(check-exn (regexp "PAIG.*") (lambda () (top-interp '{+ true false})))
(check-exn (regexp "PAIG.*") (lambda () (parse '{+ ? 3})))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{- true false})))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{* true false})))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{/ true false})))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{<= true false})))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{"hi" "bye"})))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{3 4})))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{/ 5 0})))
(check-exn (regexp "PAIG.*") (lambda () (lookup 'f mt-env)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{with [{+ 9 14} as z]
      [98 as z] :
      {+ z y}})))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{with [{+ 9 14} as z] :
      {+ z y}})))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{{blam {x x} {{<= x 5} ? 2 else: 3}} 3 4})))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{{blam {x} {{<= x 5} ? 2 else: 3}} 6 2})))
(check-exn (regexp "user-error.*") (lambda () (top-interp '{{blam {x} {{<= x 5} ? 2 else: {error 2}}} 6})))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{{blam {x} {5 ? 2 else: {error 2}}} 6})))

(check-equal? (make-array 5 NumC (10)) (Array 5 (list NumC (10) NumC (10) NumC (10) NumC (10) NumC (10)))) ; Test Case 1
