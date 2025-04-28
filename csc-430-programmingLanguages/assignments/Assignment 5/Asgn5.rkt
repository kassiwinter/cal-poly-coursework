#lang typed/racket
(require typed/rackunit)

; Assignment 5
; Jacob Malone
; Kassi Winter
; Arthur Zhao

; ExprC Type Definitions
(define-type ExprC (U NumC AppC IdC IfC LamC StrC))
(struct NumC ([n : Real])#:transparent)
(struct StrC ([s : String]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)])#:transparent)
(struct IdC ([sym : Symbol])#:transparent)
(struct IfC ([a : ExprC] [t : ExprC] [f : ExprC])#:transparent)
(struct LamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)

; Value Type Definition
(define-type Value (U Real String Boolean Closure PrimOp 'Error))
; Closure Type Definition
(struct Closure ([args : (Listof Symbol)] [body : ExprC]) #:transparent)
; Primitive Operators Definition
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

; Environment Type Definitions
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

; Environment Lookup
(define (lookup [for : Symbol] [env : Env]) : Value
  (println env)
   (match env
      ['() (error 'lookup "PAIG: name not found: ~e" for)]
      [(cons (Binding name val) r) (cond
                    [(symbol=? for name) val]
                    [else (lookup for r)])]))

; Environment Extension
(define (extend [vars : (Listof Symbol)] [vals : (Listof ExprC)] [env : Env]) : Env
  (print vars)
  (print vals)
  (match* (vars vals)
    [('() '()) env]
    [((cons var r)(cons val s))
     (extend-env (Binding var (interp val env)) (extend r s env))]))

; No-Id words
(define No-Id '(not ? else: with as blam))

 (define (is-keyword? [sym : Symbol]) : Boolean
  (cond
    [(member sym No-Id) #t]
    [else #f]))

; parse: takes in a s-expression and returns the s-expression parsed as an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    ; number
    [(? real? num) (NumC num)]
        ; function definition
    [(list 'blam (list (? symbol? args) ...) body)
     ; checking for duplicates in function definition
      (if (check-duplicates args)
         (error 'parse "PAIG: duplicate variables")
         (LamC (cast args (Listof Symbol)) (parse body)))]
    ; if?
    [(list a '? t 'else: f) (IfC (parse a) (parse t) (parse f))]
    ; variable
    [(? symbol? sym)
     (if (is-keyword? sym)
         (error 'parse "PAIG: cannot use ~v as var name" sym)
         (IdC sym))]
    ; string
    [(? string? str) (StrC str)]
    ; with (variables)
    [(list 'with (list defs 'as vars) ... ': body)
     ; check if vars are same
     (if (check-duplicates vars)
         (error 'parse "PAIG: duplicate variables")
         ; desugaring
         (AppC (LamC (cast vars (Listof Symbol)) (parse body)) (map parse (cast defs (Listof Sexp)))))]
    ; function call
    [(list fun args ...)
     (AppC (parse fun) (map parse args))]
    ; not supported
    #;[idk (error 'parse "PAIG: improperly formatted expression ~v" idk)]))

; interp: takes in an ExprC and returns the evaluated result
(define (interp [e : ExprC] [env : Env]) : Value
    (match e
      ; number
      [(NumC n) n]
      ; string
      [(StrC s) s]
      [(IdC id )
       ; lookup id in environment
       (lookup id env)]
      ; function definition
      [(LamC arg body) (Closure arg body)]
      ;function application
      [(AppC fun (list vals ...))
       (define fd (interp fun env))
       (match fd
         ; Primitive Operator
         [(PrimOp op) (op (interp (first vals) env) (interp (second vals) env))]
         ; Closure
         [(Closure args body)
          (if (equal? (length args) (length vals))
           ; we need to add each argument to the environment that we call interp with
           (interp (Closure-body fd)
                   ; adding each arg to the environment
                   ; arg var values are in args
                   (extend args vals env))
           (error 'interp "PAIG: arity mismatch (function ~v requires ~v arguments, got ~v)"
                  fun (length (Closure-args fd)) (length args)))]
         ; error
         ['Error (error 'interp "PAIG: user-error ~v" (serialize (interp (first vals) env)))]
         [_ (error 'interp "PAIG:unknown interpret error")])]
       ; if
       [(IfC a t f)
        (define test (interp a env))
        (if (boolean? test)
            (if test
                (interp t env)
                (interp f env))
            (error 'interp "PAIG: arg doesn't evaluate to a boolean value" ))]
       #;[_ (error 'interp "PAIG: error while interpreting")]))

; serialize
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

; top-interp: combines parsing and evaluation
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

; Test Cases woooooo
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


; error checking
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