#lang typed/racket
(require typed/rackunit)

; Assignment 7

; ExprC Type Definitions
(define-type ExprC (U NumC AppC VarC IfC LamC StrC SetC))
(struct NumC ([n : Real])#:transparent)
(struct StrC ([s : String]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)])#:transparent)
(struct VarC ([sym : Symbol])#:transparent)
(struct IfC ([a : ExprC] [t : ExprC] [f : ExprC])#:transparent)
(struct LamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct SetC ([var : Symbol] [to : ExprC]) #:transparent)

; Value Type Definitions
(define-type Value (U NumV StrV BoolV ClosV PrimOp ArrV NullV))
(struct NumV ([n : Real]) #:transparent)
(struct StrV ([s : String]) #:transparent)
(struct BoolV ([b : Boolean])#:transparent)
(struct ClosV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct PrimOp ([s : Symbol]) #:transparent)
(struct ArrV ([loc : Real] [size : Real])#:transparent)
(struct NullV ([bs : Symbol])#:transparent)

; Environment Type Definitions
(struct Binding ([name : Symbol] [what : Natural]) #:transparent)
(define-type Env (Listof Binding))
(define mt-env '())
(define extend-env cons)

; interp: takes in an ExprC and returns the evaluated result
(define (interp [e : ExprC] [env : Env] [store : (Mutable-Vectorof Value)]) : Value

    (match e
      [(NumC n) (NumV n)]
      [(StrC s) (StrV s)]
      [(VarC var )
       (vector-ref store (lookup var env))]
      [(SetC var to) (vector-set! store (lookup var env) (interp to env store)) (NullV 'null)]
      [(LamC arg body) (ClosV arg body env)]
      [(AppC fun (list vals ...))
       (define f-value (interp fun env store))
       (match f-value
         [(PrimOp op) (handle-primop f-value (map (lambda ([val : ExprC]) (interp val env store)) vals ) env store)]
         [(ClosV args body clos-env)
          (interp (ClosV-body f-value)
                  (extend args (map (lambda ([val : ExprC]) (interp val env store)) vals ) clos-env store)
                  store)]
         [other (error 'interp "PAIG: AppC fun doesn't eval into a  ClosV or PrimOp ~v" other)])]
      [(IfC a t f)
       (define test (interp a env store))
       (if (BoolV? test)
           (if (BoolV-b test)
               (interp t env store)
               (interp f env store))
           (error 'interp "PAIG: arg doesn't evaluate to a boolean value ~v" test))]))

; top-interp: combines parsing and evaluation
(define (top-interp [s : Sexp] [memsize : Natural]) : String
  ; set initial store to the sum of 1 (free location index) + memsize + number of primops + number of constants
  (define initial-store (make-initial-store (+ 1 memsize (length primops-list) (length top-level-constants))))
  (define initial-env (extend
                       top-level-constants
                       top-level-values
                       (extend primops-list (map (lambda ([x : Symbol]) (PrimOp x)) primops-list) mt-env initial-store)
                       initial-store))
  (serialize (interp (parse s) initial-env initial-store)))

; top-level-constants
(define top-level-constants (list 'true 'false 'null))
; top level values
(define top-level-values (list (BoolV #t) (BoolV #f) (NullV 'null)))

; Primops List and function definitions for such primops
(define primops-list (list
                      '+
                      '-
                      '*
                      '/
                      'equal?
                      '<=
                      'array
                      'make-array
                      'aref
                      'aset!
                      'substring
                      'do
                      'error))

; add: adds two NumV and returns a Value that is a NumV, raises error if either operand is not a NumV
(define (add [l : Value] [r : Value]) : Value
  (match* (l r)
    [((NumV x) (NumV y)) (NumV (+ x y))]
    [(_ _) (error '+ "PAIG: Cannot add non-numbers ~v ~v" l r)]))

; sub: subtracts two NumV and returns a Value that is a NumV, raises error if either operand is not a NumV
(define (sub [l : Value] [r : Value]) : Value
  (match* (l r)
    [((NumV x) (NumV y)) (NumV (- x y))]
    [(_ _) (error '- "PAIG: Cannot sub non-numbers ~v ~v" l r)]))

; mult: multiplies two NumV and returns a Value that is a NumV, raises error if either operand is not a NumV
(define (mult [l : Value] [r : Value]) : Value
  (match* (l r)
    [((NumV x) (NumV y)) (NumV (* x y))]
    [(_ _) (error '* "PAIG: Cannot mult non-numbers ~v ~v" l r)]))

; div: divides two NumV and returns a Value that is a NumV,
; raises error if either operand is not a NumV, or if denominator is 0
(define (div [l : Value] [r : Value]) : Value
  (match* (l r)
    [((NumV x) (NumV y))
     (if (= 0 y)
         (error '/ "PAIG: Cannot divide ~v by 0" l)
     (NumV (/ x y)))]
    [(_ _) (error '/ "PAIG: Cannot div non-numbers ~v ~v" l r)]))

; less-eq: takes in two NumV Values, and returns a boolean based on whether the first is less
; than or equal to the second one
(define (less-eq [l : Value] [r : Value]) : Value
  (match* (l r)
    [((NumV x) (NumV y)) (BoolV (<= x y))]
    [(_ _) (error '<= "PAIG: Cannot <= non-numbers ~v ~v" l r)]))

; eq: takes in two Values, and returns a boolean value based on equality
(define (eq [l : Value] [r : Value]) : Value
  (BoolV (equal? l r)))

; arr-vals: takes in a list of values, and the store and allocates space in the store for them,
; returning the ArrV Value
(define (arr-vals [vals : (Listof Value)] [store : (Mutable-Vectorof Value)]) : Value
  (if (= (length vals) 0)
      (error 'arr-vals "PAIG: Cannot make an array with no values in it")
      (ArrV (allocate store vals) (length vals))))

; arr-size: takes in an array size, the init value, and the store and
; allocates space in the store for an array of that size, returning the new ArrV
(define (arr-size [size : Value] [init : Value] [store : (Mutable-Vectorof Value)]) : Value
  (match size
    [(NumV (? exact-integer? n))
     (if (>= 0 (NumV-n size))
      (error 'arr-size "PAIG: array size <= 0")
  (ArrV (allocate store (cast
                         (build-list (cast (NumV-n (cast size NumV)) Integer) (lambda ([x : Index]) init))
                         (Listof Value)))
        (NumV-n (cast size NumV))))]
    [_ (error 'arr-size "PAIG: arr-size requires an exact integer size")]
))

; arr-ref: takes in an array and an index, and returns the value at that index
(define (arr-ref [arr : Value] [ind : Value] [store : (Mutable-Vectorof Value)]) : Value
  (match* (arr ind)
    [((ArrV loc size) (NumV (? exact-integer? index)))
     (if (or (>= index size) (< index 0))
         (error 'arr-ref "PAIG: arr-ref index is out of bounds [index: ~v array-size: ~v]" index size)
         (vector-ref store (cast (+ loc index) Integer)))]))

; arr-set: takes in an array, an index, a value, and the store and sets the value in the store
; at that index to the new value
(define (arr-set [arr : Value] [ind : Value] [val : Value] [store : (Mutable-Vectorof Value)]) : Value
  (match* (arr ind)
    [((ArrV loc size) (NumV (? exact-integer? index)))
     (if (or (>= index size) (< index 0))
         (error 'arr-set "PAIG: arr-set index is out of bounds [index: ~v array-size: ~v]" index size)
         (begin
         (vector-set! store (cast (+ (ArrV-loc (cast arr ArrV)) (NumV-n (cast ind NumV))) Integer) val)
         (NullV 'null)))]
    [(_ _) (error 'arr-set "PAIG: arr-set requires an array, an exact integer index, and a value")]))

; paig-do: takes in a list of values and returns the last value
(define (paig-do [vals : (Listof Value)]) : Value
  (if (= (length vals) 0)
       (error 'paig-do "PAIG: cannot call do with no arguments")
  (last vals)))

; paig-substring: takes in a string, a start, and an end position, and returns the substring
(define (paig-substring [str : Value] [start : Value] [end : Value]) : Value
  (match* (str start end)
    [((StrV s) (NumV (? exact-integer? x)) (NumV (? exact-integer? y)))
     (StrV (substring s x y))]
    [(_ _ _) (error 'paig-substring "PAIG: substring needs string, and exact integers")]))

; paig-error: takes in a string value and raises a user error
(define (paig-error [str : Value]) : Value
  (error 'paig-error "user-error ~v" (serialize str)))

; handle-primops: given a PrimOp, its operands, the environment, and the store
; Outputs: the evaluated expression value
(define (handle-primop [op : PrimOp]
                       [vals : (Listof Value)]
                       [env : Env]
                       [store : (Mutable-Vectorof Value)]) : Value
  (match op
    [(PrimOp '+) (add (first vals) (second vals))]
    [(PrimOp '-) (sub (first vals) (second vals))]
    [(PrimOp '*) (mult (first vals) (second vals))]
    [(PrimOp '/) (div (first vals) (second vals))]
    [(PrimOp 'equal?) (eq (first vals) (second vals))]
    [(PrimOp '<=) (less-eq (first vals) (second vals))]
    [(PrimOp 'array) (arr-vals vals store)]
    [(PrimOp 'make-array) (arr-size (first vals) (second vals) store)]
    [(PrimOp 'aref) (arr-ref (first vals) (second vals) store)]
    [(PrimOp 'aset!) (arr-set (first vals) (second vals) (third vals) store)]
    [(PrimOp 'substring) (paig-substring (first vals) (second vals) (third vals))]
    [(PrimOp 'do) (paig-do vals)]
    [(PrimOp 'error) (paig-error (first vals))]))

; lookup: given a symbol and an environment, returns the location of that symbol's binding in the store.
(define (lookup [for : Symbol] [env : Env]) : Natural
   (match env
      ['() (error 'lookup "PAIG: name not found: ~e" for)]
      [(cons (Binding name loc) r) (cond
                    [(symbol=? for name) loc]
                    [else (lookup for r)])]))

; extend: given a list of vars, a list of values, an environment, and a store, extends the environment,
; putting values in the store and extending the environment to map vars to corresponding value store locations
(define (extend [vars : (Listof Symbol)] [vals : (Listof Value)] [env : Env] [store : (Mutable-Vectorof Value)]) : Env
  (match* (vars vals)
    [('() '()) env]
    [((cons var r)(cons val s))
     (extend-env (Binding var (allocate store (cons val '()))) (extend r s env store))]
    [(_ _) (error 'extend "PAIG: vars and vals length mismatch")]))

; No-Id words
(define No-Id '(:= ? else: with as : blam))

; is-keyword? : given a symbol, returns true if it's a keyword, false otherwise
 (define (is-keyword? [sym : Symbol]) : Boolean
  (cond
    [(member sym No-Id) #t]
    [else #f]))

; parse: takes in a s-expression and returns the s-expression parsed as an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? num) (NumC num)]
    [(? string? str) (StrC str)]
    [(? symbol? sym)
     (if (is-keyword? sym)
         (error 'parse "PAIG: cannot use ~v as var name" sym)
         (VarC sym))]
    [(list (? symbol? var) ':= to) (SetC var (parse to))]
    [(list 'blam (list (? symbol? args) ...) body)
      (if (check-duplicates args)
         (error 'parse "PAIG: duplicate variables")
         (LamC (cast args (Listof Symbol)) (parse body)))]
    [(list a '? t 'else: f) (IfC (parse a) (parse t) (parse f))]
    [(list 'with (list defs 'as vars) ... ': body)
     (cond
       [(check-duplicates vars) (error 'parse "PAIG: duplicate variables")]
       [(and (andmap (lambda ([arg : Symbol]) (is-keyword? arg)) (cast vars (Listof Symbol))) (not (equal? vars '())))
        (error 'parse "PAIG: cannot use keyword as var name")]
         [(AppC (LamC (cast vars (Listof Symbol)) (parse body)) (map parse (cast defs (Listof Sexp))))])]
    [(list fun args ...) (AppC (parse fun) (map parse args))]))

; allocate: accepts a store and a list of values to place in the next set of sequential locations,
; and returns the base location.
(define (allocate [store : (Mutable-Vectorof Value)] [values : (Listof Value)]) : Natural
       (match values
         [(cons f r)
          (if (<= (vector-length store) (NumV-n (cast (vector-ref store 0) NumV)))
              (error 'allocate "PAIG: Ran out of memory :(")
              (begin
                 (my-vector-set! store (cast (NumV-n (cast (vector-ref store 0) NumV)) Nonnegative-Integer) f)
                 (my-vector-set! store 0 (NumV (+ (NumV-n (cast (vector-ref store 0) NumV)) 1)))
                 (cast (- (allocate store r) 1) Natural)))]
         ['() (cast (NumV-n (cast (vector-ref store 0) NumV)) Natural)]))

; make-initial-store: takes in a natural number (memsize) and initializes a vector of that size, where the first
; element is the index of the first free slot in the vector
(define (make-initial-store [memsize : Natural]) : (Mutable-Vectorof Value)
  (define store (cast (make-vector memsize (cast (NullV 'Null) Value)) (Mutable-Vectorof Value)))
  (my-vector-set! store 0 (NumV 1))
  store)

; serialize
(define (serialize [val : Value]) : String
  (match val
    [(NumV num) (~v num)]
    [(BoolV b)
     (if b
         "true"
         "false"
         )]
    [(StrV str) (~v str)]
    [(ArrV loc size) "#<array>"]
    [(ClosV args body env) "#<procedure>"]
    [(NullV bs) "null"]
    [(PrimOp s) "#<primop>"]))

(define while '{with ["bogus" as while] :
                     {do
                         {while := {blam (guard body) {{guard} ? 1 else: {do
                                                                                  {body}
                                                                                {while guard body}}}}}
                       while}})

(define in-order '{
                   })

; vector-set workaround
(: my-vector-set! (All (T) ((Vectorof T) Natural T -> Void)))
(define (my-vector-set! vec idx newval)
  (vector-set! vec idx newval))

#;(check-equal? (top-interp (quasiquote (with [(unquote while) as while]
                                            [0 as x] :
                                            {with [{blam () (<= x 5)} as condfn]
                                                  [{blam () (x := (+ x 1))} as bodyfn] :
                                            (do
                                                (while condfn bodyfn)
                                              x)
                                            })) 200) "6")

; test cases
(check-equal? (top-interp '{with ["bogus" as fact]
 :
 {do {fact := {blam (n) {{<= n 0} ? 1 else: {* n {fact {- n 1}}}}}}
   {fact 12}}} 100) "479001600")
(check-equal? (top-interp '{{blam {x} {+ x 14}} 2} 100) "16")
(check-equal? (top-interp '{with [{+ 9 14} as z]
      [98 as y] :
      {+ z y}} 100) "121")
(check-equal? (top-interp '{+ 1 2} 100) "3")
(check-equal? (top-interp '{- 1 2} 100) "-1")
(check-equal? (top-interp '{* 1 2} 100) "2")
(check-equal? (top-interp '{/ 2 1} 100) "2")
(check-equal? (top-interp '{/ 2 1} 100) "2")
(check-equal? (top-interp '{<= 1 2} 100) "true")
(check-equal? (top-interp '{<= 2 1} 100) "false")
(check-equal? (top-interp '{equal? 1 1} 100) "true")
(check-equal? (top-interp '{equal? 1 2} 100) "false")
(check-equal? (top-interp '{{blam {x} {{<= x 5} ? 2 else: 3}} 3} 100) "2")
(check-equal? (top-interp '{{blam {x} {{<= x 5} ? 2 else: 3}} 6} 100) "3")
(check-equal? (serialize (ClosV '(b c) (NumC 2) mt-env)) "#<procedure>")
(check-equal? (serialize (PrimOp '+)) "#<primop>")
(check-equal? (serialize (StrV "hi")) "\"hi\"")
(check-equal? (serialize (ArrV 2 5)) "#<array>")
(check-equal? (serialize (NullV 'bs )) "null")
(check-equal? (top-interp '{with [(array 4 5 6) as a] : {do
                                              {aset! a 2 {aref a 1}}
                                            {aref a 2}}} 10) "5")
(check-equal? (top-interp '{with [(make-array 3 3) as a] : {do
                                              {aset! a 2 (+ {aref a 1} 1)}
                                            {aref a 2}}} 10) "4")
(check-equal? (top-interp '{substring "hello world" 2 5} 100) "\"llo\"")


; error checking
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{+ true false} 100)))
(check-exn (regexp "PAIG.*") (lambda () (parse '{+ ? 3})))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{- true false} 100)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{* true false} 100)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{/ true false} 100)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{<= true false} 100)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{"hi" "bye"} 100)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{3 4} 100)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{/ 5 0} 100)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{with [{+ 9 14} as z]
      [98 as z] :
      {+ z y}} 100)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{with [{+ 9 14} as z] :
      {+ z y}} 100)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{{blam {x x} {{<= x 5} ? 2 else: 3}} 3 4} 100)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{{blam {x} {{<= x 5} ? 2 else: 3}} 6 2} 100)))
(check-exn (regexp "user-error.*") (lambda () (top-interp '{{blam {x} {{<= x 5} ? 2 else: {error 2}}} 6} 100)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{{blam {x} {5 ? 2 else: {error 2}}} 6} 100)))
(check-exn (regexp "user-error.*") (lambda () (top-interp '((blam (e) (e e)) error) 100)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{with [4 as ?] : ? } 100)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{with [(array 4 5 6) as a] : a } 3)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{with [(array) as a] : a } 3)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{with [(array 4 5 6) as a] : {do
                                              {aset! a 5 {aref a 1}}
                                            {aref a 2}}} 10)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{with [(array 4 5 6) as a] : {do
                                              {aset! a 2 {aref a -1}}
                                            {aref a 2}}} 10)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{with [(make-array 4 0) as a] : {do
                                              {aset! a 8 {aref a -1}}
                                            {aref a 2}}} 10)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{with [(make-array 0 0) as a] : {do
                                              {aset! a 8 {aref a -1}}
                                            {aref a 2}}} 10)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '{do} 100)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp `(substring "abcd" 0 0.234) 1000)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '(with ((make-array 5 false) as f) : (aset! f 2.3 19)) 1000)))
(check-exn (regexp "PAIG.*") (lambda () (top-interp '(with ((make-array 2.1 false) as f) : (aset! f 1 19)) 1000)))
