#lang typed/racket
(require typed/rackunit)

; 3.1 Accepts a list of strings and returns a single string combining the input strings in reverse order
#; (define (rev-str-app [l : (Lisof String)]) : String)

(define(rev-str-app [l : (Listof String)]) : String
  (match l
    ['() ""]
    [(cons current rest) ; this format is used in racket to destructure a list; seperating it from its head and tail value
     (string-append (rev-str-app rest) current)] ; NOTE: "." captures the first and last element, while "_" just captures the first element
   )
  )

(check-equal? (rev-str-app '()) "")
(check-equal? (rev-str-app (list "hi")) "hi")
(check-equal? (rev-str-app (list "hello" "kassi")) "kassihello")


;  3.2 Print-Type Questions

;  A. ":print-type" is used to print the types associated with values
;      I can use it alongside the 'typeof' function in order to print the type of value
;      Ex: (:print-type rev-str-app)

;  B. "rev-str-app" has the type "[String] : String" 
;      This type makes sense because which makes sense because it takes in a string as an input and returns the modified version.

;  C. "+" has the type "[Real ...] : Real"
;      This type is really long because it covers a wide range of possible arithmetic operations all with different numbers of arguments
;      Ex:  (:print-type +)

; 3.3 A representation for a bicycle that can either be a trek, bianchi, or gunnar.
(struct Trek ([num_wheels : Integer])#:transparent)
(struct Bianchi ([num_wheels : Integer])#:transparent)
(struct Gunnar ([num_wheels : Integer]) #:transparent)
(define-type Bicycle(U Trek Bianchi Gunnar))


; 3.4 -- Learning about the "s1?", "s1-f1", and "s1-f2" functions
(struct s1 ([f1 : Any] [f2 : Any])#:transparent)

(check-equal? (s1? (s1 42 "World")) #true)
(check-equal? (s1-f2 (s1 42 "World")) "World")
(check-equal? (s1-f1 (s1 42 "World")) 42)


; a). A function to determine whether a given value is a structure created with s1
#;(define (s1?_function [input : Any]) : Boolean
  (if (= input 1)
      true
      false)
 )

; b). A fucntion used to get the contents of field f1 in a s1 struct
#;(define (s1-f1_function [input : Any]) : Any
   (match input
    [(s1 f1 _)(f1)])
  )
; c). A function used to get the contents of field f2 in a s2 struct
#;(define (s1-f2_function [input : Any]) : Any
   (match input
    [(s1 _ f2)(f2)])
  )


; 3.5 A function that consumes a list of bicycles and returns a list containing only the Treks
(define (only-treks [l : (Listof Bicycle)]) : (Listof Trek)
  (match l
    ['()'()]
    [(cons f r)
     (cond
       [(Trek? f) (cons f (only-treks r))]
       [else (only-treks r)])]
  ))


(check-equal?(only-treks '()) '())
(check-equal?(only-treks (list(Trek 2))) (list(Trek 2)))
(check-equal?(only-treks
    (list
     (Trek 2)
     (Bianchi 2)
     (Trek 2)
     (Gunnar 2)
     (Trek 2)))
    (list
     (Trek 2)
     (Trek 2)
     (Trek 2)))


; 3.6 A function that consumes a list of bicycles and returns a list containing only the Bianchis
(define (only-bianchis [l : (Listof Bicycle)]) : (Listof Bianchi)
  (match l
    ['()'()]
    [(cons f r)
     (cond
       [(Bianchi? f) (cons f (only-bianchis r))]
       [else (only-bianchis r)])]
  ))

(check-equal?(only-bianchis'()) '()) ; --ASK-- i'm not sure what to return here if a list is empty..?
(check-equal?(only-bianchis (list (Bianchi 2))) (list (Bianchi 2)))
(check-equal?(only-bianchis
    (list
     (Trek 2)
     (Bianchi 2)
     (Trek 2)
     (Gunnar 2)
     (Bianchi 2)))
    (list
     (Bianchi 2)
     (Bianchi 2)))
          
; 3.7 A function that consumes a list of bicycles and a predicate and returns a list of bicycles containing only those elements
(define (onlyThese [l : (Listof Bicycle)] [predicate : (Bicycle -> Boolean)]) : (Listof Bicycle)
  (match l
    ['()'()]
    [(cons f r)
     (cond
       [(predicate f) (cons f (onlyThese r predicate))]
       [else (onlyThese r predicate)])]
  ))

(define (isBianchi? bike)
  (match bike
    [(Bianchi _) #t]
    [else #f]))

(define (isTrek? bike)
  (match bike
    [(Trek _) #t]
    [else #f]))

 (check-equal? (onlyThese '() isTrek?) '())
 (check-equal? (onlyThese '() isBianchi?) '())
 (check-equal? (onlyThese (list (Trek 2)) isTrek?) (list (Trek 2)))
 (check-equal? (onlyThese (list (Bianchi 2)) isBianchi?) (list (Bianchi 2)))
 (check-equal? (onlyThese (list (Bianchi 2) (Trek 2)) isTrek?) (list (Trek 2)))
 (check-equal? (onlyThese (list (Bianchi 2) (Trek 2)) isBianchi?) (list (Bianchi 2)))

; 3.8 A function that consumes two lists and returns the result of appending the second one to the first
(define (my-append [l1 : (Listof Any)] [l2 : (Listof Any)]) : (Listof Any)
  (match l1
    ['() l2]
    [(cons f r) (cons f (my-append r l2))]
  ))

(check-equal? (my-append '() '()) '())
(check-equal? (my-append (list 'a 'b 'c) '()) (list 'a 'b 'c))
(check-equal? (my-append '() (list 'd 'e 'f)) (list 'd 'e 'f))
(check-equal? (my-append (list 'a 'b 'c) (list 'd 'e 'f)) (list 'a 'b 'c 'd 'e 'f))

; 3.9 A function that consumes a list and a number and returns the elements of the list up to the number
#;(define(my-take [l : (Listof Any)] [num : Integer]) : (Listof Any)
  (cond
    [(or (= '() l) (= 0 num)) '()]
    [else (cons (f l) (my-take (r l) (- num 1)))]
    )
  )

(define(my-take [l : (Listof Any)] [num : Integer]) : (Listof Any)
  (match l
    ['()'()]
    [(cons f r)
     (cond
       [(= num 0) '()]
       [else (cons f (my-take r (- num 1)))])]
  ))

(check-equal? (my-take '() 5) '())
(check-equal? (my-take (list 'a 'b 'c) 0) '())
(check-equal? (my-take (list 'a 'b 'c) 2) (list 'a 'b))
(check-equal? (my-take (list 'a 'b 'c) 5) (list 'a 'b 'c))