#lang typed/racket
(require typed/rackunit)
; I completely finished the asignment to the best of my knowledge.
; As far as I found, I did not get stuck on anything in particular.

; 2.1. - Exercise 2.3.3
; A function that consumes the number of attendees at a movie theater and returns total profit earned
#; (define(total-profit [attendees: Integer]) : Real)
(define(total-profit [attendees : Integer]) : Real
  (- (* 5 attendees) (+ 20 (* 0.50 attendees)))
)

(check-= (total-profit 0) -20 0.01)
(check-= (total-profit 10) 25 0.01)
(check-= (total-profit 100) 430 0.01)

; 2.1 - Exercise 3.3.3
; A function that consumes the radius of a cylinder's base and its height, returning its surface area
#; (define(area-cylinder [radius : Real][height : Real]) : Real)

(define(area-cylinder [radius : Real][height : Real]) : Real
  (+ (* (* radius 2 pi) height)
     (* pi radius radius)
     (* pi radius radius))
  )

(check-= (area-cylinder 0 0) 0 0.01)
(check-= (area-cylinder 0 2) 0 0.01)
(check-= (area-cylinder 2 0) 25.132 0.01)
(check-= (area-cylinder 2 5) 87.960 0.01)

; 2.2 - Playing Card Manufacturing
; A function that accepts a card and returns the next higher card that would appear in a same-suit straight
#; (define-type Suit (U 'club 'diamond 'heart 'spade))
#; (define-type FaceCard (U 'jack 'queen 'king))
#; (struct Card ([suit : Suit] [num : (U Integer FaceCard)]))
#; (define(next-card [card : Card]) : Card)
     
(define-type Numeric (U 'club 'diamond 'heart 'spade))
(define-type FaceCard (U 'jack 'queen 'king))
(struct Card ([suit : Numeric] [num : (U Integer FaceCard)])#:transparent)

(define (next-card [card : Card]) : Card
  (match card
    [(Card suit 'jack) (Card suit 'queen)]
    [(Card suit 'queen) (Card suit 'king)]
    [(Card suit 'king) (Card suit 1)]
    [(Card suit num)
     (cond
      [(and (integer? num)(= num 10)) (Card suit 'jack)]
      [else (Card suit (add1 num))]
    )
   ]
))

#;(define(next-card [card : Card]) : Card
  (match card [(Card suit num)
     (cond
       
      [(and (real? num) (> num 0))
       (if (= num 10)
           (Card suit 'jack)
           (Card suit (+ 1 num)))]
      
      [(eq? num 'jack) (Card suit 'queen)]
      [(eq? num 'queen) (Card suit 'king)]
      [(eq? num 'king) (Card suit 1)]
      [else (error "Invalid Input")]
      )]
    )
  )

(check-equal? (next-card (Card 'spade 9)) (Card 'spade 10))
(check-equal? (next-card (Card 'diamond 10)) (Card 'diamond 'jack))
(check-equal? (next-card (Card 'heart 'jack)) (Card 'heart 'queen))
(check-equal? (next-card (Card 'club 'queen)) (Card 'club 'king))
(check-equal? (next-card (Card 'spade 'king)) (Card 'spade 1))

; 2.3 - Low-degree Polynomials
; A function that accepts a polynomial and a value and produces the result of plugging in the value
#; (define-type Linear ([A : Real] [B: Real]))
#; (define-type Quadratic ([A : Real] [B: Real] [C: Real]))
#; (define-type Polynomial (U Linear Quadratic))
#; (define (interp [poly : Polynomial] [value : Real]) : Real)

(struct Linear ([a : Real] [b : Real])#:transparent)
(struct Quadratic ([a : Real] [b : Real] [c : Real])#:transparent)
(define-type Polynomial (U Linear Quadratic))
(define(interp [poly : Polynomial][value : Real]) : Real
  (match poly
    [(Linear a b) 
     (+ (* a value) b)]
    [(Quadratic a b c)
     (+ (* a (expt value 2)) (* b value) c)])
  )

(check-= (interp (Linear 0 0) 4) 0 0.001)
(check-= (interp (Linear 0 2) 4) 2 0.001)
(check-= (interp (Linear 2 0) 4) 8 0.001)
(check-= (interp (Linear 2.5 8.6) 4) 18.6 0.001)

(check-= (interp (Quadratic 0 0 0) 4) 0 0.001)
(check-= (interp (Quadratic 0 0 2) 4) 2 0.001)
(check-= (interp (Quadratic 0 2 0) 4) 8 0.001)
(check-= (interp (Quadratic 2 0 0) 4) 32 0.001)
(check-= (interp (Quadratic 2.5 8.6 7.1) 4) 81.5 0.001)

; 2.4 - Derivative
; A function that accepts a polynomial and returns another polynomial that represents its derivative
#; (define (derivative [poly : Polynomial]) : Polynomial)

(define(derivative [poly : Polynomial]) : Polynomial
  (match poly
    [(Linear a b)
     (if (= a 0)
         (Linear 0 b)
         (Linear 0 a))] 
    [(Quadratic a b c)
     (if (= a 0)
         (Linear 0 b) 
         (Linear (* 2 a) b))] 
    )
  )

(check-equal? (derivative (Linear 0 0)) (Linear 0 0))
(check-equal? (derivative (Linear 0 2)) (Linear 0 2))
(check-equal? (derivative (Linear 2 0)) (Linear 0 2))
(check-equal? (derivative (Linear 2.5 8.6)) (Linear 0 2.5))

(check-equal? (derivative (Quadratic 0 0 0)) (Linear 0 0))
(check-equal? (derivative (Quadratic 0 0 2)) (Linear 0 0))
(check-equal? (derivative (Quadratic 0 2 0)) (Linear 0 2))
(check-equal? (derivative (Quadratic 2 0 0)) (Linear 4 0))
(check-equal? (derivative (Quadratic 2 3 0)) (Linear 4 3))
(check-equal? (derivative (Quadratic 2 3 4)) (Linear 4 3))

; 2.5 - Binary Tree
; A data definition for a full binary tree
(struct Leaf([symbol : Symbol])#:transparent)
(struct Node([child1 : BTree] [child2 : BTree])#:transparent)
(define-type BTree(U Leaf Node))


(define tiny-tree (Leaf 'A))
(define small-tree (Node (Leaf 'A) (Leaf 'B)))
(define medium-tree (Node (Node (Leaf 'A) (Node (Leaf 'C) (Leaf 'D))) (Leaf 'B)))
(define large-tree (Node (Node (Leaf 'A) (Leaf 'B)) (Node (Node (Leaf 'C) (Leaf 'D)) (Leaf 'E))))

; 2.6 - ZZ-Tree
; A function that accepts a binary tree and changes every leaf symbol to 'zz
#; (define(zz-tree [tree : BTree]) : BTree)

(define(zz-tree [tree : BTree]) : BTree 
  (match tree
    [(Leaf _)
     (Leaf 'zz)]
    [(Node child1 child2)
     (Node(zz-tree child1) (zz-tree child2))]
  ))

(check-equal? (zz-tree (Leaf 'Kassi)) (Leaf 'zz))
(check-equal? (zz-tree (Node (Leaf 'Kassi) (Leaf 'Winter))) (Node (Leaf 'zz) (Leaf 'zz)))
(check-equal? (zz-tree (Node (Node (Leaf 'Orange) (Leaf 'Yellow))
                             (Leaf 'Red))) (Node (Node (Leaf 'zz) (Leaf 'zz)) (Leaf 'zz)))

; 2.7 - Mirror
; A function that accepts a tree and produces a new tree that is the left-right mirror image of the first
#;(define (mirror [tree : BTree]) : BTree)

(define(mirror [tree : BTree]) : BTree
  (match tree
    [(Leaf symbol)
     (Leaf symbol)]
    [(Node child1 child2)
     (Node (mirror child2) (mirror child1))])
  )

(check-equal? (mirror (Leaf 'Kassi)) (Leaf 'Kassi))
(check-equal? (mirror (Node (Leaf 'Kassi) (Leaf 'Winter))) (Node (Leaf 'Winter) (Leaf 'Kassi)))
(check-equal? (mirror (Node (Node (Leaf 'Orange) (Leaf 'Yellow)) (Leaf 'Red)))
              (Node (Leaf 'Red) (Node (Leaf 'Yellow) (Leaf 'Orange))))

; 2.8 - Occurences
; A function that accepts a binary tree and a symbol, returns a number equal to the leaves that have the symbol
#;(define (occurrences [tree : BTree] [symbol : Symbol]) : Integer)

(define(occurrences [tree : BTree] [symbol : Symbol]) : Integer
  (match tree
    [(Leaf s)
     (if (eq? s symbol) 1
         0)]
    [(Node child1 child2)
     (+ (occurrences child1 symbol)
        (occurrences child2 symbol))]))

(check-= (occurrences (Leaf 'Kassi) 'Winter) 0 0.01)
(check-= (occurrences (Leaf 'Kassi) 'Kassi) 1 0.01)
(check-= (occurrences (Node (Leaf 'Kassi) (Leaf 'Winter)) 'Kassi) 1 0.01)
(check-= (occurrences (Node (Node (Leaf 'Kassi) (Leaf 'Winter)) (Leaf 'Kassi)) 'Kassi) 2 0.01)

; 2.9 - Substitution
; A function that accepts a BTree, symbol, and replacement BTree, returns a new tree 
#;(define (subst [source : BTree] [target_symbol : Symbol][replacment : BTree]) : BTree)

(define (subst [source_tree : BTree] [symbol : Symbol][replacment_tree : BTree]) : BTree
  (match source_tree 
    [(Leaf s)
     (if (eq? s symbol) replacment_tree
         (Leaf s))]
    [(Node child1 child2)
     (Node (subst child1 symbol replacment_tree)
           (subst child2 symbol replacment_tree))]))


(check-equal? (subst (Leaf 'Kassi) 'Winter (Leaf 'Yeet)) (Leaf 'Kassi))
(check-equal? (subst (Leaf 'Kassi) 'Kassi (Leaf 'Yeet)) (Leaf 'Yeet)) 
(check-equal? (subst (Node (Leaf 'Kassi) (Leaf 'Winter)) 'Kassi (Leaf 'Yeet)) (Node (Leaf 'Yeet) (Leaf 'Winter)))
(check-equal? (subst (Node (Node (Leaf 'Kassi) (Leaf 'Winter)) (Leaf 'Kassi)) 'Kassi
                     (Node (Leaf 'Yeet) (Leaf 'Skeet))) (Node (Node (Node (Leaf 'Yeet)
                                                   (Leaf 'Skeet)) (Leaf 'Winter)) (Node (Leaf 'Yeet) (Leaf 'Skeet))))

