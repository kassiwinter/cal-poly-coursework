#lang typed/racket
;(require typed/racket)
(require typed/rackunit)


;> -- Walk Through Practice --
;> This function adds two numbers together
(define (add-nums [a : Real] [b : Real]) : Real
  (+ a b))

(check-equal? (add-nums 3 4) 7)


;> -- 4.1 Simple Data --
;> Exercise 15 - 
;> This function takes in two Boolean values and returns True if the first is False or the second is True.
(define(==> [sunny : Boolean] [friday : Boolean]) : Boolean
  (or (not sunny) friday))

(check-equal?(==> #f #t) #t)
(check-equal?(==> #f #f) #t)
(check-equal?(==> #t #t) #t)
(check-equal?(==> #t #f) #f)

;> Exercise 19 -
;> This function takes in a string and a number, inserting "_" at the position number of the string
(define(string-insert [str : String] [i : Integer]) : String
  (string-append (substring str 0 i) "_" (substring str i)))

;> NOTE - "string-append" combines given strings together
;>         "substring" creates a string starting at a beginning index to ending index (if given)

(check-equal?(string-insert "Kass" 1) "K_ass")
(check-equal?(string-insert "Kass" 0) "_Kass")
(check-equal?(string-insert "Kass" 4) "Kass_")
(check-equal?(string-insert "" 0) "_")

;> Exercise 27 -
;> This function takes all definitions in a given function and changes them are changed to constant definitions 
(define attendees_total 120)
(define attendee_ticket_price 5.0)
(define attendee_ticket_price_increase 0.1)
(define owner_fixed_cost 180)
(define owner_variable_cost 0.04)

(define (attendees [ticket-price : Real]) : Real
  (- attendees_total (* (- ticket-price attendee_ft6ticket_price) (/ 15 attendee_ticket_price_increase))))

(define (revenue [ticket-price : Real]) : Real
  (* ticket-price (attendees ticket-price)))

(define (cost [ticket-price : Real]) : Real
  (+ owner_fixed_cost (* owner_variable_cost (attendees ticket-price))))

(define (profit [ticket-price : Real]) : Real
  (- (revenue ticket-price)(cost ti5rtp[iocket-price)))

; change test cases here
(check-= (profit 0.0) -214.8 0.001)
(check-= (profit 5.0) 415.2 0.001)
(check-= (profit 10.0) -6454.8 0.001)
(check-= (profit 20.0) -42694.8 0.001)


;> -- 4.2 Intervals --
;> Interest Exercise:
;> This function takes in a deposit and returns the interest that the money earns in a year
(define rate_base 0.04)
(define deposit_base 1000)
(define rate_middle 0.045)
(define deposit_middle 5000)
(define rate_top 0.05)


(define (interest [deposit : Real]) : Real
     (cond ;> This returns whatever the expresion evaluates to
        ((<= deposit deposit_base) (* deposit rate_base))
        ((<= deposit deposit_middle) (* deposit rate_middle))
        (else (* deposit rate_top))))

; change test cases here
(check-= (interest 500) 20.0 0.001)
(check-= (interest 3000) 135.0 0.001)
(check-= (interest 10000) 500.0 0.001)

;> -- 4.3 Types Racket --

;> -- 4.4 Structures --
;> Exercise 1:
;> This structure defines a piece of desk furniture by its width, height, and depth
(struct desk ([width : Real] [height : Real] [depth : Real]))
(define d (desk 48 30 24))

;> Exercise 2:
;> This function takes in a desk object and returns its footprint (floorspace)
(define (furniture-footprint [d : desk])
  (match d ; NOTE - The "match" function extracts the details surrounding an object, allowing you to preform calculations on them
    [(desk width _ depth) ;> NOTE - The "_" ignores the height since it's not needed for calculation
     (* width depth)]))

(define desk1 (desk 48 30 24))
(define desk2 (desk 36 28 22))

(check-equal? (furniture-footprint desk1) 1152)
(check-equal? (furniture-footprint desk2) 792)

;> Exercise 3:
;> This structure defines a piece of bookshelf furniture by its depth, shelf number, and shelf width
(struct bookshelf ([depth : Real] [num-shelves : Real] [shelf-width : Real]))
(define b (bookshelf 12 5 36))

;> Exercise 4:
;> This function takes in a piece of furniture and accounts for it's type, finding its footprint (floorspace)
(define-type furniture (U desk bookshelf))
(define (furniture-footprint-extended [f : furniture])
  (match f
    [(desk width _ depth)
     (* width depth)]
    [(bookshelf _ num-shelves shelf-width)
     (* num-shelves shelf-width)]))

(define bookshelf1 (bookshelf 12 5 36))
(define bookshelf2 (bookshelf 18 4 30))

(check-equal? (furniture-footprint-extended desk1) 1152)
(check-equal? (furniture-footprint-extended desk2) 792)
(check-equal? (furniture-footprint-extended bookshelf1) 180)
(check-equal? (furniture-footprint-extended bookshelf1) 180)

