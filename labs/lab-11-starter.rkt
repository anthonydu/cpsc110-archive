;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab-11-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
;; CPSC 110 - Graphs Lab

(@assignment labs/lab-11)

;; If you are:
;;   - A 110 or 107 student replace the first set of '???'s with your cwl.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   - A UBC Extended Learning student, replace the first set of ??? with
;;     the your email address as confirmed in the email you received from
;;     extended learning.  The handin password is also in that email.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   
(@cwl antdu ???)

(@problem 1)

(@htdd Venue)
(define-struct vnu (nm los))
;; Venue is (make-vnu String (listof Streetway))
;; interp. a venue name and streetways leading from that venue

(@htdd Streetway)
(define-struct sw (nm dst))
;; Streetway is (make-sw String String)
;; interp. (make-sw s v) is a streetway s that leads to the destination venue v.

(define VENUES
  (list (make-vnu "Shine Night Club"
                  (list (make-sw "Richards Southbound"
                                 "WOW Tasty Food Delivery")
                        (make-sw "Cordova Eastbound"
                                 "Rocket Reprographics")
                        (make-sw "Cordova Westbound"
                                 "West Coast Express")))
        (make-vnu "Rocket Reprographics"
                  (list (make-sw "Cordova Eastbound"
                                 "Anti-Hero 13 Boutique")))
        (make-vnu "Anti-Hero 13 Boutique" (list))
        (make-vnu "WOW Tasty Food Delivery"
                  (list (make-sw "Hastings Eastbound"
                                 "Vancouver Film School")
                        (make-sw "Richards Southbound"
                                 "MacLeod's Books")
                        (make-sw "Hastings Westbound"
                                 "Vancouver Lookout")))
        (make-vnu "Vancouver Film School"
                  (list (make-sw "Hastings Westbound"
                                 "WOW Tasty Food Delivery")
                        (make-sw "Homer Northbound"
                                 "Rocket Reprographics")
                        (make-sw "Hastings Eastbound"
                                 "BC Marijuana Party")))
        (make-vnu "West Coast Express"
                  (list (make-sw "Cordova Eastbound"
                                 "Shine Night Club")))
        (make-vnu "Vancouver Lookout"
                  (list (make-sw "Seymour Northbound"
                                 "West Coast Express")
                        (make-sw "Hastings Eastbound"
                                 "WOW Tasty Food Delivery")))
        (make-vnu "F.M. Classic Pizza"
                  (list (make-sw "Seymour Northbound"
                                 "Vancouver Lookout")
                        (make-sw "Pender Eastbound"
                                 "MacLeod's Books")))
        (make-vnu "7-11"
                  (list (make-sw "Seymour Northbound"
                                 "F.M. Classic Pizza")))
        (make-vnu "MacLeod's Books"
                  (list (make-sw "Richards Southbound"
                                 "D&S Bubble Tea")
                        (make-sw "Pender Westbound"
                                 "F.M. Classic Pizza")
                        (make-sw "Pender Eastbound"
                                 "Capital Tax and Accounting")))
        (make-vnu "Capital Tax and Accounting"
                  (list (make-sw "Homer Northbound"
                                 "Vancouver Film School")
                        (make-sw "Pender Eastbound"
                                 "London School of Hairdressing and Aesthetics")
                        (make-sw "Pender Westbound"
                                 "MacLeod's Books")))
        (make-vnu "London School of Hairdressing and Aesthetics"
                  (list (make-sw "Hamilton Southbound"
                                 "Candy Meister")
                        (make-sw "Pender Westbound"
                                 "Capital Tax and Accounting")))
        (make-vnu "BC Marijuana Party"
                  (list (make-sw "Hamilton Southbound"
                                 "London School of Hairdressing and Aesthetics")
                        (make-sw "Hastings Westbound"
                                 "Vancouver Film School")))
        (make-vnu "Candy Meister"
                  (list (make-sw "Hamilton Northbound"
                                 "London School of Hairdressing and Aesthetics")
                        (make-sw "Dunsmuir Westbound"
                                 "BC Hydro")))
        (make-vnu "BC Hydro"
                  (list (make-sw "Homer Northbound"
                                 "Capital Tax and Accounting")
                        (make-sw "Dunsmuir Westbound"
                                 "D&S Bubble Tea")))
        (make-vnu "D&S Bubble Tea"
                  (list (make-sw "Dunsmuir Westbound"
                                 "7-11")))))



;; Consider this to be a primitive function that comes with the data definitions
;; and that given a venue name it produces the corresponding venue.  Because
;; this consumes a string and generates a venue calling it will amount to a
;; generative step in a recursion through a graph of venues and streetways.
;; You must not edit this function, but you can experiment with it to see how
;; it works.

;;(@htdf lookup-venue)
;;(@signature String -> Venue)
(define (lookup-venue name)
  (local [(define (scan lst)
            (cond [(empty? lst) (error "No venue named " name)]
                  [else
                   (if (string=? (vnu-nm (first lst)) name)
                       (first lst)
                       (scan (rest lst)))]))]
    (scan VENUES)))



;; Complete the data definition by finishing the template for fn-for-venue
;; and fn-for-los in the encapsulated fn-for-map. DO NOT ADD ACCUMULATORS,
;; simply produce the template that corresponds to the type comments.

(@template-origin encapsulated genrec Venue (listof Streetway) Streetway)
(define (fn-for-map v)
  (local [(define (fn-for-venue v)
            (... (vnu-nm v)
                 (vnu-los v)))
          
          (define (fn-for-los los)
            (cond [(empty? los) (...)]
                  [else
                   (... (first los)
                        (rest los))]))
          
          (define (fn-for-sw sw)
            (... (sw-nm sw)
                 ;; lookup-venue is the generative step that makes the whole MR
                 ;; generative
                 (fn-for-venue (lookup-venue (sw-dst sw)))))]
    (fn-for-venue v)))



;; Problem 1:

;; Design a function that, given a venue and the name of a venue IN THAT ORDER,
;; produces true if the named venue can be reached from the given venue.
;; Call it can-get-to?.

(@htdf can-get-to?)
(@signature Venue String -> Boolean)
;; produces true if the named venue can be reached from the given venue
(check-expect (can-get-to? (lookup-venue "West Coast Express")
                           "West Coast Express") true)
(check-expect (can-get-to? (lookup-venue "West Coast Express")
                           "Candy Meister") true)
(check-expect (can-get-to? (lookup-venue "7-11")
                           "Anti-Hero 13 Boutique") true)
(check-expect (can-get-to? (lookup-venue "Anti-Hero 13 Boutique")
                           "Rocket Reprographics") false)
(check-expect (can-get-to? (lookup-venue "Rocket Reprographics")
                           "7-11") false)
(check-expect (can-get-to? (lookup-venue "Rocket Reprographics")
                           "7-11") false)

(@template-origin encapsulated genrec accumulator
                  Venue (listof Streetway) Streetway)

(define (can-get-to? v d)
  ;; trivial:   reaches destination venue
  ;; reduction: move to a new venue through streetways
  ;; argument:  the number of streetways is finite,
  ;;            so eventually the trivial case will be reached
  ;;            or all venues will have been visited
  
  ;; visited is (listof Venue): a list of all venues that had been visited
  (local [(define (fn-for-venue v visited)
            (cond [(string=? d (vnu-nm v)) true]
                  [(member? v visited) false]
                  [else (fn-for-los (vnu-los v) (cons v visited))]))
          
          (define (fn-for-los los visited)
            (cond [(empty? los) false]
                  [else
                   (or (fn-for-sw (first los) visited)
                       (fn-for-los (rest los) visited))]))
          
          (define (fn-for-sw sw visited)
            (fn-for-venue (lookup-venue (sw-dst sw)) visited))]
    (fn-for-venue v empty)))



;; Problem 2:

;; Design a function, called find-route, that given a venue and the name of
;; some other venue IN THAT ORDER, produces a list of names of streetways that
;; can get you to the named venue (i.e a list of driving directions), or false
;; if the named venue cannot be reached from the given venue.

;; Your find-route design does not need to be tail-recursive. But if you finish
;; early, try refactoring your solution so it is! You may want to keep a copy
;; of your non-tail-recursive solution to hand in.

(@problem 2)
(@htdf find-route)
(@signature Venue String -> (listof String) or false)
;; produces a list of names of streetways that can get you to the named venue
(check-expect (find-route (lookup-venue "West Coast Express")
                          "West Coast Express") empty)
(check-expect (find-route (lookup-venue "West Coast Express")
                          "Candy Meister") (list
                                            "Cordova Eastbound"
                                            "Richards Southbound"
                                            "Hastings Eastbound"
                                            "Hastings Eastbound"
                                            "Hamilton Southbound"
                                            "Hamilton Southbound"))
(check-expect (find-route (lookup-venue "7-11")
                          "Anti-Hero 13 Boutique") (list
                                                    "Seymour Northbound"
                                                    "Seymour Northbound"
                                                    "Seymour Northbound"
                                                    "Cordova Eastbound"
                                                    "Richards Southbound"
                                                    "Hastings Eastbound"
                                                    "Homer Northbound"
                                                    "Cordova Eastbound"))
(check-expect (find-route (lookup-venue "Anti-Hero 13 Boutique")
                          "Rocket Reprographics") false)
(check-expect (find-route (lookup-venue "Rocket Reprographics")
                          "7-11") false)
(check-expect (find-route (lookup-venue "Rocket Reprographics")
                          "7-11") false)

(@template-origin encapsulated genrec accumulator try-catch
                  Venue (listof Streetway) Streetway)

(define (find-route v d)
  ;; trivial:   reaches destination venue
  ;; reduction: move to a new venue through streetways
  ;; argument:  the number of streetways is finite,
  ;;            so eventually the trivial case will be reached
  ;;            or all venues will have been visited

  ;; path is (listof Streetway): a list of streetways to take
  ;;                             to reach destination venue
  ;; visited is (listof Venue): a list of all venues that had been visited
  (local [(define (fn-for-venue v path visited)
            (cond [(string=? d (vnu-nm v)) path]
                  [(member? v visited) false]
                  [else (fn-for-los (vnu-los v)
                                    path
                                    (cons v visited))]))
          
          (define (fn-for-los los path visited)
            (cond [(empty? los) false]
                  [else
                   (local [(define try (fn-for-sw (first los)
                                                  (append path
                                                          (list
                                                           (sw-nm (first los))))
                                                  visited))]
                     (if (not (false? try))
                         try
                         (fn-for-los (rest los)
                                     path
                                     visited)))]))
          
          (define (fn-for-sw sw path visited)
            (fn-for-venue (lookup-venue (sw-dst sw)) path visited))]
    (fn-for-venue v empty empty)))