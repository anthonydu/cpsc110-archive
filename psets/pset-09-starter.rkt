;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pset-09-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR
;; PARTNER
;;

(require spd/tags)

(@assignment psets/pset-09);Do not edit or remove this tag

;; If you are:
;;   - A 110 or 107 student replace the first set of '???'s with your cwl.
;;     For problem sets, If you have a partner, please replace the second
;;     set of '???'s with their cwl.  Remember this, it is what you will
;;     do with these @cwl annotations for the whole course.
;;   - A UBC Extended Learning student, replace the first set of ??? with
;;     your email address as confirmed in the email you received from
;;     extended learning.  The handin password is also in that email.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   
(@cwl antdu ???)

;;
;; THIS IS THE MOST CHALLENGING PROBLEM SET SO FAR THIS TERM.  PLEASE BE
;; SURE YOU WORK THROUGH IT CAREFULLY THOUGH.  110 FINAL EXAMS OFTEN INCLUDE
;; PROBLEMS BASED ON PROBLEM SETS 9, 10, OR 11.
;;
;; ALSO NOTE THAT THE AUTOGRADER COOL DOWN IS 1 HOUR FOR THIS PROBLEM SET.
;;
;; THERE WILL BE A SPECIAL PINNED THREAD ON PIAZZA IN WHICH WE WILL ANSWER
;; QUESTIONS ABOUT THIS PROBLEM SET.
;;
;; In this problem set you are going to work on one of the toughest problems
;; we face running 110 - scheduling of TAs.  As you may know, we have about
;; 45 TAs, and we have to schedule them for many labs, 3 lectures, and office
;; hours.  We solved this by writing a schedule solver, and that's what you
;; are going to do for this problem set.
;;
;; We are making it a little easier for you, in that all you will be having
;; to deal with is labs. 
;;
;; We are giving you two key data definitions, as well as some examples of
;; that data.  We are also giving you a wish list entry for the main solve
;; function you have to design.  The function consumes a list of TAs and a
;; list of lab slots to fill.  It produces a list of assignments.  So, for
;; example, in the following very simple case, where there are two slots,
;; and two TAs, you get an assignment of the TAs to those slots.
;;
;; (solve (list (make-slot "A" 1) (make-slot "B" 1))
;;        (list (make-ta "Ali" (list "B"))
;;              (make-ta "Azi" (list "A"))))  ==>
;;
;; (list (list "Ali" "B") (list "Azi" "A"))
;;
;; In this simple example there was only one possible assignment. But in
;; general there might be more than one assignment, or it might be impossible
;; to generate an assignment.

;; By now you know enough about search to know that the first thing you need
;; to do is figure out the search space.  What does the tree look like? What
;; information do you have to represent at each node in the tree?  How do you
;; generate the next nodes in the tree? How do you tell you have a solution?
;; Although this function ends up producing just a list of assignments, does
;; it need more than just the assignments so far at each node in the tree?
;; What other information do you need to represent at each node?
;;
;; As you consider the search tree, note that a TA can work more than one slot,
;; but a slot is filled by one TA.  So once you assign a TA to a slot that slot
;; is done.
;;
;; As usual, anything we give you below you must not change.  The autograder
;; will want to call solve with arguments as described below.  Also note that
;; we give you just a few small data examples for illustration. You will want
;; to test your function with additional examples.
;;

(@problem 1)

;; Constants:

(define MAX-SLOTS-PER-TA 3) ;this is the max number of labs a TA can work

;; Data definitions:

(@htdd Slot)
(define-struct slot (lab n))
;; Slot is (make-slot String Natural)
;; interp. the name of a lab - "A", "B", etc.
;;         the lab's slot number - 1, 2, 3 etc.
;;         A given lab can have multiple slots, each slot 
;;         should have a unique number in that lab.
;;  
(define SLOTS1
  (list (make-slot "A" 1) (make-slot "A" 2)   ;A needs 2 TAs
        (make-slot "B" 1)                     ;B needs 1
        (make-slot "C" 1) (make-slot "C" 2))) ;C needs 2
(define SLOTS1.5
  (list (make-slot "A" 1) (make-slot "A" 2)
        (make-slot "B" 1) (make-slot "B" 2)
        (make-slot "C" 1) (make-slot "C" 2)))
(define SLOTS1.6
  (list (make-slot "A" 1) (make-slot "A" 2)
        (make-slot "B" 1)
        (make-slot "C" 1) (make-slot "C" 2)
        (make-slot "D" 1)))
(define SLOTS2
  (list (make-slot "A" 1) (make-slot "A" 2) (make-slot "A" 3) 
        (make-slot "B" 1)
        (make-slot "C" 1) (make-slot "C" 2) (make-slot "C" 3) (make-slot "C" 4)
        (make-slot "D" 1) (make-slot "D" 2) (make-slot "D" 3) 
        (make-slot "E" 1) (make-slot "E" 2)
        (make-slot "F" 1)
        (make-slot "G" 1) (make-slot "G" 2) (make-slot "G" 3)
        (make-slot "H" 1) (make-slot "H" 2)
        (make-slot "I" 1)
        (make-slot "J" 1) (make-slot "J" 2)
        (make-slot "K" 1) (make-slot "K" 2) 
        (make-slot "L" 1) (make-slot "L" 2) (make-slot "L" 3)))

(@htdd TA)
(define-struct ta (name avail))
;; TA is (make-ta String (listof String))
;; interp. A TA with their name and the labs names they are free to work.

(define TAS1
  (list (make-ta "Ali" (list "A" "B"))
        (make-ta "Ari" (list     "B" "C"))
        (make-ta "Azi" (list "A"     "C"))))
(define TAS2
  (list (make-ta "Aai" (list "A"             "E"                     "K"))
        (make-ta "Abi" (list     "B" "C"         "F"             "J"))
        (make-ta "Aci" (list         "C" "D"                             "L"))
        (make-ta "Adi" (list "A"         "D"         "G" "H"))
        (make-ta "Aei" (list         "C"                     "I"         "L"))
        (make-ta "Afi" (list                 "E"     "G"))
        (make-ta "Agi" (list "A"         "D"                             "L"))
        (make-ta "Ahi" (list         "C"             "G"))
        (make-ta "Aii" (list                             "H"     "J" "K"))))
(define TAS2.5
  (list (make-ta "Aai" (list "A"             "E"                     "K"))
        (make-ta "Abi" (list     "B" "C"         "F"             "J"))
        (make-ta "Aci" (list         "C" "D"                             "L"))
        (make-ta "Adi" (list "A"         "D"         "G" "H"))
        (make-ta "Aei" (list         "C"                     "I"         "L"))
        (make-ta "Afi" (list                 "E"     "G"))
        (make-ta "Agi" (list "A"         "D"                             "L"))
        (make-ta "Ahi" (list         "C"             "G"))
        (make-ta "Aii" (list                             "H"     "J" "K"))
        (make-ta "Aji" (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L"))))
(define TAS3
  (list (make-ta "Mti" empty)
        (make-ta "Ali" (list "A"        ))
        (make-ta "Ari" (list         "C"))
        (make-ta "Azi" (list "A" "B" "C"))))
(define TAS3.5
  (list (make-ta "Mti" empty)
        (make-ta "Ali" (list "A"        ))
        (make-ta "Ari" (list         "C"))
        (make-ta "Azi" (list "A" "B" "C" "D"))))


(@htdf solve)
(@signature (listof Slot) (listof TA) -> (listof (listof String)) or false)
;; produce assignments of form (list (list "ta name" "lab name") ...)
;; CONSTRAINTS:
;;  - The list of slots must not contain duplicates (slots with the
;;    same lab name and number).
;;  - The list of TAs must not contain duplicates (tas with the
;;    same name)
(check-expect (solve empty empty) empty)
(check-expect (solve (list (make-slot "A" 1)
                           (make-slot "B" 1)) empty) false)
(check-expect (solve empty (list (make-ta "Ali" (list "B"))
                                 (make-ta "Azi" (list "A")))) empty)
(check-expect (solve (list (make-slot "" 0)) (list (make-ta "" empty))) false)
(check-expect (solve (list (make-slot "" 0))
                     (list (make-ta "" (list "")))) (list (list "" "")))
(check-expect
 (solve (list (make-slot "A" 1)
              (make-slot "B" 1))
        (list (make-ta "Ali" (list "B"))
              (make-ta "Azi" (list "A"))))
 (list (list "Azi" "A") (list "Ali" "B")))
(check-expect
 (solve (list (make-slot "A" 1)
              (make-slot "B" 1) (make-slot "B" 2))
        (list (make-ta "Ali" (list "B"))
              (make-ta "Azi" (list "A"))))
 false)
(check-expect (solve SLOTS1 TAS1) (list (list "Ali" "A")
                                        (list "Azi" "A")
                                        (list "Ali" "B")
                                        (list "Ari" "C")
                                        (list "Azi" "C")))
(check-expect (solve SLOTS1 TAS3) (list (list "Ali" "A")
                                        (list "Azi" "A")
                                        (list "Azi" "B")
                                        (list "Ari" "C")
                                        (list "Azi" "C")))
(check-expect (solve SLOTS1.5 TAS1) (list (list "Ali" "A")
                                          (list "Azi" "A")
                                          (list "Ali" "B")
                                          (list "Ari" "B")
                                          (list "Ari" "C")
                                          (list "Azi" "C")))
(check-expect (solve SLOTS1.6 TAS3) false)
(check-expect (solve SLOTS2 TAS1) false)
#;
(check-expect (solve SLOTS1 TAS2) (list (list "Aai" "A")
                                        (list "Adi" "A")
                                        (list "Abi" "B")
                                        (list "Abi" "C")
                                        (list "Aci" "C")))
(check-expect (solve SLOTS1 TAS2) (list (list "Aai" "A")
                                        (list "Agi" "A")
                                        (list "Abi" "B")
                                        (list "Ahi" "C")
                                        (list "Aci" "C")))
(check-expect (solve SLOTS2 TAS2) false)
#;
(check-expect (solve SLOTS2 TAS2.5) (list (list "Aai" "A")
                                          (list "Adi" "A")
                                          (list "Agi" "A")
                                          (list "Abi" "B")
                                          (list "Abi" "C")
                                          (list "Aci" "C")
                                          (list "Aei" "C")
                                          (list "Ahi" "C")
                                          (list "Aci" "D")
                                          (list "Adi" "D")
                                          (list "Agi" "D")
                                          (list "Aai" "E")
                                          (list "Afi" "E")
                                          (list "Abi" "F")
                                          (list "Adi" "G")
                                          (list "Afi" "G")
                                          (list "Ahi" "G")
                                          (list "Aii" "H")
                                          (list "Aji" "H")
                                          (list "Aei" "I")
                                          (list "Aii" "J")
                                          (list "Aji" "J")
                                          (list "Aai" "K")
                                          (list "Aii" "K")
                                          (list "Aci" "L")
                                          (list "Aei" "L")
                                          (list "Agi" "L")))
(check-expect (solve SLOTS2 TAS2.5) (list (list "Aai" "A")
                                          (list "Agi" "A")
                                          (list "Adi" "A")
                                          (list "Abi" "B")
                                          (list "Ahi" "C")
                                          (list "Aci" "C")
                                          (list "Aei" "C")
                                          (list "Abi" "C")
                                          (list "Aci" "D")
                                          (list "Agi" "D")
                                          (list "Adi" "D")
                                          (list "Afi" "E")
                                          (list "Aai" "E")
                                          (list "Abi" "F")
                                          (list "Afi" "G")
                                          (list "Ahi" "G")
                                          (list "Adi" "G")
                                          (list "Aii" "H")
                                          (list "Aji" "H")
                                          (list "Aei" "I")
                                          (list "Aii" "J")
                                          (list "Aji" "J")
                                          (list "Aai" "K")
                                          (list "Aii" "K")
                                          (list "Aci" "L")
                                          (list "Aei" "L")
                                          (list "Agi" "L")))

(@template-origin genrec encapsulated try-catch)
;; trivial: all slots allocated
;; reduction: find a ta to fulfill a slot if possible
;; argument: slots are finite, so keep allocating
;;           will evetually fulfill all of them
#;
(define (solve slots tas)
  (local [;; creates a list of slots matched with an availiable TA
          (define (loop-slots slots tas) ; fn-for-slots
            (cond [(empty? slots) empty]
                  [else
                   (local [(define first-slot (first slots))
                           (define avail-ta (find-avail-ta first-slot tas))
                           (define rnr
                             (loop-slots (rest slots)
                                         (remove-ta-slot tas
                                                         avail-ta
                                                         first-slot)))]
                     ; if all previous matches were sucessful
                     ; and there is an availiable TA for the current slot
                     (if (and (not (false? avail-ta))
                              (not (false? rnr)))
                         (cons (list (ta-name avail-ta)
                                     (slot-lab first-slot))
                               rnr)
                         false))]))

          ;; finds and returns the first availiable TA for a slot
          (define (find-avail-ta slot tas) ; fn-for-tas
            (cond [(empty? tas) false]
                  [else
                   (local [(define first-ta (first tas))
                           (define (has-slot? s) (eq? (slot-lab slot) s))]
                     (if (and (< (length (filter false?
                                                 (ta-avail first-ta)))
                                 MAX-SLOTS-PER-TA)
                              (ormap has-slot? (ta-avail first-ta)))
                         (first tas)
                         (find-avail-ta slot (rest tas))))]))

          ;; generates a new list of TA's with a TA's slot replaced by false
          (define (remove-ta-slot tas ta slot) ; helper
            (cond [(empty? tas) empty]
                  [else
                   (if (eq? (first tas) ta)
                       (cons (make-ta (ta-name ta)
                                      (cons false
                                            (remove (slot-lab slot)
                                                    (ta-avail ta))))
                             (rest tas))
                       (cons (first tas)
                             (remove-ta-slot (rest tas) ta slot)))]))]
    
    (loop-slots slots tas)))

(define (solve slots tas)
  (local [;; creates a list of slots matched with an availiable TA
          (define (loop-slots slots tas) ; fn-for-slots
            (cond [(empty? slots) empty]
                  [else
                   (local [(define first-slot (first slots))
                           (define avail-ta (find-avail-ta first-slot tas))
                           (define rnr
                             (loop-slots (rest slots)
                                         (remove-ta-slot tas
                                                         avail-ta
                                                         first-slot)))]
                     ; if all previous matches were sucessful
                     ; and there is an availiable TA for the current slot
                     (if (and (not (false? avail-ta))
                              (not (false? rnr)))
                         (cons (list (ta-name avail-ta)
                                     (slot-lab first-slot))
                               rnr)
                         false))]))

          ;; finds and returns the first availiable TA for a slot
          (define (find-avail-ta slot tas) ; fn-for-tas
            (cond [(empty? tas) false]
                  [else
                   (local [(define first-ta (first tas))
                           (define (has-slot? s) (eq? (slot-lab slot) s))]
                     (if (and (< (length (filter false?
                                                 (ta-avail first-ta)))
                                 MAX-SLOTS-PER-TA)
                              (ormap has-slot? (ta-avail first-ta)))
                         (first tas)
                         (find-avail-ta slot (rest tas))))]))

          ;; generates a new list of TA's with a TA's slot replaced by false
          (define (remove-ta-slot tas ta slot) ; helper
            (cond [(empty? tas) empty]
                  [else
                   (if (eq? (first tas) ta)
                       (cons (make-ta (ta-name ta)
                                      (cons false
                                            (remove (slot-lab slot)
                                                    (ta-avail ta))))
                             (rest tas))
                       (cons (first tas)
                             (remove-ta-slot (rest tas) ta slot)))]))

          ;; sorts a list of TA's by their number of availiablities
          (define (sort-tas-by-avail tas)
            (cond [(empty? tas) empty]
                  [else
                   (local [(define (insert ta tas)
                             (cond [(empty? tas) (cons ta empty)]
                                   [else
                                    (if (> (length (ta-avail ta))
                                           (length (ta-avail (first tas))))
                                        (cons (first tas)
                                              (insert ta
                                                      (rest tas)))
                                        (cons ta tas))]))]
                     (insert (first tas)
                             (sort-tas-by-avail (rest tas))))]))]
    
    (loop-slots slots (sort-tas-by-avail tas))))















; instructors' check-expects

(check-expect (solve empty empty) empty) 
(check-expect (solve empty (list (make-ta "Ali" (list "A"))))
              empty)
(check-expect (solve (list (make-slot "A" 1)) empty) false) 
(check-expect (solve (list (make-slot "A" 1) 
                           (make-slot "A" 2))
                     (list (make-ta "Ali" (list "A" "B"))))
              false)
(check-expect (solve (list (make-slot "B" 1) 
                           (make-slot "C" 1))
                     (list (make-ta "Ali" (list "A"))
                           (make-ta "Azi" (list "B"))))
              false)
(check-expect (solve (list (make-slot "A" 1)
                           (make-slot "B" 1)
                           (make-slot "C" 1)
                           (make-slot "D" 1))
                     (list (make-ta "Ali" (list "A" "B" "C" ))))
              false)
(check-satisfied (solve (list (make-slot "A" 1) 
                              (make-slot "B" 1)
                              (make-slot "C" 1))
                        (list (make-ta "Ali" (list "A" "B" "C" ))))
                 (lambda (pairs)
                   (valid-schedule? (list (make-slot "A" 1) 
                                          (make-slot "B" 1)
                                          (make-slot "C" 1))
                                    (list (make-ta "Ali" (list "A" "B" "C" )))
                                    pairs)))
(check-satisfied (solve (list (make-slot "A" 1) 
                              (make-slot "B" 1)
                              (make-slot "C" 1)
                              (make-slot "D" 1))
                        (list (make-ta "Ali" (list "A" "B" "C" "D"))
                              (make-ta "Sam" (list "D"))))
                 (lambda (pairs)
                   (valid-schedule? (list (make-slot "A" 1) 
                                          (make-slot "B" 1)
                                          (make-slot "C" 1)
                                          (make-slot "D" 1))
                                    (list (make-ta "Ali" (list "A" "B" "C" "D"))
                                          (make-ta "Sam" (list "D")))
                                    pairs)))
(check-satisfied (solve SLOTS1 TAS1)
                 (lambda (pairs)
                   (valid-schedule? SLOTS1 TAS1 pairs)))
(check-satisfied (solve (list (make-slot "A" 1)
                              (make-slot "B" 1)
                              (make-slot "C" 1)
                              (make-slot "D" 1))
                        (list (make-ta "Ali"  (list "A" "B" "C" "D"))
                              (make-ta "Sam"  (list "B" "C"))
                              (make-ta "Alex" (list "C"))))
                 (lambda (pairs)
                   (valid-schedule? (list (make-slot "A" 1)
                                          (make-slot "B" 1)
                                          (make-slot "C" 1)
                                          (make-slot "D" 1))
                                    (list (make-ta "Ali"
                                                   (list "A" "B" "C" "D"))
                                          (make-ta "Sam"
                                                   (list "B" "C"))
                                          (make-ta "Alex"
                                                   (list "C")))
                                    pairs)))
(check-satisfied (solve (list (make-slot "A" 1)
                              (make-slot "A" 2)
                              (make-slot "B" 1)
                              (make-slot "B" 2)
                              (make-slot "C" 1)
                              (make-slot "C" 2)
                              (make-slot "D" 1))
                        (list (make-ta "Ali"
                                       (list "A" "B" "C"))
                              (make-ta "Sam"
                                       (list "A" "B" "C" "D"))
                              (make-ta "Ari" (list "A"))))
                 (lambda (pairs)
                   (valid-schedule? (list (make-slot "A" 1) 
                                          (make-slot "A" 2)
                                          (make-slot "B" 1)
                                          (make-slot "B" 2)
                                          (make-slot "C" 1)
                                          (make-slot "C" 2)
                                          (make-slot "D" 1))
                                    (list (make-ta "Ali"
                                                   (list "A" "B" "C"))
                                          (make-ta "Sam" 
                                                   (list "A" "B" "C" "D"))
                                          (make-ta "Ari" (list "A")))
                                    pairs)))

(check-satisfied (solve (list (make-slot "A" 1) 
                              (make-slot "B" 1)
                              (make-slot "B" 2)
                              (make-slot "C" 1)
                              (make-slot "C" 2)
                              (make-slot "D" 1)
                              (make-slot "E" 1))
                        (list (make-ta "Ali"
                                       (list "A" "B" "C" "D"))
                              (make-ta "Azi"
                                       (list "A" "B" "C" "E"))
                              (make-ta "Sam" (list "A"))))
                 (lambda (pairs)
                   (valid-schedule? (list (make-slot "A" 1) 
                                          (make-slot "B" 1)
                                          (make-slot "B" 2)
                                          (make-slot "C" 1)
                                          (make-slot "C" 2)
                                          (make-slot "D" 1)
                                          (make-slot "E" 1))
                                    (list (make-ta "Ali" (list "A" "B" "C" "D"))
                                          (make-ta "Azi" (list "A" "B" "C" "E"))
                                          (make-ta "Sam" (list "A")))
                                    pairs)))
(check-expect (solve (list (make-slot "A" 1) 
                           (make-slot "B" 1)
                           (make-slot "B" 2)
                           (make-slot "C" 1)
                           (make-slot "C" 2)
                           (make-slot "D" 1)
                           (make-slot "E" 1)
                           (make-slot "E" 1))
                     (list (make-ta "Ali" (list "A" "B" "C" "D" "E" ))
                           (make-ta "Azi" (list "A" "B" "C" "D" "E"))
                           (make-ta "Sam" (list "A"))))
              false)


;; code below here is to support autograder testing of solutions

(define (valid-schedule? slots tas pairs)
  (local [;(@template-origin 2-one-of)
          (define (same-elements? l1 l2)
            (cond [(empty? l1) (empty? l2)]
                  [(empty? l2) false]
                  [else
                   (and (member (first l1) l2)
                        (same-elements? (rest l1) (remove (first l1) l2)))]))

          (define (listed-all-slots? ta pairs)
            (andmap (lambda (lab)
                      (member lab (ta-avail ta)))
                    (map second
                         (ta-pairs ta pairs))))

          (define (not-over-time? ta pairs)
            (<= (length (ta-pairs ta pairs))  MAX-SLOTS-PER-TA))

          (define (not-double-booked? ta pairs)
            (not (contains-duplicates? (map second (ta-pairs ta pairs)))))

          (define (contains-duplicates? lox)
            (cond [(empty? lox) false]
                  [else
                   (or (member (first lox) (rest lox))
                       (contains-duplicates? (rest lox)))]))

          (define (ta-pairs ta pairs)
            (filter (lambda (pair)
                      (string=? (first pair) (ta-name ta)))
                    pairs))]

    
    (and (not (false? pairs))
         (same-elements? (map slot-lab slots) ;lab names required
                         (map second pairs))  ;lab names filled
         (andmap (lambda (ta)
                   (and (listed-all-slots?  ta pairs)
                        (not-over-time?     ta pairs)
                        (not-double-booked? ta pairs)))
                 tas))))

#;
(solve (list (make-slot "A" 1) 
             (make-slot "B" 1)
             (make-slot "B" 2)
             (make-slot "C" 1)
             (make-slot "C" 2)
             (make-slot "D" 1)
             (make-slot "E" 1))
       (list (make-ta "Ali"
                      (list "A" "B" "C" "D"))
             (make-ta "Azi"
                      (list "A" "B" "C" "E"))
             (make-ta "Sam" (list "A"))))
#;; expects
(list (list "Sam" "A")
      (list "Ali" "B")
      (list "Azi" "B")
      (list "Ali" "C")
      (list "Azi" "C")
      (list "Ali" "D")
      (list "Azi" "E"))
; my function produced false
; due to the program assigning slots A, B, and C to Ali
; it made it impossible to create a schedule when it in fact could
; if Sam is assigned slot A and Ali is assigned slots B, C, and D

; one solution I can think of is to create a sort function
; that sorts tas by number of slots avaliable
; before going into the assigning process
; OR in find-avail-ta, return the ta with the least availiable slots

