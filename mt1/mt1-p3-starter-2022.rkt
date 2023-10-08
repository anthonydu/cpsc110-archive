;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mt1-p3-starter-2022) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment exams/2022w1-mt1/mt1-p3)

(@cwl antdu)   ;fill in your CWL here (same as for problem sets)

(@problem 1) ;do not edit or delete this line
(@problem 2) ;do not edit or delete this line
(@problem 3) ;do not edit or delete this line

#|

Given the following data definition:

|#

(@htdd ListOfNatural)
;; ListOfNatural is one of:
;;  - empty
;;  - (cons Natural ListOfNatural)
;; interp. a list of natural numbers

(define LON1 empty)
(define LON2 (cons 1 (cons 0 (cons 4 empty))))

(@dd-template-rules one-of
                    atomic-distinct
                    compound
                    self-ref)

(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))


#|

Design a function that consumes a list of naturals and drops naturals from the
front of a list until it reaches a natural that is odd. So, for example:

(drop-until-odd (cons 2 (cons 5 empty)))

should produce

(cons 5 empty)

But note that it only drops until it reaches the first odd, so:

(drop-until-odd (cons 4 (cons 2 (cons 1 (cons 6 (cons 5 empty))))))

should produce

(cons 1 (cons 6 (cons 5 empty)))

because it stops dropping at the 1.

BSL has a built in predicated called odd? that you will want to use.

Your function design must include a @signature, purpose, commented out stub,
appropriate tests, a @template-origin tag, a @template tag, and a function
definition.

This problem will be autograded.  NOTE that all of the following are required.
Violating one or more will cause your solution to receive 0 marks.

  - Files must not have any errors when the Check Syntax button is pressed.
    Press Check Syntax and Run often, and correct any errors early.

  - You must define a function with the same name as in the @htdf tag below.

|#

;; Functions

(@htdf drop-until-odd)
(@signature ListOfNatural -> ListOfNatural)
;; drop naturals from the front of a list until it reaches an odd natural
(check-expect (drop-until-odd (drop-until-odd empty)) empty)
(check-expect (drop-until-odd (drop-until-odd (list 2 5)))
              (list 5))
(check-expect (drop-until-odd (drop-until-odd (list 4 2 1 6 5)))
              (list 1 6 5))
(check-expect (drop-until-odd (drop-until-odd (list 2 4 6 8 10)))
              empty)

; (define (drop-until-odd lon) lon) ; stub

(@template-origin ListOfNatural)

(@template
 (define (drop-until-odd lon)
   (cond [(empty? lon) (...)]
         [else
          (... (first lon)
               (drop-until-odd (rest lon)))])))

(define (drop-until-odd lon)
  (cond [(empty? lon) lon]
        [else
         (if (odd? (first lon))
             lon
             (drop-until-odd (rest lon)))]))

