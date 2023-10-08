;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname m07-2-one-of-prefixes-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment lectures/m07-2-one-of-prefixes)

(@cwl antdu) ;replace ??? with your cwl

(@problem 1)
#|
Design a function that consumes two lists of strings and produces
true if the first list is a prefix of the second, meaning that 
the first list matches, 1 for 1, the beginning of the second list.

For example:

  (prefix=? (list "a" "b") (list "a" "b" "c")) --> true
  (prefix=? (list "a" "b") (list "b" "c"))     --> false
  (prefix=? (list "a" "b") (list "a" "b"))     --> true
  (prefix=? (list "a" "b") (list "a"))         --> false

  
As a reminder, here is a data definition for a list of strings. To 
save space later we are calling it LOS instead of ListOfString.
|#

;; Data Definitions:

(@htdd LOS)
;; LOS is one of:
;;  - empty
;;  - (cons String LOS)
;; interp. a list of strings
(define LOS1 empty)
(define LOS2 (cons "a" (cons "b" empty)))

(@template-origin LOS)

(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; Function:

(@htdf prefix=?)
(@signature LOS LOS -> Boolean)
;; produces true if the first LOS is a prefix of the second
(check-expect (prefix=? empty empty) true)
(check-expect (prefix=? empty (list "a" "b")) true)
(check-expect (prefix=? (list "a" "b") empty) false)
(check-expect (prefix=? (list "a") (list "a")) true)
(check-expect (prefix=? (list "a") (list "b")) false)
(check-expect (prefix=? (list "a") (list "a" "b")) true)
(check-expect (prefix=? (list "a" "b") (list "a")) false)

; (define (prefix=? los1 los2) false) ; stub

(@template-origin 2-one-of)

(define (prefix=? los1 los2)
  (cond [(empty? los1) true]
        [(empty? los2) false]
        [else
         (and (string=? (first los1) (first los2))
              (prefix=? (rest los1) (rest los2)))]))

