;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m02-status-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))

(require spd/tags)

(@assignment lectures/m02-status)

(@cwl antdu) ;replace ??? with your cwl

#|

;; Is this function correct?? (ignore the fact that it is commented out)

(define (can-vote? s)
  (= s 1))




;; How about now? Is it correct now?

(@htdf can-vote?)
(@signature Natural -> Boolean)
;; produces true if a person with given status is eligible to vote
(check-expect (can-vote? 0) false)
(check-expect (can-vote? 1) true)

(@template-origin Natural)

(define (can-vote? s)
  (= s 1))


;; What's the problem?

|#
(@problem 1)

(@htdd Status)
;; Status is one of:
;; - "minor"
;; - "adult"
;; interp. the legal status of a person
;; <examples are redundant for enumerations>

(@dd-template-rules one-of             ; 2 cases
                    atomic-distinct    ; "minor"
                    atomic-distinct)   ; "adult"

; template
(define (fn-for-status s)
  (cond [(string=? s "minor") (...)]   ; cond is essentially switch-case
        [(string=? s "adult") (...)]))

(@htdf can-vote?)
(@signature Status -> Boolean)
;; produce true if a person is eligible to vote and false if otherwise

(check-expect (can-vote? "adult") true)
(check-expect (can-vote? "minor") false)

; (define (can-vote? s) false) ; stub

(@template-origin Status)
(@template (define (fn-for-status s)
             (cond [(string=? s "minor") (...)]
                   [(string=? s "adult") (...)])))

(define (can-vote? s)
             (cond [(string=? s "minor") false]
                   [(string=? s "adult") true]))













