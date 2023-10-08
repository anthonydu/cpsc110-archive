;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m02-grade-standing-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment lectures/m02-grade-standing)

(@cwl antdu)

;;
;; YOU MUST CONSULT THE m02-grade-standing-documents.rkt
;; starter file which has documents you will need.  But do all your
;; actual work in this starter file.
;;
;; We want to design a program to operate on submitted grades. For now 
;; JUST FOCUS ON THE % GRADE AND STANDING.  AFTER designing a representation
;; for this information, you must design two functions:
;;
;;  - A fn that consumes grade/standing and produces true if
;;    the grade/standing is >= 90.  Call this function excellent?
;;
;;  - A fn that consumes grade/standing and produces strings
;;    like  "90%"  "75%" "P" "F". Call this function grade->string


(@problem 1)
;; Data definition goes here:
(@htdd GradeStanding)
;; GradeStanding is one of:
;; - Natrual
;; - "H"
;; - "P"
;; - "F"
;; - "T"
;; interp. a percent grade or a standing
;; CONSTRAINT: If a Natrual, is in [0, 100]
(define GS1 100)
(define GS2 50)
(define GS3 "H")
(define GS4 "P")
(define GS5 "F")
(define GS6 "T")

(@dd-template-rules one-of              ; 5 cases
                    atomic-non-distinct ; Natrual
                    atomic-distinct     ; "H"
                    atomic-distinct     ; "P"
                    atomic-distinct     ; "F"
                    atomic-distinct)    ; "T"

; template
(define (fn-for-grade-standing gs)
  (cond [(number? gs) (... gs)]
        [(string=? gs "H") (...)]
        [(string=? gs "P") (...)]
        [(string=? gs "F") (...)]
        [else (...)]))

(@htdf excellent?)
(@signature GradeStanding -> Boolean)
;; produces true if the GradeStanding is >= 90

(check-expect (excellent? 91) true)
(check-expect (excellent? 90) true)
(check-expect (excellent? 89) false)
(check-expect (excellent? "H") false)
(check-expect (excellent? "P") false)
(check-expect (excellent? "F") false)
(check-expect (excellent? "T") false)

; (define (excellent? gs) false) ; stub

(@template-origin GradeStanding)
(@template (define (fn-for-grade-standing gs)
             (cond [(number? gs) (... gs)]
                   [(string=? gs "H") (...)]
                   [(string=? gs "P") (...)]
                   [(string=? gs "F") (...)]
                   [else (...)])))

(define (excellent? gs)
  (cond [(number? gs) (>= gs 90)]
        [(string=? gs "H") false]
        [(string=? gs "P") false]
        [(string=? gs "F") false]
        [else false]))



(@problem 2)

(@htdf grade->string)
(@signature GradeStanding -> String)
;; produces strings like "90%", "75%", "P", "F"

(check-expect (grade->string 0) "0%")
(check-expect (grade->string "H") "H")
(check-expect (grade->string "P") "P")
(check-expect (grade->string "F") "F")
(check-expect (grade->string "T") "T")

;(define (grade->string gs) "") ; stub

(@template-origin GradeStanding)
(@template (define (fn-for-grade-standing gs)
             (cond [(number? gs) (... gs)]
                   [(string=? gs "H") (...)]
                   [(string=? gs "P") (...)]
                   [(string=? gs "F") (...)]
                   [else (...)])))

(define (grade->string gs)
  (cond [(number? gs) (string-append (number->string gs) "%")]
        [(string=? gs "H") "H"]
        [(string=? gs "P") "P"]
        [(string=? gs "F") "F"]
        [else "T"]))
