;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mt1-p4-starter-2022) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment exams/2022w1-mt1/mt1-p4)

(@cwl antdu)   ;fill in your CWL here (same as for problem sets)

(@problem 1) ;do not edit or delete this line
(@problem 2) ;do not edit or delete this line
(@problem 3) ;do not edit or delete this line
(@problem 4) ;do not edit or delete this line

#|
 
Given the following data definitions:

|#

(@htdd Package)
(define-struct pkg (vol wei des fra?))
;; Package is (make-pkg Number Natural String Boolean)

;; interp. A package, with (vol)ume in centimeters,
;;         (wei)ght of the package in grams
;;         code of (des)tination airport
;;         (fra)gile? is true if the package contains fragile items

(define P1 (make-pkg 20  100 "YYZ" false))
(define P2 (make-pkg 50  200 "YVR" false))
(define P3 (make-pkg 100 200 "YVR" true))
(define P4 (make-pkg 35  300 "YUL" true))

(@dd-template-rules compound)

(define (fn-for-pkg p)
  (... (pkg-vol p)
       (pkg-wei p)
       (pkg-des p)
       (pkg-fra? p)))

(@htdd ListOfPackage)
;; ListOfPackage is one of:
;; - empty
;; - (cons Package ListOfPackage)
;; interp. a list of packages

(define LOP1 empty)
(define LOP2 (cons P2 (cons P3 empty)))

(@dd-template-rules one-of
                    atomic-distinct
                    compound
                    ref
                    self-ref)

(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (fn-for-pkg (first lop))
              (fn-for-lop (rest lop)))]))

#|

Design a function that produces the total weight of all the packages in the list
that both contain fragile items and have a volume less than given.  For example:

  (delivery-weight 40 (cons P1 (cons P3 (cons P2 (cons P4 empty)))))

should produce 300 since P4 is the only package in the list that contains
fragile items and has a volume less than 40.  

  (delivery-weight 150 (cons P3 (cons P1 (cons P4 empty))))

should produce 500 since both P3 and P4 contain fragile items and have a volume 
less that 150; their combined weights is 300 + 200.

This problem with be autograded. Failure to comply with any of the following
will result in a significantly reduced score, or in many cases 0.

 - The function must be called delivery-weight. 
 - It must accept its arguments in the order shown above.
 - You must not in any way edit, add to, or otherwise modify any of the
   data definition above.
 - You must not comment out any of the supplied @ metadata tags.
 - You must include @htdf tag, @signature tag, purpose, sufficient tests, a
   a commented out stub, a @template-origin tag, a @template tag, and a working
   function definition.
 - You must follow all applicable design rules, including helper rules. 
 - There must be no errors when pressing Check Syntax.

|#

(@htdf delivery-weight)
(@signature Number ListOfPackage -> Number)
;; return the total weight of all packages that are fragile and smaller
(check-expect (delivery-weight 0 empty) 0)                   ; zero vol, empty
(check-expect (delivery-weight 0 (list P1 P3 P2 P4)) 0)      ; zero vol only
(check-expect (delivery-weight 36 empty) 0)                  ; empty list only

(check-expect (delivery-weight 34 (list P1 P2 P3 P4)) 0)     ; P4 - 1
(check-expect (delivery-weight 35 (list P2 P3 P4 P1)) 0)     ; P4
(check-expect (delivery-weight 36 (list P3 P4 P1 P2)) 300)   ; P4 + 1

(check-expect (delivery-weight 99 (list P1 P3 P2 P4)) 300)   ; P3 - 1
(check-expect (delivery-weight 100 (list P4 P1 P2 P3)) 300)  ; P3
(check-expect (delivery-weight 101 (list P3 P4 P1 P2)) 500)  ; P3 + 1

; (define (delivery-weight vol lop) 0) ; stub

(@template-origin ListOfPackage)
(@template
 (define (delivery-weight vol lop)
   (cond [(empty? lop) (...)]
         [else
          (... (fn-for-pkg (first lop))
               (delivery-weight (rest lop)))])))

(define (delivery-weight vol lop)
  (cond [(empty? lop) 0]
        [else
         (+ (if (small-and-fragile? vol (first lop))
                (pkg-wei (first lop))
                0)
            (delivery-weight vol (rest lop)))]))

















(@htdf small-and-fragile?)
(@signature Number Package -> Boolean)
;; return whether the Package has a volumn smaller than a number
(check-expect (small-and-fragile? 19 P1) false)
(check-expect (small-and-fragile? 20 P1) false)
(check-expect (small-and-fragile? 21 P1) false)

(check-expect (small-and-fragile? 49 P2) false)
(check-expect (small-and-fragile? 50 P2) false)
(check-expect (small-and-fragile? 51 P2) false)

(check-expect (small-and-fragile? 99 P3) false)
(check-expect (small-and-fragile? 100 P3) false)
(check-expect (small-and-fragile? 101 P3) true)

(check-expect (small-and-fragile? 34 P4) false)
(check-expect (small-and-fragile? 35 P4) false)
(check-expect (small-and-fragile? 36 P4) true)

; (define (small-and-fragile? vol p) 0) ; stub

(@template-origin Package)
(@template
 (define (small-and-fragile? vol p)
   (... (pkg-vol p)
        (pkg-wei p)
        (pkg-des p)
        (pkg-fra? p))))

(define (small-and-fragile? vol p)
  (and (< (pkg-vol p) vol) (pkg-fra? p)))





