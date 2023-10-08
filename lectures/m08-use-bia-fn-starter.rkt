;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname m08-use-bia-fn-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require spd/tags)

(@assignment lectures/m08-use-bia-fn)

(@cwl antdu) ;replace ??? with your cwl

#|
Complete the design of each of the following functions.  You MUST use the 
template origin provided, which is always either use-abstract-fn or
fn-composition use-abstract-fn.  To help you learn how to write functions
that use built-in abstract functions we  provide the template or other
suggestions in some of the problems.

When choosing built-in abstract functions yourselves remember:

 - map always produces a list of the same length as it's argument
 - filter produces a list of all, some or none of the elements in its 
   argument
 - foldr can produce any kind of value at all
 - andmap and ormap produce boolean values
 - buildlist produces a list with the same number of elements as its
   argument

|#

(define CIRCLE (circle 10 "solid" "red"))

(@problem 1)
(@htdf odds-minus-evens)
(@signature (listof Integer) -> Integer)
(check-expect (odds-minus-evens (list 3)) 3)
(check-expect (odds-minus-evens (list 2)) -2)
(check-expect (odds-minus-evens (list 1 4 5 2)) 0)

; (@template-origin use-abstract-fn)
(@template-origin use-abstract-fn fn-composition)

; (define (odds-minus-evens loi) 0)

#;
(define (odds-minus-evens loi)
  (local [(define (negate-even i)
            (if (even? i) (- i) i))]
    (foldr + 0 (map negate-even loi))))

(define (odds-minus-evens loi)
  (local [(define (fn i rnr) ; result of natural recursion
            (if (odd? i) 
                (+ rnr i)
                (- rnr i)))]
    (foldr fn 0 loi)))


(@problem 2)
(@htdf sum-larger-than)
(@signature Integer (listof Integer) -> Integer)
;; produce sum of all elements of loi > n
(check-expect (sum-larger-than 4 (list 3)) 0)
(check-expect (sum-larger-than 5 (list 5)) 0)
(check-expect (sum-larger-than 6 (list 6)) 0)
(check-expect (sum-larger-than 3 (list 1 2 3 4 5)) 9)

(@template-origin fn-composition use-abstract-fn)

; (define (sum-larger-than n loi) 0)

(define (sum-larger-than n loi)
  (local [(define (>n? x) (> x n))]
    (foldr + 0 (filter >n? loi))))

(@problem 3)
(@htdf boxes)
(@signature Natural -> Image)
;; produce n+1 nested boxes, the smallest is quite small
(check-expect (boxes 0) empty-image)
(check-expect (boxes 1) (square 1 "outline" "black"))
(check-expect (boxes 2)
              (overlay (square 11 "outline" "black")
                       (square 1 "outline" "black")))
(check-expect (boxes 3)
              (overlay (square 21 "outline" "black")
                       (square 11 "outline" "black")
                       (square  1 "outline" "black")))

; (define (boxes n) empty-image) ; stub

(@template-origin fn-composition use-abstract-fn)

(define (boxes n)
  (local [(define (box n loi)
            (overlay (square n "outline" "black")
                     loi))]
    (foldr box empty-image (build-list n (lambda (n) (+ (* n 10) 1))))))


(@problem 4)
(@htdf pyramid)
(@signature Natural -> Image)
;; produce pyramid of circles n high, w n on bottom row
(check-expect (pyramid 0) empty-image)
(check-expect (pyramid 1) CIRCLE)
(check-expect (pyramid 2)
              (above CIRCLE
                     (beside CIRCLE CIRCLE)))
(check-expect (pyramid 3)
              (above CIRCLE
                     (beside CIRCLE CIRCLE)
                     (beside CIRCLE CIRCLE CIRCLE)))

(@template-origin fn-composition use-abstract-fn Natural)

;;
;; this function has to make a stack of n rows, and each row has
;; to have n elements.  So it will have two compositions of foldr
;; and build-list.
;;

; (define (pyramid n) empty-image) ; stub

(define (pyramid n)
  (local [(define (make-row n)
            (cond [(zero? n) empty-image]
                  [else
                   (beside CIRCLE    
                           (make-row (sub1 n)))]))]
    (foldr above empty-image (build-list (+ n 1) (lambda (n) (make-row n))))))