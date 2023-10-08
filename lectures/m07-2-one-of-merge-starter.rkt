;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname m07-2-one-of-merge-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment lectures/m07-2-one-of-merge)

(@cwl antdu) ;replace ??? with your cwl


(@problem 1)
#|
Design a function that consumes two lists of numbers. Each list
is already sorted in increasing order. The function should produce
the merged list sorted in increasing order.
Show the cross product of type comments table, the simplification,
and the correspondence between table cells and cond cases.

For example:

  (merge (list 2 3 5) (list 1 4 6)) --> (list 1 2 3 4 5 6) 
  
As a reminder, here is a data definition for a list of numbers To 
save space later we are calling it LON instead of ListOfNumber
|#

;; Data Definitions:

(@htdd LON)
;; LON is one of:
;;  - empty
;;  - (cons Number LON)
;; interp. a list of numbers
(define LON1 empty)
(define LONA (list 2 3 5))
(define LONB (list 1 4 6))

;; Function:

(@htdf merge)
(@signature LON LON -> LON)
;; produce the merged list sorted in increasing order
(check-expect (merge empty empty) empty)
(check-expect (merge empty (list 1 4 6)) (list 1 4 6))
(check-expect (merge (list 2 3 5) empty) (list 2 3 5))
(check-expect (merge (list 2 3 5) (list 1 4 6)) (list 1 2 3 4 5 6))
(check-expect (merge (list 2 3 5 7 8) (list 1 4 6)) (list 1 2 3 4 5 6 7 8))
(check-expect (merge (list 2 3 5) (list 1 4 6 7 8)) (list 1 2 3 4 5 6 7 8))
(check-expect (merge (list 1 2 4) (list 3 5 6)) (list 1 2 3 4 5 6))

; (define (merge lon1 lon2) empty) ; stub

(@template-origin 2-one-of)

(define (merge lon1 lon2)
  (cond [(empty? lon1) lon2]
        [(empty? lon2) lon1]
        [else
         (if (< (first lon1) (first lon2))
             (cons (first lon1) (merge (rest lon1) lon2))
             (cons (first lon2) (merge lon1 (rest lon2))))]))

; This is incorrect: 
#;(define (merge lon1 lon2)
    (cond [(empty? lon1) lon2]
          [(empty? lon2) lon1]
          [else (append (if (< (first lon1) (first lon2))
                            (list (first lon1) (first lon2))
                            (list (first lon2) (first lon1)))
                        (merge (rest lon1) (rest lon2)))]))
; It fails (check-expect (merge (list 1 2 4) (list 3 5 6)) (list 1 2 3 4 5 6))
