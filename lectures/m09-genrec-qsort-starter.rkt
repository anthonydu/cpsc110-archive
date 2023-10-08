;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname m09-genrec-qsort-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(@assignment lectures/m09-genrec-qsort)

(@cwl antdu) ;replace ??? with your cwl

(@problem 1)
;;
;; We are now going to turn back to sorting

;;
;; Here's the idea:
;;  If we have to sort a list, we will first make the problem
;;  smaller by breaking it into two lists, sort them, and then
;;  put them back together. 
;;
;;  We will break it in two lists by taking the first element -- 
;;  which we call the PIVOT, and then filtering out two lists, one
;;  w/ elements less than the pivot and one w/ elements greater
;;  than the pivot. Once those lists are sorted, we can append
;;  the < list, a list consisting of just the pivot and the >
;;  list back back together to get the result.
;;
;;                   (list 6 8 1 9 3 7 2) 
;;                   /         |        \
;;                  /          |         \
;;        (list 1 3 2)         6        (list 8 9 7)
;;         /  |  \                         /    |    \
;;        /   |   \                       /     |     \
;;     empty  1 (list 3 2)          (list 7)    8    (list 9)
;;                /  |  \             / | \           /  |  \
;;               /   |   \           /  |  \         /   |   \
;;          (list 2) 3  empty     empty 7 empty    empty 9  empty
;;           /  |  \
;;        empty 2 empty  
;; This way of sorting is called QUICKSORT. It is a generative
;; recursion.

(@htdf qsort)
(@signature (listof Number) ->  (listof Number))
;; produce list of numbers sorted in ASCENDING order
;; CONSTRAINT: lon contains no duplicates
; data-driven tests
; base case          - 1
(check-expect (qsort empty) empty)
; fn-driven tests
; 1 long             - 2
(check-expect (qsort (list 3)) (list 3))
; all values < pivot - 3
(check-expect (qsort (list 6 1 4 2)) (list 1 2 4 6))
; all values > pivot - 4
(check-expect (qsort (list 6 8 9 7 10)) (list 6 7 8 9 10))
; a mix of values    - 5

(check-expect (qsort (list 3 1 2 5 6 8 4 7 9)) (list 1 2 3 4 5 6 7 8 9))
; same as 2 long g, 

#|
Three part termination argument.

Base case: 

Reduction step: 

Argument that repeated application of reduction step will eventually 
reach the base case: 
|#

(@template-origin genrec use-abstract-fn)

#;;template 
(define (genrec-fn lon)
  (cond [(trivial? lon) (trivial-answer lon)]
        [else (... lon
                   (genrec-fn (next-problem lon)))]))

(define (qsort lon)
  (cond [(empty? lon) empty]
        [else
         (local [(define (greater-pivot? n) (> n (first lon)))
                 (define (smaller-pivot? n) (< n (first lon)))]
           (append (qsort (filter smaller-pivot? lon))
                   (list (first lon))
                   (qsort (filter greater-pivot? lon))))]))








