;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname m10-find-tree-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(@assignment lectures/m10-find-tree)
(@cwl antdu) ;replace ??? with your cwl


(@htdd Tree) 

(define-struct node (name subs))
;; Tree is (make-node String (listof Tree))
;; interp. a bare bones arbitrary arity tree, each node has a name and subs
;;
;; NOTE: The template has a little extra in it to help with the functions
;;       we will be writing.

(define (fn-for-tree t)  
  (local [(define (fn-for-t t)
            (local [(define name (node-name t))  ;unpack the fields
                    (define subs (node-subs t))] ;for convenience
              
              (... name (fn-for-lot subs))))
          
          (define (fn-for-lot lot)
            (cond [(empty? lot) (...)]
                  [else
                   (... (fn-for-t (first lot))
                        (fn-for-lot (rest lot)))]))]
    
    (fn-for-t t)))

(define L1 (make-node "L1" empty))
(define L2 (make-node "L2" empty))
(define L3 (make-node "L3" empty))

(define M1 (make-node "M1" (list L1)))
(define M2 (make-node "M2" (list L2 L3)))

(define TOP (make-node "TOP" (list M1 M2)))


(@problem 1)
;;
;; Complete the design of the following function using the templates
;; provided.  Use ordinary recursion as we have used all term.
;;
;; NOTE: you must not rename any locally defined functions from the template.
;;
;; This is not a trick problem, it is an ordinary backtracking search.

(@htdf find-tree)

(@signature Tree String -> Tree or false)
;; produce tree w/ given name (or fail)
(check-expect (find-tree L1 "L1") L1)
(check-expect (find-tree L1 "L2") false)
(check-expect (find-tree L2 "L2") L2)
(check-expect (find-tree M1 "L1") L1)
(check-expect (find-tree TOP "L3") L3)

(@template-origin encapsulated Tree (listof Tree) try-catch)

(define (find-tree t n)  
  (local [(define (fn-for-t t)
            (local [(define name (node-name t))
                    (define subs (node-subs t))]
              (if (string=? n name)
                  t
                  (fn-for-lot subs))))
          
          (define (fn-for-lot lot)
            (cond [(empty? lot) false]
                  [else
                   (if (not (false? (fn-for-t (first lot))))
                       (fn-for-t (first lot))
                       (fn-for-lot (rest lot)))]))]
    
    (fn-for-t t)))



(@problem 2)
;;
;; Define a new version of the function which is tail recursive.  Below
;; we have set you up with everything you had above, for a new function
;; that is named find-tree/tr.
;;
;; Proceed by
;;   - copying your solution from the problem above to down below
;;   - rename the copy to find-tree/tr
;;   - then convert the code to be tail recursive
;;
;; NOTE: you must not rename any locally defined functions from the template


(@htdf find-tree/tr)

(@signature Tree String -> Tree or false)
;; produce tree w/ given name (or fail)
(check-expect (find-tree/tr L1 "L1") L1)
(check-expect (find-tree/tr L1 "L2") false)
(check-expect (find-tree/tr L2 "L2") L2)
(check-expect (find-tree/tr M1 "L1") L1)
(check-expect (find-tree/tr TOP "L3") L3)

(@template-origin encapsulated Tree (listof Tree) try-catch)

(define (find-tree/tr t n)  
  (local [(define (fn-for-t t t-wl)
            (local [(define name (node-name t))
                    (define subs (node-subs t))]
              (if (string=? n name)
                  t
                  (fn-for-lot (append subs t-wl)))))
          
          (define (fn-for-lot t-wl)
            (cond [(empty? t-wl) false]
                  [else
                   (fn-for-t (first t-wl) (rest t-wl))]))]
    
    (fn-for-t t empty)))
