;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname m07-regions-search-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require spd/tags)
(@assignment lectures/m07-regions-search)

(@cwl antdu) ;replace ??? with your cwl
;;
;; Region and ListOfRegion data definitions provided.  
;;

(@htdd Region ListOfRegion)
(define-struct single (label weight color))
(define-struct group (color subs))
;; Region is one of:
;;  - (make-single String Natural Color)
;;  - (make-group Color ListOfRegion)
;; interp.
;;  an arbitrary-arity tree of regions
;;  single regions have label, weight and color
;;  groups have a color and a list of sub-regions
;;
;;  weight is a unitless number indicating how much weight
;;  the given single region contributes to whole tree

;; ListOfRegion is one of:
;;  - empty
;;  - (cons Region ListOfRegion)
;; interp. a list of regions

;; All the Ss and Gs are Regions
(define S1 (make-single "one" 20 "red"))
(define S2 (make-single "two" 40 "blue"))
(define S3 (make-single "three" 60 "orange"))
(define S4 (make-single "four" 30 "black"))
(define S5 (make-single "five" 50 "purple"))
(define S6 (make-single "six" 80 "yellow"))

(define G1 (make-group "red"  (list S1 S2 S3)))
(define G2 (make-group "blue" (list G1 S4)))
(define G3 (make-group "orange" (list S5 S6)))
(define G4 (make-group "black" (list G2 G3)))

(define LORE empty)
(define LOR123 (list S1 S2 S3))

(@problem 1)
#| Use local to encapsulate these templates. |#

(@template-origin Region ListOfRegion encapsulated)

(define (fn-for-region r)
  (local [(define (fn-for-region r)
            (cond [(single? r)
                   (... (single-label r)
                        (single-weight r)
                        (single-color r))]
                  [else
                   (... (group-color r)
                        (fn-for-lor (group-subs r)))]))

          (define (fn-for-lor lor)
            (cond [(empty? lor) (...)]
                  [else
                   (... (fn-for-region (first lor))
                        (fn-for-lor (rest lor)))]))]
    
    (fn-for-region r)))

(@problem 2)
#|
Design a function that consumes a string and a region
and looks for a region with the given label.  If there is one
the function should produce the first one it finds.  If there is
not one it should signal failure by producing false.

Use the encapsulated templates from Problem 1 above.
|#

(@htdf find-region)
(@signature String Region -> Region or false)
;; produce a region with the given label, false if not found
; test on simple single found
(check-expect (find-region "one" S1) S1)
; test on simple single not found
(check-expect (find-region "one" S2) false)
; test that is 2 deep found
(check-expect (find-region "three" G4) S3)
; test that is 2 deep not found
(check-expect (find-region "ten" G4) false)

; (define (find-region l r) false) ; stub

(@template-origin Region ListOfRegion encapsulated try-catch)

(define (find-region l r)
  (local [(define (find-region--region l r)
            (cond [(single? r)
                   (if (string=? l (single-label r))
                       r
                       false)]
                  [else
                   (find-region--lor l (group-subs r))]))

          

          (define (find-region--lor l lor)
            (cond [(empty? lor) false]
                  [else
                   (local [(define try (find-region--region l (first lor)))]
                     ; this is try-catch template
                     (if (not (false? try)) try ; must be the exact same thing
                         (find-region--lor l (rest lor))))]))]
    
    (find-region--region l r)))

