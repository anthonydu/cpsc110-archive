;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m05-naturals-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require spd/tags)

(@assignment lectures/m05-naturals)

(@cwl antdu) ;replace ??? with your cwl

;; ------------------
;; Data Definitions:
(@htdd Natural)
;; Natural is one of:
;;  - 0
;;  - (add1 Natural)
;; interp. a natural number
(define N0 0)         ;0
(define N1 (add1 N0)) ;1
(define N2 (add1 N1)) ;2

(@dd-template-rules one-of          ;2 cases
                    atomic-distinct ;0
                    compound        ;(add1 Natural)
                    self-ref)       ;(sub1 n) is Natural

(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (... n                     ;template rules wouldn't normally put this
              ;                     ;here, but we will see that we end up coming
              ;                     ;back to add it
              (fn-for-natural (sub1 n)))]))


;; ------------------
;; Functions:


(@problem 1)
;; Design a function to make nested boxes

(@htdf boxes)
(@signature Natural -> Image)
;; produce n+1 nested boxes, the smallest is quite small
(check-expect (boxes 0) (square 1 "outline" "black"))
(check-expect (boxes 1)
              (overlay (square 11 "outline" "black")
                       (square 1 "outline" "black")))
(check-expect (boxes 2)
              (overlay (square 21 "outline" "black")
                       (square 11 "outline" "black")
                       (square  1 "outline" "black")))

; (define (boxes n) empty-image) ; stub

(@template-origin Natural)

(@template 
 (define (boxes n)
   (cond [(zero? n) (...)]
         [else
          (... n
               (boxes (sub1 n)))])))


(define (boxes n)
  (cond [(zero? n) (square 1 "outline" "black")]
        [else
         (underlay (square (+ (* n 10) 1) "outline" "black")
                   (boxes (sub1 n)))]))




(@problem 2)
;; Design a function to compute factorial.

(@htdf fact)
(@signature Natural -> Natural)
;; produce n*n-1*n-2...*1
(check-expect (fact 0) 1)
(check-expect (fact 1) 1)
(check-expect (fact 2) 2)
(check-expect (fact 3) 6)
(check-expect (fact 4) 24)
(check-expect (fact 5) 120)

; (define (fact n) 1)  ; stub

(@template-origin Natural)

(@template 
 (define (fact n)
   (cond [(zero? n) (...)]
         [else
          (... n 
               (fact (sub1 n)))])))

(define (fact n)
  (cond [(zero? n) 1]
        [else
         (* n 
            (fact (sub1 n)))]))



(@problem 3)
;; Design a function to make a list of the first n natural numbers
;; starting at 1.
;;
;; So (blist 4) should produce (cons 1 (cons 2 (cons 3 (cons 4 empty))))

(@htdf blist)
(@signature Natural -> ListOfNatural)
;; produce first n natural numbers; starting at 1
(check-expect (blist 0) empty)
(check-expect (blist 1) (list 1))
(check-expect (blist 2) (list 1 2))
(check-expect (blist 3) (list 1 2 3))

; (define (blist n) empty) ; stub

(@template-origin Natural)

(@template 
 (define (blist n)
   (cond [(zero? n) (...)]
         [else
          (... n 
               (blist (sub1 n)))])))

(define (blist n)
  (cond [(zero? n) empty]
        [else
         (append (blist (sub1 n))
                 (cons n empty))]))
