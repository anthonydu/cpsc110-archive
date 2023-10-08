;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m01-htdf-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require spd/tags)

(@assignment lectures/m01-htdf)

(@cwl antdu) ;replace ??? by your CWL


;; *****************************************************************************
;; NOTE:
;;
;; Your AUTOGRADER RESULTS will be found at the following address:
;;
;;    https://cs110.students.cs.ubc.ca/handback/12345678/
;;
;; Where you must replace 12345678 with your UBC student number.  You will need
;; to login with your CWL and CWL password.
;;
;;  For lecture starters you can get some feedback from the autograder during
;;  lecture, provided you can submit a file that passes the Check Syntax button
;;  at the top right of the Dr Racket window.
;;
;;  Especially at first it can be hard to get all your ( ) and "  " properly
;;  balanced, and so you might not be able to pass the Check Syntax.  If that
;;  happens then first start by getting the gist of the problem right.  You
;;  can (and should) go back after lecture to work the problem until you
;;  get autograder feedback saying it is 100% correct.
;;
;;  But for problem sets, labs, and exams you should not submit your file if
;;  Check Syntax produces an error.  Fix the error first, then submit.
;; *****************************************************************************


(@problem 1)
;;
;; Design a function, called topple, that takes an image and rotates it 
;; by 90 degrees.
;;

(@htdf topple)
(@signature Image -> Image)
;; produce image rotated by 90 degrees counterclockwise

(define rectin (rectangle 40 20 "outline" "black"))
(define rectout (rectangle 20 40 "outline" "black"))
(define trianglein (triangle/sss 40 60 80 "solid" "seagreen"))
(define triangleout (rotate 90 trianglein))

(check-expect (topple rectin) rectout)
(check-expect (topple trianglein) triangleout)

; (define (topple i) empty-image) ; stub

(@template-origin Image)
(@template
 (define (topple i)
   (... i)
 )
)

(define (topple i)
  (rotate 90 i)
)



(@problem 2)
;;
;; Design a function that consumes the name of something and produces a
;; "checkbox line" image that allows someone to check off that item.  For 
;; example (checkbox-line "apples") would produce an image with a small
;; check box next to the word apples.
;;

(@htdf checkbox-line)
(@signature String -> Image)
;; produce a square box next to some text

(define squarebox (square 20 "outline" "black"))

(check-expect (checkbox-line "apples")
              (beside squarebox (text "apples" 24 "black")))
(check-expect (checkbox-line "")
              (beside squarebox (text "" 24 "black")))

; (define (checkbox-line str) empty-image) ; stub


(@template-origin String)
(@template (define (checkbox-line str) (... str)))

(define (checkbox-line str)
  (beside (square 20 "outline" "black") (text str 24 "black"))
)



(@problem 3)
;;
;; Design a function, that consumes an image and determines whether it is tall.
;;

(@htdf tall?)
(@signature Image -> Boolean)
;; determines whether an image is tall (height > width)

(check-expect (tall? (rectangle 40 20 "outline" "black")) false)
(check-expect (tall? (rectangle 20 40 "outline" "black")) true)
(check-expect (tall? (rectangle 30 30 "outline" "black")) false)

; (define (tall? img) false) ; stub

(@template-origin Image)
(@template (define (tall? img) (... img)))

(define (tall? img)
  (> (image-height img) (image-width img))
)


(@problem 4)
;;
;; Design a function, called image>, that takes two images and determines 
;; whether the first is larger than the second.
;;

(@htdf image>)
(@signature Image Image -> Boolean)
;; determines whether the area of the first image is larger than the second

(define rectbig (rectangle 30 30 "outline" "black"))
(define rectsmall (rectangle 20 40 "outline" "black"))
(define rectalsobig (circle 15 "outline" "black"))

(check-expect (image> rectbig rectsmall) true)
(check-expect (image> rectsmall rectbig) false)
(check-expect (image> rectbig rectalsobig) false)

; (define (image> i1 i2) false) ; stub

(@template-origin Image)
(@template (define (image> i1 i2) (... i1 i2)))

(define (image> i1 i2)
  (> (* (image-height i1) (image-width i1))
     (* (image-height i2) (image-width i2)))
)





