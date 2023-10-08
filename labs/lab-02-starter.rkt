;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-02-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(@assignment labs/lab-02)

;; If you are:
;;   - A 110 or 107 student replace the first set of '???'s with your cwl.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   - A UBC Extended Learning student, replace the first set of ??? with
;;     the your email address as confirmed in the email you received from
;;     extended learning.  The handin password is also in that email.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   
(@cwl antdu ???)

;; HtDF Lab, Problem 1

;; PROBLEM:
;;
;; Design a function called square? that consumes an image and determines 
;; whether the image's height is the same as the image's width.
(@problem 1)
(@htdf square?) ;!!!UNCOMMENT this line when you start on this function
(@signature Image -> Boolean)
;; Determines whether an image has the same width as its height

(check-expect (square? (circle 10 "solid" "red")) true) ; a non rectangle square
(check-expect (square? (rectangle 5 10 "solid" "red")) false) ; width > height
(check-expect (square? (rectangle 10 5 "solid" "red")) false) ; width < height

; (define (square? empty-image) false) ; stub

(@template-origin Image)
(@template   
 (define (square? img)
   (... img)))
  
(define (square? img)
  (= (image-width img) (image-height img)))

; explination for TA check-off in addition to existing comments:
; output whether the image has the same width as height









;; HtDF Lab, Problem 2

;; PROBLEM:
;; 
;; Design a function named underline that consumes an image 
;; and produces that image underlined by a black line of the same width. 
;; 
;; 
;; For example, 
;; 
;;   (underline (circle 20 "solid" "green"))
;; 
;; should produce
;;
;;   (above (circle 20 "solid" "green")
;;          (rectangle 40 2 "solid" "black"))
(@problem 2)
(@htdf underline) ;!!!UNCOMMENT this line when you start on this function
(@signature Image -> Image)
;; Produces a image underlined by a black line of the same width

(check-expect (underline (circle 20 "solid" "green"))
              (above (circle 20 "solid" "green")
                     (rectangle 40 2 "solid" "black")))
(check-expect (underline (rectangle 20 20 "solid" "green"))
              (above (rectangle 20 20 "solid" "green")
                     (rectangle 20 2 "solid" "black")))

; (define (underline empty-image) empty-image) ; stub

(@template-origin Image)
(@template   
 (define (underline img)
   (... img)))
  
(define (underline img)
  (above img (rectangle (image-width img) 2 "solid" "black")))

; explination for TA check-off in addition to existing comments:
; put the input image above a black solid underline with the same width








;; HtDF Lab, Problem 3

;; PROBLEM:
;; 
;; A (much too) simple scheme for pluralizing words in English is to add an 
;; s at the end unless the word already ends in s.
;; 
;; Design a function that consumes a string, and adds s to the end unless 
;; the string already ends in s.
(@problem 3)
(@htdf pluralize) ;!!!UNCOMMENT this line when you start on this function
(@signature String -> String)
;; Adds s to the end unless the string already ends in s

(check-expect (pluralize "") "")              ; empty string
(check-expect (pluralize "apple") "apples")   ; word without s
(check-expect (pluralize "cactus") "cactus")  ; word with s

; (define (pluralize "") "") ; stub

(@template-origin String)
(@template   
 (define (pluralize str)
   (... str)))
  
(define (pluralize str)
  (cond [(= (string-length str) 0) ""]
        [(string=? (substring str (- (string-length str) 1)) "s") str]
        [else (string-append str "s")]))

; explination for TA check-off in addition to existing comments:
; if string is empty, return an empty string
; if the last character of the string is s, output the input string
; else output the input string with an "s" appended











;; HtDF Lab, Problem 4

;; PROBLEM:
;; 
;; Design a function called nth-char-equal? that consumes two strings 
;; and a natural and produces true if the strings both have length greater 
;; than n and have the same character at position n.
;; 
;; 
;; Note, the signature for such a function is:
;; 
;; (@signature String String Natural -> Boolean)
;; 
;; 
;; The tag and template for such a function are:
;; 
;; (@template-origin String)
;; 
;; (@template (define (nth-char-equal? s1 s2 n)
;;              (... s1 s2 n)))
(@problem 4)
(@htdf nth-char-equal?) ;!!!UNCOMMENT this line when you start on this function
(@signature String String Natural -> Boolean)
;; Produces true if the strings both have length greater than n
;; and have the same character at position n

(check-expect (nth-char-equal? "Racket" "sucks" 5) false) ; one index = length
(check-expect (nth-char-equal? "Racket" "sucks" 6) false) ; index > length
(check-expect (nth-char-equal? "hello" "world" 0) false)  ; index 0 & false
(check-expect (nth-char-equal? "Never gonna"
                               "give you up" 11) false)   ; both index = length
(check-expect (nth-char-equal? "Never gonna"
                               "Let you down" 1) true)    ; true

; (define (nth-char-equal? "" "" 0) false) ; stub

(@template-origin String)
(@template (define (nth-char-equal? s1 s2 n)
             (... s1 s2 n)))
  
(define (nth-char-equal? s1 s2 n)
             (if (and (> (string-length s1) n) (> (string-length s2) n))
                 (string=? (substring s1 n (+ n 1))
                           (substring s2 n (+ n 1))) false))

; explination for TA check-off in addition to existing comments:
; if the length of both string are greater than n,
; output whether the character at the nth location is the same, else false










