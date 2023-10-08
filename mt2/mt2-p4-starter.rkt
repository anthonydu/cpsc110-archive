;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mt2-p4-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(@assignment exams/2022w1-mt2/mt2-p4)

(@cwl antdu)   ;fill in your CWL here (same as for problem sets)


(@problem 1) ;do not edit or delete this line
(@problem 2) ;do not edit or delete this line
(@problem 3) ;do not edit or delete this line
(@problem 4) ;do not edit or delete this line


#|

YOU ARE NOT PERMITTED TO USE THE STEPPER AT ANY POINT IN THIS EXAM.

In this problem you must complete the design of a function by writing the
template origin tag and the function definition.  

This problem will be autograded.  NOTE that all of the following are required.
Violating one or more will cause your solution to receive 0 marks.

  - The file MUST NOT have any errors when the Check Syntax button is pressed.
    Press Check Syntax and Run often, and correct any errors early.

  - The function definition MUST call one or more built-in abstract functions.

  - The function definition MUST NOT be recursive.

  - The function definition MUST NOT use any part of the recursive Natural
    template or the (listof X) template. For instance,

      - it must not include (cond [(empty? ... anywhere
      - it must not include (cond [(zero? ... anywhere
      - it must not include (first loc) anywhere

  - You MUST NOT change or comment out any check-expects, but you are free
    to add new ones.

  - For maximum credit, the function definition should use exactly 3 
    different built in abstract functions. Submissions that use 2 will 
    receive no more than 70%. Submissions that use only 1 will receive at 
    most 30%. Submissions that do not use any built-in abstract functions 
    or that use four or more built-in abstract functions will receive 0 marks.

  - The result of the function must directly be the result of one of the
    built-in abstract functions. So, for example, the following would not
    be valid function body:

       (define (foo x)
         (empty? (filter ...)))

|#

(@htdf colorbar)
(@signature Natural Natural (listof String) -> Image)
;; produce bar of squares of size s, of colors w/ name length < l
(check-expect (colorbar 100 5  (list "red" "blue" "yellow" "grey"))
              (beside (square 100 "solid" "red")
                      (square 100 "solid" "blue")
                      (square 100 "solid" "grey")))
(check-expect (colorbar 80 6  (list "orange" "turquoise" "grey" "brown"))
              (beside (square 80 "solid" "grey")
                      (square 80 "solid" "brown")))

(@template-origin use-abstract-fn fn-composition)

(define (colorbar s l loc)
  (local [(define (short-color? c) (< (string-length c) l))
          (define (draw-square c) (square s "solid" c))]
    (foldr beside empty-image (map draw-square (filter short-color? loc)))))


