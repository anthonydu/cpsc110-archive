;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mt2-p5-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment exams/2022w1-mt2/mt2-p5)

(@cwl antdu)   ;fill in your CWL here (same as for problem sets)


(@problem 1) ;do not edit or delete this line
(@problem 2) ;do not edit or delete this line
(@problem 3) ;do not edit or delete this line
(@problem 4) ;do not edit or delete this line
(@problem 5) ;do not edit or delete this line

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
      - it must not include (first this) anywhere
      - it must not include (first that) anywhere

  - You MUST NOT change or comment out any check-expects, but you are free
    to add new ones.

  - You are NOT PERMITTED to use member, member?, memq, memq or memv.

  - You should use built-in abstract functions appropriately to write code
    that is clear and easy to understand.


|#

(@htdf in-this-not-that)
(@signature Number (listof Number) (listof Number) -> Boolean)
;; produce true if a given number is in the first list, but not in the second
(check-expect (in-this-not-that 3 (list 1 3 5) (list 2 4 6)) true)
(check-expect (in-this-not-that 3 (list 1 3 5) (list 3))     false)
(check-expect (in-this-not-that 6 (list 1 5)   (list 2 4 6)) false)

(@template-origin use-abstract-fn)

(define (in-this-not-that x this that)
  (local [(define (nx=? n) (= n x))
          (define (nx-not=? n) (not (nx=? n)))]
    (and (ormap nx=? this) (andmap nx-not=? that))))

