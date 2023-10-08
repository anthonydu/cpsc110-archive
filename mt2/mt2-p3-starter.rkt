;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mt2-p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment exams/2022w1-mt2/mt2-p3)

(@cwl antdu)   ;fill in your CWL here (same as for problem sets)

(@problem 1) ;do not edit or delete this line
(@problem 2) ;do not edit or delete this line
(@problem 3) ;do not edit or delete this line

#|

YOU ARE NOT PERMITTED TO USE THE STEPPER AT ANY POINT IN THIS EXAM.

The description of this problem is entirely contained in the mt2-p3-figure.pdf
file.  Read that file for the problem description.  ALL of your answers go in
this file.

NOTE: This problem will be autograded, and ALL OF THE FOLLOWING ARE ESSENTIAL
      IN YOUR SOLUTION.  Failure to follow these requirements may result in
      receiving zero marks for this problem.

 - This file includes special check-expects at the bottom that will check
   whether your answer is well formed when you run the file. If one or more
   answers are not well formed the test will fail with a message that
   describes what needs to be fixed.
 - Run the file EVERY time you edit an answer.
 - Note that initially the tests WILL FAIL for the starter, because they
   want you to edit your answers in before the tests pass.
 - Your submission will be graded if there are failing tests.  A failing test
   just means that specific answer is not well formed.
 - BUT your submission will not be graded if running it produces red errors.

|#

;;
;; For part 1 you MUST DELETE ALL BUT ONE OF THE STRINGS in the following list.
;; The remaining string is your answer to the question.
;;
(define P3-PART-1
  (list "C"))

;;
;; For part 2 you MUST DELETE ALL BUT ONE OF THE STRINGS in the following list.
;; The remaining string is your answer to the question.
;;
(define P3-PART-2
  (list "B"))

;;
;; For part 3 you MUST DELETE ALL BUT ONE OF THE STRINGS in the following list.
;; The remaining string is your answer to the question.
;;
(define P3-PART-3
  (list "D"))

;;
;; For part 4 you MUST DELETE ALL BUT ONE OF THE INTEGERS in the following list.
;; The remaining integer is your answer to the question.
;;
(define P3-PART-4
  (list 6)) 

;;
;; For part 5 you MUST DELETE ALL BUT ONE OF THE INTEGERS in the following list.
;; The remaining integer is your answer to the question.
;;
(define P3-PART-5
  (list 3))




;;============================================================================
;;============================================================================
;;============================================================================
;; You should ignore this code.  Do not read or edit below here.
;;
;; This code is here so that running the file will verify that your answer is
;; well formed.

(define (mcq-checker name choices)
  (local [(define (check x)
            (cond [(not (and (list? x) (= (length x) 1)))
                   (error name "Must be a list of one element.")]
                  [(not (member (car x) choices))
                   (error name
                          (format
                           "Single element of the list ~a must be one of ~S"
                           name choices))]
                  [else true]))]
    check))
          

(check-satisfied P3-PART-1 (mcq-checker 'P3-PART-1 (list "A" "B" "C" "D")))
(check-satisfied P3-PART-2 (mcq-checker 'P3-PART-2 (list "A" "B" "C" "D" "E")))
(check-satisfied P3-PART-3 (mcq-checker 'P3-PART-3 (list "A" "B" "C" "D")))
(check-satisfied P3-PART-4 (mcq-checker 'P3-PART-4 (list 3 4 5 6 7 8)))
(check-satisfied P3-PART-5 (mcq-checker 'P3-PART-5 (list 1 2 3 4 5)))
