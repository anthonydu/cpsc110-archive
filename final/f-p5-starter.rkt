;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname f-p5-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.

(require spd/tags)

(@assignment exams/2022w1-f/f-p5)

(@cwl antdu)   ;fill in your CWL here (same as for problem sets)

(@problem 1) ;do not edit or delete this line
(@problem 2) ;do not edit or delete this line
(@problem 3) ;do not edit or delete this line
(@problem 4) ;do not edit or delete this line
(@problem 5) ;do not edit or delete this line

#|
--------------------------------------------------------------------------------
In Bizz/Buzz, the goal is count upwards from 1, which seems easy. Except that 
everytime you get to a multiple of 3 you say Bizz instead of the number, and
at multiples of 7 you say Buzz.  At 21 you would say Bizz Buzz.

Design a function that consumes a stop number and produces a list of strings
from "1" to the stopping number (inclusive), and that follows the rules of 
BizzBuzz.  For example:

(bizz-buzz 22) would produce

(list "1" "2" "bizz" "4" "5" "bizz" "buzz" "8" "bizz" "10"
      "11" "bizz" "13" "buzz" "bizz" "16" "17" "bizz" "19" "20"
      "bizz-buzz" "22")

There are several good ways to template this function. Two of the best are:

 - using build-list (and optionally map). For the build-list approach just
   a reminder about the function remainder.  You can go into the Dr. Racket
   interaction pane and play with remainder. Try these examples:
       (remainder 4 3)
       (remainder 5 3)
       (remainder 6 3)
       (remainder 7 3)

 - using accumulators. For the accumulator based approach you will want to
   note the following:

     1  2  3  4  5  6  7  8  9 ...
     1  2  3  1  2  3  1  2  3 ...


In both approaches you will find the number->string primitive is useful.

Be sure your solution includes an @template-origin tag that indicates the
approach you take.

NOTE: This problem will be autograded. **ALL OF THE FOLLOWING ARE ESSENTIAL**
      in your solution.  Failure to follow these requirements may result in
      receiving zero marks for this problem.

 - the function you design must be called bizz-buzz
 - it must follow the signature and purpose described above
 - you must include correct @htdf, @signature and @template-origin tags
 - you must not comment out any of the tags
 - your submission must pass the Check Syntax button
 - you must follow all applicable design rules

|#

(@htdf bizz-buzz)
(@signature Natural -> (listof String))
;; produces a list of strings with multiples of 3 and 7 replaced by bizz buzz
(check-expect (bizz-buzz 21)
              (list "1" "2" "bizz" "4" "5" "bizz" "buzz" "8" "bizz" "10"
                    "11" "bizz" "13" "buzz" "bizz" "16" "17" "bizz" "19" "20"
                    "bizz-buzz"))
(check-expect (bizz-buzz 22)
              (list "1" "2" "bizz" "4" "5" "bizz" "buzz" "8" "bizz" "10"
                    "11" "bizz" "13" "buzz" "bizz" "16" "17" "bizz" "19" "20"
                    "bizz-buzz" "22"))
(check-expect (bizz-buzz 42)
              (list
               "1"
               "2"
               "bizz"
               "4"
               "5"
               "bizz"
               "buzz"
               "8"
               "bizz"
               "10"
               "11"
               "bizz"
               "13"
               "buzz"
               "bizz"
               "16"
               "17"
               "bizz"
               "19"
               "20"
               "bizz-buzz"
               "22"
               "23"
               "bizz"
               "25"
               "26"
               "bizz"
               "buzz"
               "29"
               "bizz"
               "31"
               "32"
               "bizz"
               "34"
               "buzz"
               "bizz"
               "37"
               "38"
               "bizz"
               "40"
               "41"
               "bizz-buzz"))
(check-expect (bizz-buzz 3)
              (list "1" "2" "bizz"))
(check-expect (bizz-buzz 7)
              (list
               "1"
               "2"
               "bizz"
               "4"
               "5"
               "bizz"
               "buzz"))
(check-expect (bizz-buzz 1)
              (list "1"))
(check-expect (bizz-buzz 0)
              empty)

(@template-origin Natural accumulator)

(define (bizz-buzz n)
  (local [(define (bizz-buzz n rsf)
            (cond [(zero? n) rsf]
                  [else
                   (cond [(and (zero? (remainder n 3))
                               (zero? (remainder n 7)))
                          (bizz-buzz (sub1 n) (cons "bizz-buzz" rsf))]
                         [(zero? (remainder n 3))
                          (bizz-buzz (sub1 n) (cons "bizz" rsf))]
                         [(zero? (remainder n 7))
                          (bizz-buzz (sub1 n) (cons "buzz" rsf))]
                         [else
                          (bizz-buzz (sub1 n)
                                     (cons (number->string n) rsf))])]))]
    (bizz-buzz n empty)))
