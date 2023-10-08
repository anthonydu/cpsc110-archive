;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mt1-p1-starter-2022) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/image)

(@assignment exams/2022w1-mt1/mt1-p1)

(@cwl antdu)   ;fill in your CWL here (same as for problem sets)

(@problem 1) ;do not delete or edit this line

#|

Consider the following PARTIAL data definition. You will note that it only
includes a structure definition and the type comment.  The, interpretation and
data examples are not important for this problem -- do NOT add them.

For the type comment, you must complete the @dd-template-rules tag and template
function definition.  You are permitted to simplify cond questions, or leave 
them unsimplified. But you must simplify correctly, and necessary guards must
be in place.

Note that this problem will be autograded. Failure to adhere to the following
may result in a score as low as 0.

 - Your @dd-template-rules and cond question-answer pairs MUST BE IN THE SAME
   ORDER as the type comment.

 - You must not comment out or edit the @htdd tag.

 - You must not comment out the @dd-template-rules tag

 - You must not comment out the fn-for-foo function definition.

 - Your submission must pass the Check Syntax button without any errors.

 - You should only edit the @dd-template-rules and template function definition.
   You must not edit any other parts of the file.

|#

(@htdd Foo)
(define-struct bar (x y)) 
;; Foo is one of:
;; - Number
;; - "red"
;; - "green"
;; - (make-bar Number Foo)

(@dd-template-rules one-of
                    atomic-non-distinct
                    atomic-distinct
                    atomic-distinct
                    compound)

(define (fn-for-foo f)
  (cond [(number? f) (... f)]
        [(string=? f "red") (...)]
        [(string=? f "green") (...)]
        [else (... (bar-x f) (bar-y f))])
  )
