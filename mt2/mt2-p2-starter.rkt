;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mt2-p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment exams/2022w1-mt2/mt2-p2)

(@cwl antdu)   ;fill in your CWL here (same as for problem sets)

(@problem 1) ;do not edit or delete this line
(@problem 2) ;do not edit or delete this line

#|

YOU ARE NOT PERMITTED TO USE THE STEPPER AT ANY POINT IN THIS EXAM.

Problems 1 and 2 of this midterm both use the following data definitions.

After reviewing the data definition you can find the problem description below.

|#

(@htdd Professor)
(define-struct prof (nam cit awd stu))
;; Professor is (make-prof String Natural (listof String) (listof Professor))
;; interp. an academic tree for professors.
;;         each professor has a (nam)e, number of (cit)ations, a list of awards
;;         they have received and list of current professors they supervised

(define P1 (make-prof "Bostock"  6720  empty empty))
(define P2 (make-prof "Borkin"   3073  empty empty))
(define P3 (make-prof "Meyer"    3880  empty empty))
(define P4 (make-prof "Heer"     27847
                      (list "Sloan" "VGTC" "ACM" "VisAcademy")
                      (list P1)))
(define P5 (make-prof "Hullman"  3253
                      (list "Google" "Microsoft")
                      empty))
(define P6 (make-prof "Munzner"  12660
                      (list "VGTC" "VisAcademy")
                      (list P2 P3)))

(define P7 (make-prof "Ng"       12720 (list "Sloan" "Hellman") empty))

(define P8 (make-prof "Agrawala" 21820 (list "Sloan" "SIGGRAPH") (list P4 P5)))
(define P9 (make-prof "Hanrahan" 50211
                      (list "Turing" "Oscar" "VisAcademy")
                      (list P6 P7 P8)))

(@template-origin Professor (listof String) (listof Professor) encapsulated)

(define (fn-for-prof p)
  (local [(define (fn-for-prof p)
            (... (prof-nam p)
                 (prof-cit p)
                 (fn-for-los (prof-awd p))
                 (fn-for-lop (prof-stu p))))

          (define (fn-for-los los)
            (cond [(empty? los) (...)]
                  [else
                   (... (first los)
                        (fn-for-los (rest los)))]))

          (define (fn-for-lop lop)
            (cond [(empty? lop) (...)]
                  [else
                   (... (fn-for-prof (first lop))
                        (fn-for-lop (rest lop)))]))]

    (fn-for-prof p)))

#|

Complete the design of the following abstract fold function for Professor.
Note that we have already given you the actual function definition and the
template origin tag. You must complete the design with a signature, purpose,
and the two following check-expects:

  - uses the fold function to produce a copy of P9
  - uses the fold function to count the total number of profs in P9,
    which is 9

Be VERY CAREFUL WRITING THE SIGNATURE. You must have a SINGLE uncommented
@signature tag. The autograder is very picky about these problems. If you
skip the type of one parameter then the types of all following parameters
will probably be marked wrong. On the other hand an incorrect type typically
does not affect anything after it. So work very carefully to first setup the
number of parameters the function has, and be sure your final answer has
types for that many parameters. HINT, there are 6.

This problem will be autograded.  NOTE that all of the following are required.
Violating one or more will cause your solution to receive 0 marks.

 - You MUST NOT edit the provided fold-prof function definition or
   the template origin tag.

 - You must have a single uncommented @signature tag.  Because it is an
   exam you may leave commented scratch work in the submission.

 - Your submission must pass the Check Syntax button.



|#

(@htdf fold-prof)
(@signature (String Natural Y Z -> X)
            (String Y -> Y)
            (X Z -> Z)
            Y Z Professor -> X)
;; abstract fold function for Professor
(check-expect (fold-prof make-prof cons cons empty empty P9) P9)
(check-expect
 (local [(define (c1 nam cit y z) (add1 z))]
   (fold-prof c1 cons + empty 0 P9)) 9)

(@template-origin Professor  (listof String) (listof Professor) encapsulated)

(define (fold-prof c1 c2 c3 b1 b2 p)
  (local [(define (fn-for-prof p)
            (c1 (prof-nam p)
                (prof-cit p)
                (fn-for-los (prof-awd p))
                (fn-for-lop (prof-stu p))))

          (define (fn-for-los los)
            (cond [(empty? los) b1]
                  [else
                   (c2 (first los)
                       (fn-for-los (rest los)))]))

          (define (fn-for-lop lop)
            (cond [(empty? lop) b2]
                  [else
                   (c3 (fn-for-prof (first lop))
                       (fn-for-lop (rest lop)))]))]

    (fn-for-prof p)))
