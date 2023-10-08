;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mt2-p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment exams/2022w1-mt2/mt2-p1)

(@cwl antdu)   ;fill in your CWL here (same as for problem sets)

(@problem 1) ;do not edit or delete this line


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

(@template-origin Professor (listof String) (listof Professor)  encapsulated)

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
Complete the design of the following function.

NOTE: This problem will be autograded, and ALL OF THE FOLLOWING ARE ESSENTIAL
      IN YOUR SOLUTION.  Failure to follow these requirements may result in
      receiving zero marks for this problem.

 - the function you design must be called profs-with-award
 - it must follow the signature and purpose below
 - you must not comment out any @ metadata tags
 - your submission must pass the Check Syntax button
 - you must use the encapsulated template below in your solution
 - you must NOT RENAME ANY OF THE LOCAL FUNCTIONS inside the template
 - you must follow all design rules related to templates

|#



(@htdf profs-with-award)
(@signature String Professor -> (listof String))
;; produce the names of all profs in the tree who have the given award
(check-expect (profs-with-award "" P1) empty)
(check-expect (profs-with-award "" P5) empty)
(check-expect (profs-with-award "" P9) empty)
(check-expect (profs-with-award "Sloan" P1) empty)
(check-expect (profs-with-award "Google" P5) (list "Hullman"))
(check-expect (profs-with-award "VisAcademy" P9)
              (list "Hanrahan" "Munzner" "Heer"))
(check-expect (profs-with-award "Sloan" P9) (list "Ng" "Agrawala" "Heer"))

; (define (profs-with-award str p) empty) ;stub

(@template-origin Professor (listof String)  (listof Professor) encapsulated)

(define (profs-with-award s p)
  (local [(define (fn-for-prof p)
            (if (fn-for-los (prof-awd p))
                (append (list (prof-nam p)) (fn-for-lop (prof-stu p)))
                (fn-for-lop (prof-stu p))))

          (define (fn-for-los los)
            (cond [(empty? los) false]
                  [else
                   (or (string=? (first los) s)
                       (fn-for-los (rest los)))]))

          (define (fn-for-lop lop)
            (cond [(empty? lop) empty]
                  [else
                   (append (fn-for-prof (first lop))
                           (fn-for-lop (rest lop)))]))]

    (fn-for-prof p)))
