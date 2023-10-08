;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab-08-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; CPSC 110 - Abstraction Lab

(require spd/tags)
(require racket/file)

(@assignment labs/lab-08)

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

(@problem 1)

;; The first four problems use the following data definition,
;; which represents a path through a binary search tree.

(@htdd Path)
;; Path is one of:
;; - empty
;; - (cons "L" Path)
;; - (cons "R" Path)
;; interp. 
;;  A sequence of left and right 'turns' down through a binary tree
;;  (list "L" "R" "R") means take the left child of the root, then
;;  the right child of that node, and the right child again.
;;  empty means you have arrived at the destination.
(define P1 empty)
(define P2 (list "L" "R"))
(define P3 empty)
(define P4 (cons "L" (cons "R" empty)))
(define P5 (cons "L" (cons "R" (cons "R" empty))))

(@dd-template-rules one-of atomic-distinct self-ref self-ref)
(define (fn-for-path p)
  (cond [(empty? p) (...)]
        [(string=? (first p) "L") (... (fn-for-path (rest p)))]
        [(string=? (first p) "R") (... (fn-for-path (rest p)))]))



;; Design an abstract function (including signature, purpose, and tests)
;; called num-lr to simplify the lefts-minus-rights and rights-minus-lefts
;; functions defined below.
;;
;; Then re-define the original lefts-minus-rights and rights-minus-lefts
;; functions to use your abstract function. Remember, the signature and tests
;; should not change from the original functions. For simplicity, assume 
;; that all numbers throughout this problem have type Integer.


(@htdf lefts-minus-rights)
(@signature Path -> Integer)
;; produce the difference between number of left turns and right turns

(check-expect (lefts-minus-rights empty) 0)
(check-expect (lefts-minus-rights (list "R" "L" "R")) -1)
(check-expect (lefts-minus-rights (list "L" "R" "L"))  1)

(@template-origin use-abstract-fn)
#;
(define (lefts-minus-rights p)
  (cond [(empty? p) 0]
        [(string=? (first p) "L") (add1 (lefts-minus-rights (rest p)))]
        [(string=? (first p) "R") (sub1 (lefts-minus-rights (rest p)))]))

(define (lefts-minus-rights p)
  (num-lr add1 sub1 p))


(@htdf rights-minus-lefts)
(@signature Path -> Integer)
;; produce the difference between number of right turns and left turns

(check-expect (rights-minus-lefts empty) 0)
(check-expect (rights-minus-lefts (list "R" "L" "R"))  1)
(check-expect (rights-minus-lefts (list "L" "R" "L")) -1)

(@template-origin use-abstract-fn)
#;
(define (rights-minus-lefts p)
  (cond [(empty? p) 0]
        [(string=? (first p) "L") (sub1 (rights-minus-lefts (rest p)))]
        [(string=? (first p) "R") (add1 (rights-minus-lefts (rest p)))]))

(define (rights-minus-lefts p)
  (num-lr sub1 add1 p))

;; Use the space below to design the abstract function for Problem 1:

(@htdf num-lr)
(@signature (Integer -> Integer) (Integer -> Integer) Path -> Integer)
;; abstract function for lefts-minus-rights and rights-minus-lefts
(check-expect (num-lr add1 sub1 (list "R" "L" "R")) -1)
(check-expect (num-lr sub1 add1 (list "R" "L" "R"))  1)

(@template-origin Path)

(define (num-lr c1 c2 p)
  (cond [(empty? p) 0]
        [(string=? (first p) "L") (c1 (num-lr c1 c2 (rest p)))]
        [(string=? (first p) "R") (c2 (num-lr c1 c2 (rest p)))]))
















(@problem 2)
;;
;; Use your abstract function from the previous problem to design a function
;; called path-length that determines the length of a given path.

(@htdf path-length)
(@signature Path -> Natural)
;; produces the length of a given path
(check-expect (path-length empty) 0)
(check-expect (path-length (list "R")) 1)
(check-expect (path-length (list "L" "R")) 2)
(check-expect (path-length (list "R" "L" "R")) 3)

(@template-origin use-abstract-fn)

(define (path-length p)
  (num-lr add1 add1 p))
















(@problem 3)
#|
Complete the design of the following abstract fold function for Path.
Note that we have already given you the actual function definition and the
template tag. You must complete the design with a signature, purpose,
function definition and the two following check-expects:

  - uses the fold function to produce a copy of (list "R" "L" "R")
  - uses the fold function to produce the number of "L"s minus the
    number of "R"s in the list (list "R" "L" "R"), which is -1
|#
(@htdf fold-path)
(@signature (X -> X) (X -> X) X Path -> X)
(check-expect (local [(define (appendl l) (cons "L" l))
                      (define (appendr l) (cons "R" l))]
                (fold-path appendl appendr empty (list "R" "L" "R")))
              (list "R" "L" "R"))
(check-expect (fold-path add1 sub1 0 (list "R" "L" "R")) -1)

(@template-origin Path)

(define (fold-path c1 c2 b p)
  (cond [(empty? p) b]
        [(string=? (first p) "L") (c1 (fold-path c1 c2 b (rest p)))]
        [(string=? (first p) "R") (c2 (fold-path c1 c2 b (rest p)))]))













(@problem 4)
;;
;; Use your fold-path function to design a function called path-string that
;; produces a single string that concatenates all of the turns in a path.
;;

(@htdf path-string)
(@signature Path -> String)
;; produces a single string that concatenates all of the turns in a path
(check-expect (path-string empty) "")
(check-expect (path-string (list "R")) "R")
(check-expect (path-string (list "L" "R")) "LR")
(check-expect (path-string (list "R" "L" "R")) "RLR")

(@template-origin use-abstract-fn)

(define (path-string p)
  (local [(define (strappl s) (string-append "L" s))
          (define (strappr s) (string-append "R" s))]
    (fold-path strappl strappr "" p)))














(@problem 5)
;;
;; Design a function called popular-spring-class-count that takes a list of
;; class data and produces the number of classes from Term 2 where enrollment
;; exceeded 70% capacity (that is, enrollment / capacity > 0.7).
;;
;; The function that you design must call at least one built-in abstract
;; functions. Your function definition must not be recursive, and it must
;; not use the (listof Class) template.
;;

(@htdd Class)
(define-struct class (id sec term credits enrolled capacity title))
;; Class is (make-class String String Natural
;;             Natural Natural Natural String)
;; interp. (make-class id sec term credits enrolled capacity title) is
;; data about a UBC CS class where:
;; - id is the class identifier
;; - sec is the class section
;; - term is the term during which the class is held, restricted to [1,2]
;; - credits is the number of credits the course is worth
;; - enrolled is the number of students enrolled
;; - capacity is the number of students that could be enrolled
;; - title is an abbreviated title for the course
;; CONSTRAINT: a class's capacity is always >= 1

(define C0 (make-class "CPSC229" "202" 2 4 40 84 "CMPTNL BSKT WVNG"))
(define C1 (make-class "CPSC259" "201" 2 4 190 188 "DTA&ALG ELEC ENG"))
(define C2 (make-class "CPSC400" "123" 1 0 190 188 "SNRITIS FOR MJRS"))

(@dd-template-rules compound)
(define (fn-for-class c)
  (... (class-id c)
       (class-sec c)
       (class-term c)
       (class-credits c)
       (class-enrolled c)
       (class-capacity c)
       (class-title c)))

;;
;; Design a function called popular-spring-class-count that takes a list of
;; class data and produces the number of classes from Term 2 where enrollment
;; exceeded 70% capacity (that is, enrollment / capacity > 0.7).
;;
;; The function that you design must call at least one built-in abstract
;; functions. Your function definition must not be recursive, and it must
;; not use the (listof Class) template.
;;

;; Use the space below to design the function for Problem 5:

(@htdf popular-spring-class-count)
(@signature (listof Class) -> Natural)
;; produce the number of classes from Term 2 with enrollment over 70%
(check-expect (popular-spring-class-count empty) 0)
(check-expect (popular-spring-class-count (list C0)) 0)
(check-expect (popular-spring-class-count (list C1)) 1)
(check-expect (popular-spring-class-count (list C2)) 0)
(check-expect (popular-spring-class-count (list C0 C1)) 1)
(check-expect (popular-spring-class-count (list C0 C2)) 0)
(check-expect (popular-spring-class-count (list C1 C2)) 1)
(check-expect (popular-spring-class-count
               (list (make-class "" "" 2 0 7 10 ""))) 0)

(@template-origin Class use-abstract-fn)

(define (popular-spring-class-count loc)
  (local [(define (t2&.7+1 c rnr) ; result of natural recursion
            (if (and (= (class-term c) 2)
                     (> (/ (class-enrolled c)
                           (class-capacity c)) 0.7))
                (add1 rnr)
                rnr))]
    (foldr t2&.7+1 0 loc)))

; starting from the right
; first natural recursion returns rnr = 0
; because the last instance of loc is empty
; so the default value init is used
; add1 to the running sum each time it traverses left


  








