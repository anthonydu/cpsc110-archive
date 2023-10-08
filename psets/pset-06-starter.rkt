;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname pset-06-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment psets/pset-06); Do not edit or remove this tag

;; If you are:
;;   - A 110 or 107 student replace the first set of '???'s with your cwl.
;;     For problem sets, If you have a partner, please replace the second
;;     set of '???'s with their cwl.  Remember this, it is what you will
;;     do with these @cwl annotations for the whole course.
;;   - A UBC Extended Learning student, replace the first set of ??? with
;;     your email address as confirmed in the email you received from
;;     extended learning.  The handin password is also in that email.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   
(@cwl antdu ???)


(@problem 1)
;; Below is the start of a data definition called Course that represents limited
;; information about UBC courses.  Below there are only two example data.  
;; Please complete this definition by adding constants C110, C213, C313 and C317
;; which are representations of the descendent tree for 110, 213, 313 and 317.  
;; You can find the information you need at
;;  https://cs110.students.cs.ubc.ca/psets/pset-06-image.png
;;
;; NOTE 1: Use the information in the image above, rather than any other source.
;;         We are significantly simplying the information.
;;
;; NOTE 2: Do this very carefully, the autograder wants to see correct results
;;         from the functions you design to operate on this data.
;;
;; NOTE 3: The tree you will make for C110 will be a bit odd because 210 has 110
;;         as a pre-req, and both 213 and 221 have 210 as a pre-req, and 313 has
;;         213 AND 221 as a pre-req, and 317 has 213 AND 221 as a pre-req. As a
;;         result, 313 and 317 will both show up twice in your descendent tree
;          for C110. This is okay for this problem set.
;; NOTE 4: Expect this step of the problem set to take you some time.


(@htdd Course ListOfCourse)
(define-struct course (number credits dependents))
;; Course is (make-course Natural Natural ListOfCourse)
;; interp. a course with a course number,
;;         the number of credits the course is worth, and a
;;         list of courses that list this course as a direct pre-requisite

;; ListOfCourse is one of:
;; - empty
;; - (cons Course ListOfCourse)
;; interp. a list of courses

(define LOC0 empty)
(define C100 (make-course 100 3 LOC0))

(define C322 (make-course 322 3 LOC0))
(define C320 (make-course 320 3 LOC0))
(define C319 (make-course 319 4 LOC0))
(define C317 (make-course 317 3 LOC0))
(define C314 (make-course 314 3 LOC0))
(define C313 (make-course 313 3 LOC0))
(define C312 (make-course 312 3 LOC0))
(define C311 (make-course 311 3 LOC0))
(define C310 (make-course 310 4 (list C319)))
(define C304 (make-course 304 3 LOC0))
(define C303 (make-course 303 3 LOC0))
(define C302 (make-course 302 3 LOC0))
(define C221 (make-course 221 4 (list C304 C313 C314 C317 C320 C322)))
(define C213 (make-course 213 4 (list C313 C317)))
(define C210 (make-course 210 4 (list C213 C221 C310 C311 C312)))
(define C203 (make-course 203 3 LOC0))
(define C189 (make-course 189 1 LOC0))
(define C110 (make-course 110 4 (list C189 C203 C210 C213 C302 C303)))

(define (fn-for-course c)
  (... (course-number c)
       (course-credits c)
       (fn-for-loc (course-dependents c))))

(define (fn-for-loc loc)
  (cond [(empty? loc) (...)]
        [else
         (... (fn-for-course (first loc))
              (fn-for-loc (rest loc)))]))

(@problem 2)
;;
;; Design a function that produces the list of all the course numbers in the
;; course's tree including the given course's number.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf all-numbers--course all-numbers--loc)
(@signature Course -> ListOfNatural)
(@signature ListOfCourse -> ListOfNatural)
;; produces the list of all course numbers in a Course tree
(check-expect (all-numbers--loc empty) empty)
(check-expect (all-numbers--loc (list C310 C319))
              (list 310 319 319))
(check-expect (all-numbers--course C100) (list 100))
(check-expect (all-numbers--course C110)
              (list 110
                    189
                    203
                    210 213 313 317
                    221 304 313 314 317 320 322
                    310 319
                    311 312
                    213 313 317
                    302
                    303))

; (define (all-numbers--course c) empty) ; stub
; (define (all-numbers--loc c) empty) ; stub

(@template-origin Course)
(@template
 (define (all-numbers--course c)
   (... (course-number c)
        (course-credits c)
        (all-numbers--loc (course-dependents c)))))

(define (all-numbers--course c)
  (append (list (course-number c))
          (all-numbers--loc (course-dependents c))))

(@template-origin ListOfCourse)
(@template
 (define (all-numbers--loc loc)
   (cond [(empty? loc) (...)]
         [else
          (... (all-numbers--course (first loc))
               (all-numbers--loc (rest loc)))])))

(define (all-numbers--loc loc)
  (cond [(empty? loc) empty]
        [else
         (append (all-numbers--course (first loc))
                 (all-numbers--loc (rest loc)))]))


(@problem 3)
;;
;; Design a function that takes two arguments: a Course and a Natural, in that
;; order. It produces the list of courses in the tree that are worth that
;; many credits or more.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf all-credits>=--course all-credits>=--loc)
(@signature Course Natural -> ListOfCourse)
(@signature ListOfCourse Natural -> ListOfCourse)
;; produce a list of courses in the tree that are worth n credits or more
(check-expect (all-credits>=--loc empty 999) empty)
(check-expect (all-credits>=--course C221 4) (list C221))
(check-expect (all-credits>=--course C221 3)
              (list C221 C304 C313 C314 C317 C320 C322))
(check-expect (all-credits>=--course C110 4)
              (list C110 C210 C213 C221 C310 C319 C213))
(check-expect (all-credits>=--loc (list C110 C100) 4)
              (list C110 C210 C213 C221 C310 C319 C213))

; (define (all-credits>=--course c n) empty) ; stub
; (define (all-credits>=--loc loc n) empty) ; stub

(@template-origin Course)
(@template
 (define (all-credits>=--course c n)
   (... n
        (course-number c)
        (course-credits c)
        (all-credits>=--loc (course-dependents c) n))))

(define (all-credits>=--course c n)
  (if (>= (course-credits c) n)
      (append (list c) (all-credits>=--loc (course-dependents c) n))
      (all-credits>=--loc (course-dependents c) n)))

(@template-origin ListOfCourse)
(@template
 (define (all-credits>=--loc loc n)
   (cond [(empty? loc) (...)]
         [else
          (... (all-credits>=--course (first loc) n)
               (all-credits>=--loc (rest loc) n))])))

(define (all-credits>=--loc loc n)
  (cond [(empty? loc) empty]
        [else
         (append (all-credits>=--course (first loc) n)
                 (all-credits>=--loc (rest loc) n))]))

(@problem 4)
;;
;; Design a function that produces the largest course number in the tree.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf largest-number--course largest-number--loc)
(@signature Course -> Natural)
(@signature ListOfCourse -> Natural)
;; produces the largest course number in the tree
(check-expect (largest-number--loc empty) 0)
(check-expect (largest-number--course C100) 100)
(check-expect (largest-number--course C110) 322)
(check-expect (largest-number--course C213) 317)
(check-expect (largest-number--loc (list C100 C110)) 322)

; (define (largest-number--course c) 0) ; stub
; (define (largest-number--loc loc) 0) ; stub

(@template-origin Course)
(@template
 (define (largest-number--course c)
   (... (course-number c)
        (course-credits c)
        (largest-number--loc (course-dependents c)))))

(define (largest-number--course c)
  (if (> (course-number c) (largest-number--loc (course-dependents c)))
      (course-number c)
      (largest-number--loc (course-dependents c))))

(@template-origin ListOfCourse)
(@template
 (define (largest-number--loc loc)
   (cond [(empty? loc) (...)]
         [else
          (... (largest-number--course (first loc))
               (largest-number--loc (rest loc)))])))

(define (largest-number--loc loc)
  (cond [(empty? loc) 0]
        [else
         (if (> (largest-number--course (first loc))
                (largest-number--loc (rest loc)))
             (largest-number--course (first loc))
             (largest-number--loc (rest loc)))]))

(@problem 5)
;;
;; Design a function that takes two arguments: a Course and a Natural, in that
;; order. It produces the course in the tree with that course number. If it
;; can't find a course in the given tree with that course number, it signals
;; failure by producing false.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf find--course find--loc)
(@signature Course Natural -> Course or false)
(@signature ListOfCourse Natural -> Course or false)
;; produces the course in the tree with the course number
(check-expect (find--loc empty 100) false)
(check-expect (find--course C110 121) false)
(check-expect (find--course C210 314) C314)
(check-expect (find--course C314 314) C314)
(check-expect (find--loc (list C110 C210) 121) false)
(check-expect (find--loc (list C110 C210) 314) C314)

; (define (find--course c n) false) ; stub
; (define (find--loc loc n) false) ; stub

(@template-origin Course)
(@template
 (define (find--course c n)
   (... n
        (course-number c)
        (course-credits c)
        (find--loc (course-dependents c)))))

(define (find--course c n)
  (if (= (course-number c) n)
      c
      (find--loc (course-dependents c) n)))

(@template-origin ListOfCourse try-catch)
(@template
 (define (find--loc loc n)
   (cond [(empty? loc) (...)]
         [else
          (if (not (false? (find--course (first loc) n)))
              (find--course (first loc) n)
              (find--loc (rest loc) n))])))

(define (find--loc loc n)
  (cond [(empty? loc) false]
        [else
         (if (not (false? (find--course (first loc) n)))
             (find--course (first loc) n)
             (find--loc (rest loc) n))]))
