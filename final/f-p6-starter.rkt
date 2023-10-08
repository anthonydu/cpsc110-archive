;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname f-p6-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.

(require spd/tags)

(@assignment exams/2022w1-f/f-p6);Do not edit or remove this tag

(@cwl antdu)   ;fill in your CWL here (same as for problem sets)

(@problem 1) ;do not edit or delete this line
(@problem 2) ;do not edit or delete this line
(@problem 3) ;do not edit or delete this line
(@problem 4) ;do not edit or delete this line
(@problem 5) ;do not edit or delete this line
(@problem 6) ;do not edit or delete this line

#|

In a continuing sign that the oligarchs are taking over the world, Elon Musk
has now bought the North Pole, including Santa Claus' entire operation there.
Unsurprisingly his first move was to fire half the elves.  His second move was
to realize that none of the packages are packed yet for this year's big delivery
run. With so many elves now fired it's going to be difficult to figure out how
to pack. Some of the fired elves were expert at that.

Before being fired one of the elves had started writing a solver to figure out
how to package all the gifts. Unfortunately she didn't get the program done, and
you have to finish it.

But fortunately she worked in a very systematic way, so you should be able to
get it completed. The analysis of the search tree is in f-p6-figure.pdf. Below
is the code file in its current state. As you will see, a lot of work has
already been done. In this problem you:
 
  - MUST COMPLETE THE 2 WISH LIST ENTRIES with check-expects, a template
    origin tag, and a working function definition 
  - SHOULD NOT HAVE TO, but are PERMITTED to change the check-expects for
    solve, in case your helpers produce results in a different order
  - SHOULD USE the has-room-for? helper that Eva has already written


NOTE: This problem will be autograded.
      **ALL OF THE FOLLOWING ARE ESSENTIAL** in your solution.
      Failure to follow these requirements may result in receiving zero marks.

 - the functions you design must conform to the wish list entries
 - you must design the two functions at top-level, with all recipe elements
 - you must include correct @htdf, @signature and @template-origin tags
 - you must not comment out any of the tags
 - your submission must pass the Check Syntax button
 - you must follow all applicable design rules
 - MUST NOT CHANGE the data definitions or example data
 - MUST NOT CHANGE the signature of any function
 - MUST NOT CHANGE the definition of any function for which there
   is already a complete definition


|#

;; Data definitions:

(@htdd Package)
(define-struct pkg (size gifts))
;; Slot is (make-pkg Natural (listof Gift))
;; interp. a package to go on the sleigh, with its:
;;           - size measured in sugarplums
;;           - a list of all the gifts it contains
(define P5 (make-pkg 5 empty))
(define P8 (make-pkg 8 empty))

(define P1 (make-pkg 1 empty))
(define P2 (make-pkg 2 empty))
(define P3 (make-pkg 3 empty))
(define P4 (make-pkg 4 empty))
(define P6 (make-pkg 6 empty))
(define P7 (make-pkg 7 empty))


(@htdd Gift)
(define-struct gift (size for))
;; Gift is (make-gift Integer String)
;; interp. A gift, with its size in sugarplums and
;;         the name of the person it is for.
(define G3X (make-gift 3 "X"))
(define G4Y (make-gift 4 "Y"))
(define G2Z (make-gift 2 "Z"))

(define G1 (make-gift 1 "Ely"))
(define G2 (make-gift 1 "Bernie"))
(define G3 (make-gift 2 "Gautie"))
(define G4 (make-gift 2 "JB"))
(define G5 (make-gift 3 "BillG"))
(define G6 (make-gift 3 "Warr"))

(@htdd SearchState)
(define-struct ss (packages unpacked-gifts))
;; SearchState is (make-ss (listof Package) (listof Gift))
;; interp. The changing information at one point in the search tree.
;;         packages is all the original supplied packages, except
;;         that some of them may have had gifts added to them.
;;         unpacked-gifts is the gifts that still remain to be packed.

(@htdf solve)
(@signature (listof Package) (listof Gift) -> (listof Package) or false)
;; produce packages containing all the supplied gifts, or fail
(check-expect (solve empty empty) empty) 
(check-expect (solve (list P1 P2 P3) empty) (list P1 P2 P3))
(check-expect (solve empty (list G1)) false)
(check-expect (solve (list P1) (list G1))
              (list (make-pkg 1 (list G1))))
(check-expect (solve (list P1) (list G1 G2)) false)
(check-expect (solve (list P1 P3) (list G1 G2 G3 G4)) false)
(check-expect (solve (list P1 P2 P3 P4) (list G4 G3 G2 G1))
              (list (make-pkg 1 (list (make-gift 1 "Ely")))
                    (make-pkg 3
                              (list (make-gift 1 "Bernie")
                                    (make-gift 2 "Gautie")))
                    (make-pkg 2 (list (make-gift 2 "JB")))
                    (make-pkg 4 empty)))


(@template-origin genrec arb-tree try-catch encapsulated)

(define (solve lop log)
  (local [;;
          ;; trivial case:
          ;;  all gifts have been packed,
          ;;  or no package exists with room in it for first gift
          ;; reduction step:
          ;;  make one new ss for each package that has room for
          ;;  the first remaining gift
          ;; argument:
          ;;  there's a finite number of packages and gifts, at
          ;;  each step the number of gifts remaining to pack is
          ;;  reduced by one
          (define (search/one ss)
            (cond [(succeeded? ss) (ss-packages ss)]
                  [else
                   (search/list (next-search-states ss))]))
          
          (define (search/list loss)
            (cond [(empty? loss) false]
                  [else
                   (local [(define try (search/one (first loss)))]
                     (if (not (false? try))
                         try
                         (search/list (rest loss))))]))]
    
    (search/one (make-ss lop log))))

;; these are the 2 wish-list entries you must complete

(@htdf succeeded?)
(@signature SearchState -> Boolean)
;; produce true if there are no more unpacked gifts
(check-expect (succeeded? (make-ss (list (make-pkg 2 empty))
                                   (list (make-gift 3 "")))) false)
(check-expect (succeeded? (make-ss (list (make-pkg 4 (list (make-gift 4 "A"))))
                                   (list (make-gift 4 "A")
                                         (make-gift 3 "B")))) false)
(check-expect (succeeded? (make-ss (list (make-pkg 4 (list (make-gift 3 "B"))))
                                   (list (make-gift 5 "A")
                                         (make-gift 3 "B")))) false)
(check-expect (succeeded? (make-ss (list (make-pkg 2 empty)) empty)) true)
(check-expect (succeeded? (make-ss empty empty)) true)

(@template-origin SearchState)

(define (succeeded? ss)
  (cond [(empty? (ss-unpacked-gifts ss)) true]
        [else false]))



(@htdf next-search-states)
(@signature SearchState -> (listof SearchState))
;; produce one next search state for each package that has room for first gift
(check-expect (next-search-states (make-ss (list (make-pkg 2 empty))
                                           (list (make-gift 3 ""))))
              empty)
(check-expect (next-search-states (make-ss (list (make-pkg 4 empty))
                                           (list (make-gift 4 "A")
                                                 (make-gift 3 "B"))))
              (list (make-ss (list (make-pkg 4 (list (make-gift 4 "A"))))
                             (list (make-gift 3 "B")))))
(check-expect (next-search-states (make-ss
                                   (list (make-pkg 8 (list (make-gift 3 "B")))
                                         (make-pkg 9 (list (make-gift 5 "A")))
                                         (make-pkg 5 empty)
                                         (make-pkg 4 empty))
                                   (list (make-gift 5 "A")
                                         (make-gift 3 "B"))))
              (list (make-ss (list (make-pkg 8 (list (make-gift 5 "A")
                                                     (make-gift 3 "B")))
                                   (make-pkg 9 (list (make-gift 5 "A")))
                                   (make-pkg 5 empty)
                                   (make-pkg 4 empty))
                             (list (make-gift 3 "B")))
                    (make-ss (list (make-pkg 5 (list (make-gift 5 "A")))
                                   (make-pkg 8 (list (make-gift 3 "B")))
                                   (make-pkg 9 (list (make-gift 5 "A")))
                                   (make-pkg 4 empty))
                             (list (make-gift 3 "B")))))
(check-expect (next-search-states (make-ss empty (list (make-gift 5 "A")
                                                       (make-gift 3 "B"))))
              empty)

(@template-origin use-abstract-fn fn-composition)

(define (next-search-states ss)
  (local [(define (has-room? pkg)
            (has-room-for? pkg (first (ss-unpacked-gifts ss))))
          
          (define (add-pkg pkg)
            (make-ss (cons (make-pkg (pkg-size pkg)
                                     (cons (first (ss-unpacked-gifts ss))
                                           (pkg-gifts pkg)))
                           (remove pkg (ss-packages ss)))
                     (rest (ss-unpacked-gifts ss))))]
    
    (map add-pkg (filter has-room? (ss-packages ss)))))






;; The following helper function should be useful for you.

(@signature Package Gift -> Boolean)
;; produce true if total of existing gifts in package leaves room to add gift
(check-expect (has-room-for? P5 (make-gift 4 "X")) true)
(check-expect (has-room-for? P6 (make-gift 6 "X")) true)
(check-expect (has-room-for? P7 (make-gift 8 "X")) false)
(check-expect (has-room-for? (make-pkg 7 (list G3 G5)) G1) true)
(check-expect (has-room-for? (make-pkg 7 (list G3 G5)) G4) true)
(check-expect (has-room-for? (make-pkg 6 (list G3 G6)) G4) false)

(define (has-room-for? p g)
  (<= (+ (foldl + 0 (map gift-size (pkg-gifts p)))
         (gift-size g))
      (pkg-size p)))



