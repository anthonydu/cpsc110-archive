;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname lab-06-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(@assignment labs/lab-06)

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
;; DATA DEFINITIONS ===============

(define TEXT-SIZE 10)
(define TEXT-COLOUR "black")

(@htdd SentenceTree ListOfSentenceTree)
(define-struct stree (prefix subs))
;; SentenceTree is (make-stree String ListOfSentenceTree)
;; interp. an arbitrary-arity SentenceTree
;;  stree has prefix and a list of sub-SentenceTree's

;; ListOfSentenceTree is one of:
;;  - empty
;;  - (cons SentenceTree ListOfSentenceTree)
;; interp. a list of SentenceTree's

(define ST1221 (make-stree "IN A BACK TO SCHOOL SPECIAL ABOUT MONO" empty))
(define ST1222 (make-stree "PERCHED ON THE TIP OF A SINKING SHIP" empty))

(define ST121 (make-stree "YOU REALLY MEAN IT" empty))
(define ST122 (make-stree "WE ARE" (list ST1221 ST1222)))

(define ST131 (make-stree "FREEZE TIME" empty))
(define ST132 (make-stree "MY FAVOURITE SONG ON REPEAT" empty))

(define ST11 (make-stree "JOKING ABOUT JEALOUSY" empty))
(define ST12 (make-stree "LIKE" (list ST121 ST122)))
(define ST13 (make-stree "TO" (list ST131 ST132)))

(define ST1 (make-stree "KISS ME" (list ST11 ST12 ST13)))

(@template-origin SentenceTree)

(define (fn-for-stree st)
  (... (stree-prefix st)
       (fn-for-lost (stree-subs st))))

(@template-origin ListOfSentenceTree)

(define (fn-for-lost lost)
  (cond [(empty? lost) (...)]
        [else
         (... (fn-for-stree (first lost))
              (fn-for-lost (rest lost)))]))

;; FUNCTIONS ======================
(@problem 2)
(@htdf sentence-count--stree sentence-count--lost)
(@signature SentenceTree -> Natural)
(@signature ListOfSentenceTree -> Natural)
;; counts the number of sentences in a sentence tree
(check-expect (sentence-count--stree ST1) 6)
(check-expect (sentence-count--stree ST11) 1)
(check-expect (sentence-count--stree ST12) 3)
(check-expect (sentence-count--lost empty) 0)
(check-expect (sentence-count--lost (list ST11)) 1)
(check-expect (sentence-count--lost (list ST11 ST12 ST13)) 6)

; (define (sentence-count st) 0) ; stub

(@template-origin SentenceTree)
(@template
 (define (sentence-count--stree st)
   (... (stree-prefix st)
        (sentence-count--lost (stree-subs st)))))

(define (sentence-count--stree st)
  (+ (if (empty? (stree-subs st)) 1 0)
     (sentence-count--lost (stree-subs st))))

(@template-origin ListOfSentenceTree)
(@template
 (define (sentence-count--lost lost)
   (cond [(empty? lost) (...)]
         [else
          (... (sentence-count--stree (first lost))
               (sentence-count--lost (rest lost)))])))

(define (sentence-count--lost lost)
  (cond [(empty? lost) 0]
        [else
         (+ (sentence-count--stree (first lost))
            (sentence-count--lost (rest lost)))]))

(@problem 3)
(@htdf render--stree render--lost)
(@signature SentenceTree -> Image)
(@signature ListOfSentenceTree -> Image)
;; render SentenceTree or ListOfTree as an image
(check-expect (render--stree (make-stree "" empty))
              (text "" TEXT-SIZE TEXT-COLOUR))
(check-expect (render--stree (make-stree "hello world" empty))
              (text "hello world" TEXT-SIZE TEXT-COLOUR))
(check-expect (render--lost empty)
              empty-image)
(check-expect (render--lost (list ST11 ST12 ST13))
              (above (render--stree ST11)
                     (render--stree ST12)
                     (render--stree ST13)))
(check-expect (render--stree ST1)
              (beside (text "KISS ME" TEXT-SIZE TEXT-COLOUR)
                      (render--lost (list ST11 ST12 ST13))))

(@template-origin SentenceTree)
(@template
 (define (render--stree st)
   (... (stree-prefix st)
        (render--lost (stree-subs st)))))

(define (render--stree st)
  (beside (text (stree-prefix st) TEXT-SIZE TEXT-COLOUR)
          (render--lost (stree-subs st))))

(@template-origin ListOfSentenceTree)
(@template
 (define (render--lost lost)
   (cond [(empty? lost) (...)]
         [else
          (... (render--stree (first lost))
               (render--lost (rest lost)))])))

(define (render--lost lost)
  (cond [(empty? lost) empty-image]
        [else
         (above (render--stree (first lost))
                (render--lost (rest lost)))]))
