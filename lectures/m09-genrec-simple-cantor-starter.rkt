;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname m09-genrec-simple-cantor-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))

(require 2htdp/image)
(require spd/tags) 
(@assignment lectures/m09-genrec-simple-cantor)

(@cwl antdu) ;replace ??? with your cwl

(@problem 1)

;;
;; Design a function that consumes a width in pixels and produces a simple
;; cantor image of the given width.  For example, (cantor 400) might produce
;; an image like the one in:
;; https://cs110.students.cs.ubc.ca/lectures/m09-genrec-simple-cantor-image.png
;; (Ignore the pale grey areas if they show up in your browser.)



(define BAR-HEIGHT 20)
(define BAR-COLOR  "blue")

(define GAP-HEIGHT 4)

(define SPACER-COLOR "transparent")

(define CUTOFF 4)

(@htdf cantor)
(@signature Number -> Image)
;; Produce cantor set image of given width
;; CONSTRAINT: w >= 0
(check-expect (cantor 0)
              (rectangle 0             BAR-HEIGHT "solid" BAR-COLOR))
(check-expect (cantor (sub1 CUTOFF))
              (rectangle (sub1 CUTOFF) BAR-HEIGHT "solid" BAR-COLOR))
(check-expect (cantor CUTOFF)
              (rectangle CUTOFF        BAR-HEIGHT "solid" BAR-COLOR))
(check-expect (cantor (* 3 CUTOFF))
              (above (rectangle (* 3 CUTOFF) BAR-HEIGHT "solid" BAR-COLOR)
                     (rectangle (* 3 CUTOFF) GAP-HEIGHT "solid" SPACER-COLOR)
                     (beside (cantor CUTOFF)
                             (rectangle CUTOFF BAR-HEIGHT "solid" SPACER-COLOR)
                             (cantor CUTOFF))))

(@template-origin genrec)

#;;template 
(define (genrec-fn lon)
  (cond [(trivial? lon) (trivial-answer lon)]
        [else (... lon
                   (genrec-fn (next-problem lon)))]))

(define (cantor w)
  (cond [(<= w CUTOFF) (rectangle w BAR-HEIGHT "solid" BAR-COLOR)]
        [else
         (local [(define cantor/3 (cantor (/ w 3)))
                 (define bar (rectangle w BAR-HEIGHT "solid" BAR-COLOR))
                 (define spacer
                   (rectangle (/ w 3) BAR-HEIGHT "solid" SPACER-COLOR))
                 (define gap (rectangle w GAP-HEIGHT "solid" SPACER-COLOR))]
           (above bar
                  gap
                  (beside cantor/3
                          spacer
                          cantor/3)))]))







(@template-origin genrec)
#;
(define (genrec-fn d)
  ;; base case:
  ;; reduction:
  ;; argument: 
  
  (cond [(trivial? d) (trivial-answer d)]        
        [else         
         (... d              
              (genrec-fn (next-problem d)))]))
