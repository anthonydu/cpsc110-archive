;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m03-compound-spider-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@assignment lectures/m03-compound-spider)

(@cwl antdu) ;replace ??? with your cwl

;; Spider, goes down the screen with thread

#|
PROBLEM:

Revise this program so that when the program starts the spider moves down the 
screen, but pressing the space key changes its direction. It should also change
direction when it hits the top or bottom edge.

 - First "reverse-engineer" the domain analysis from the program.
 - Then revise the domain analysis for this new behaviour.
 - Then systematically work your way through the program in the same
   order as HtDW says revising the program to match the new analysis.

   You will need to CHANGE the definition of the Spider type

   When you change a type (a data definition) you need to systematically
   go check on every function that consumes or produces that type to update
   it to the new definition.

Then you can extend the program to make the spider appear to wiggle as it
moves. Unlike in Cowabunga you should represent the state of the wiggle
explicitly as a field in the world state.
|#



;; My creepy crawly spider

(@htdw Spider)

;; =================
;; Constants:

(define WIDTH 400)
(define HEIGHT 600)

(define CTR-X (/ WIDTH 2))

(define SPEED 2) ; pixels per tick

(define SPIDER-RADIUS 10)

(define TOP (+ 0        SPIDER-RADIUS)) ; to be entirely visible
(define BOT (- HEIGHT 1 SPIDER-RADIUS)) ; center has to be in [TOP, BOT]
(define MID (/ HEIGHT 2              ))

(define SPIDER-IMAGE (circle SPIDER-RADIUS "solid" "black"))

(define MTS (empty-scene WIDTH HEIGHT))


;; =================
;; Data definitions:
(@problem 1)
(@htdd Spider)
(define-struct spider (y dy))
;; Spider is (make-spider Number Number)
;; interp. y is spider's vertical position in screen coordinates (pixels)
;;         dy is velocity in pixels per tick, + is down, - is up
;; CONSTRAINT: to be visible, must be in
;;             [TOP, BOT] which is [SPIDER-RADIUS, HEIGHT - SPIDER-RADIUS]
(define S-TOP-D (make-spider TOP  3))   ; top going down
(define S-MID-D (make-spider MID  3))   ; middle going down
(define S-MID-U (make-spider MID -3))   ; middle going up
(define S-BOT-U (make-spider BOT -3))   ; bottom going up


(@dd-template-rules compound);2 fields

(define (fn-for-spider s)
  (... (spider-y s) (spider-dy s)))

;; =================
;; Functions:

(@htdf main)
(@signature Spider -> Spider)
;; start the world with (main TOP)
;; no tests for htdw-main template

(@template-origin htdw-main)

;; no @template for htdw-main template

(define (main s)
  (big-bang s                     ; Spider
    (state     true)              ; FOR DEBUGGING PURPOSES ONLY
    (on-tick   tock)              ; Spider -> Spider
    (to-draw   render)            ; Spider -> Image
    (on-key    reverse-spider)))  ; Spider KeyEvent -> Spider


(@htdf reverse-spider)
(@signature Spider KeyEvent -> Spider)
;; change direction of spider on space, unchanged for other keys

(check-expect (reverse-spider (make-spider 100  2) " ") (make-spider 100 -2))
(check-expect (reverse-spider (make-spider 200 -3) " ") (make-spider 200  3))
(check-expect (reverse-spider (make-spider 100  2) "a") (make-spider 100  2))

; (define (reverse-spider s ke) s)

(@template-origin KeyEvent ; using large enumeration rule
                  Spider)  ; to get Spider selectors in cond answer

(@template
 (define (reverse-spider s ke)
   (cond [(key=? ke " ") (... (spider-y s) (spider-dy s))]
         [else (... (spider-y s) (spider-dy s))])))

(define (reverse-spider s ke)
  (cond [(key=? ke " ") (make-spider (spider-y s) (- (spider-dy s)))]
        [else s]))


(@htdf tock)
(@signature Spider -> Spider)
;; produce the next spider by adding SPEED to s, stopping at bottom
(check-expect (tock (make-spider TOP SPEED))
              (make-spider (+ TOP SPEED) SPEED))
(check-expect (tock (make-spider BOT (- SPEED)))
              (make-spider (+ BOT (- SPEED)) (- SPEED)))

(check-expect (tock (make-spider (- TOP  1 (- SPEED)) (- SPEED)))
              (make-spider (+ TOP 0) SPEED))
(check-expect (tock (make-spider (- TOP  0 (- SPEED)) (- SPEED)))
              (make-spider (+ TOP 0) SPEED))
(check-expect (tock (make-spider (- TOP -1 (- SPEED)) (- SPEED)))
              (make-spider (+ TOP 1) (- SPEED)))

(check-expect (tock (make-spider (- BOT  1 SPEED) SPEED))
              (make-spider (- BOT 1) SPEED))
(check-expect (tock (make-spider (- BOT  0 SPEED) SPEED))
              (make-spider (- BOT 0) (- SPEED)))
(check-expect (tock (make-spider (- BOT -1 SPEED) SPEED))
              (make-spider (- BOT 0) (- SPEED)))

; (define (tock s) s) ;stub

(@template-origin Spider)

(@template
 (define (tock s)
  (... (spider-y s) (spider-dy s))))

(define (tock s)
  (cond [(>= (+ (spider-y s) (spider-dy s)) BOT)
         (make-spider BOT (- (spider-dy s)))]
        [(<= (+ (spider-y s) (spider-dy s)) TOP)
         (make-spider TOP (- (spider-dy s)))]
        [else (make-spider (+ (spider-y s) (spider-dy s)) (spider-dy s))]))






























(@htdf render)
(@signature Spider -> Image)
;; place SPIDER-IMAGE and thread image on MTS
(check-expect (render (make-spider 21 4))
              (add-line (place-image SPIDER-IMAGE CTR-X 21 MTS)
                        CTR-X 0
                        CTR-X 21
                        "black"))

(check-expect (render (make-spider 36 -4))
              (add-line (place-image SPIDER-IMAGE CTR-X 36 MTS)
                        CTR-X 0
                        CTR-X 36
                        "black"))

;(define (render s) MTS)

(@template-origin Spider)

(@template
 (define (render s)
  (... (spider-y s) (spider-dy s))))

(define (render s)
  (add-line (place-image SPIDER-IMAGE
                         CTR-X
                         (spider-y s)
                         MTS)
            CTR-X 0
            CTR-X (spider-y s)
            "black"))


(main (make-spider TOP 10))
