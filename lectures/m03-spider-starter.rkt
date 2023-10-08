;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m03-spider-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@assignment lectures/m03-spider)

(@cwl antdu) ;replace ??? with your cwl


(@problem 1)
#|
PROBLEM:

Design a world program in which a spider starts at the top of the screen
and slowly drops down it. The spider should stop when it reaches the bottom
of the screen.

You can improve your spider by re-running the HtDW recipe to add these
features. 


  - Draw a line from the top of the screen to the spider, this is the thread 
    it is hanging from. You will need to use add-line for this. Look in the
    DrRacket help desk to see how add-line works.  [NOTE that adding this
    functionality will cause the autograder to complain, the autograder is
    just designed to grade the original problem.]
    
  - Arrange for pressing the space key to reset the spider to the top of 
    the screen.
|#



;; a creepy spider that crawls down the screen
(@htdw Spider)



;; =================
;; Constants:

(define WIDTH 400)
(define HEIGHT 600)

(define CTR-X (/ WIDTH 2))
(define SPIDER-RADIUS 10)
(define SPEED 2)


(define TOP (+ 0 SPIDER-RADIUS))
(define BOT (- HEIGHT 1 SPIDER-RADIUS)) ; the first row of pixels is 0

(define MTS (rectangle WIDTH HEIGHT "solid" "white"))
(define SPIDER-IMAGE (circle SPIDER-RADIUS "solid" "black"))



;; =================
;; Data definitions:

(@htdd Spider)
;; Spider is Number
;; interp. the y coordinate of a spider
;;         (the distance from the center of the spider to the top of the screen)
;; CONSTRAINT: For spider to be fully visible, must be in [TOP, BOT]
(define y-coord1 TOP)
(define y-coord2 (/ HEIGHT 2))
(define y-coord3 BOT)

(@dd-template-rules atomic-non-distinct)

(define (fn-for-spider s) (... s))



;; =================
;; Functions:

(@htdf main)
(@signature Spider -> Spider)
;; start the world with (main TOP)

(@template-origin htdw-main)

(define (main s)
  (big-bang s            ; Spider
    (on-tick   tock)     ; Spider -> Spider
    (to-draw   render)   ; Spider -> Image
    ;(stop-when ...)     ; Spider -> Boolean
    ;(on-mouse  ...)     ; Spider Integer Integer MouseEvent -> Spider
    ;(on-key    ...)     ; Spider KeyEvent -> Spider
    ))


(@htdf tock)
(@signature Spider -> Spider)
;; produce the next by adding SPEED to Spider, unless the sum exceeds BOT

(check-expect (tock TOP) (+ TOP SPEED))
(check-expect (tock (- BOT SPEED 1)) (- BOT 1))
(check-expect (tock (- BOT SPEED))         BOT)
(check-expect (tock (- BOT (/ SPEED 2)))   BOT)
(check-expect (tock BOT)                   BOT)

; (define (tock s) s) ; stub

(@template-origin Spider)
(@template (define (tock s) (... s)))

(define (tock s)
  (if (<= s (- BOT SPEED)) (+ s SPEED) BOT))



(@htdf render)
(@signature Spider -> Image)
;; render world state by placing SPIDER-IMAGE on MTS at Spider

(check-expect (render TOP) (place-image SPIDER-IMAGE CTR-X TOP MTS))
(check-expect (render BOT) (place-image SPIDER-IMAGE CTR-X BOT MTS))

; (define (render s) SPIDER-IMAGE) ; stub

(@template-origin Spider)
(@template (define (fn-for-spider s) (... s)))

(define (render s) (place-image SPIDER-IMAGE CTR-X s MTS))