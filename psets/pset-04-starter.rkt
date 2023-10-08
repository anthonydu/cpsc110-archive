;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pset-04-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR
;; PARTNER.
(require 2htdp/universe)
(require 2htdp/image)
(require spd/tags)

(@assignment psets/pset-04);Do not edit or remove this tag

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


;; Below is our solution to Problem Set 3.
;; For problem set 4 you must improve this program in important ways. Be sure
;; not to change the program's behaviour in any other ways than we are asking.
;; Making extra improvements could cause you problems with the autograder.
;;
;; Modify the program so that instead of there being a single ball there
;; can be any number of balls.  The mouse button should add a new ball.
;; Pressing space should clear all the balls.
;;
;; Again your change should be systematic and complete.  All relevant
;; design elements and tags must be updated.  You must name the new
;; world state type ListOfBall.
;;
;; NOTE, if you work systematically, starting from the HtDW recipe, you
;; will decide that you need a new ListOfBall type; but that the Ball
;; type does not change.  Therefore you should NOT change your next-ball
;; function.  Your render-ball function will also be useful when you
;; work on your place-ball function, although render-ball will have to
;; change some.
;;

(@problem 1)
(@htdw Ball)

;; Constants:
(define WIDTH  605)
(define HEIGHT 535)

(define BALL-RADIUS 10)

(define TOP (+ 0        BALL-RADIUS));these constants define the "inner box"
(define BOT (- HEIGHT 1 BALL-RADIUS));that constrains the center of the ball
(define LEF (+ 0        BALL-RADIUS));
(define RIG (- WIDTH  1 BALL-RADIUS));

(define BALL (circle BALL-RADIUS "solid" "white"))

(define MTS (rectangle WIDTH HEIGHT "solid" "green"))


;; ===========================================================================
;; ===========================================================================
;; Data definitions:

(@htdd Ball)

(define-struct ball (x y dx dy))
;; Ball is (make-ball Number Number Number Number)
;; interp. (make-ball x y dx dy) is ball
;;   - position x, y    in screen coordinates
;;   - velocity dx, dy  in pixels/tick
(define B1 (make-ball (/ WIDTH 2) (/ HEIGHT 2) 4 -3))

(@dd-template-rules compound)

(define (fn-for-ball b)
  (... (ball-x b) 
       (ball-y b) 
       (ball-dx b) 
       (ball-dy b)))

(@htdd ListOfBall)
;; ListOfBall is one of:
;;  - empty
;;  - (cons Ball ListOfBall)
;; interp. a list of Balls
(define lob1 empty)
(define lob2 (list (make-ball 1 1 1 1)))
(define lob3 (list (make-ball 1 1 1 1) (make-ball 2 2 2 2)))

(@dd-template-rules one-of             ;2 cases
                    atomic-distinct    ;empty
                    compound           ;(cons Ball ListOfBall)
                    ref                ;(first lob) is Ball
                    self-ref)          ;(rest lob) is ListOfBall

(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-ball (first lob))
              (fn-for-lob (rest lob)))]))


;; ===========================================================================
;; ===========================================================================
;; Functions:

(@htdf main)
(@signature ListOfBall -> ListOfBall)
;; start the game, call with (main empty)
;; <no tests for main functions>

(@template-origin htdw-main)

(define (main lob)
  (big-bang lob
    (on-draw   render-balls)  ;ListOfBall -> Image
    (on-tick   next-balls)    ;ListOfBall -> ListOfBall
    (on-mouse  handle-mouse)  ;ListOfBall Integer Integer MouseEvent
    ;;                        ;  -> ListOfBall
    (on-key    handle-key)))  ;ListOfBall KeyEvent -> ListOfBall
            

(@htdf render-ball) ; DO NOT CHANGE, not used as helper
(@signature Ball -> Image)
;; place BALL on image at appropriate x, y coordinate
(check-expect (render-ball (make-ball 20 30 3 3))
              (place-image BALL 20 30 MTS))
(check-expect (render-ball (make-ball (- WIDTH 4) (- HEIGHT 5) -2 -3))
              (place-image BALL (- WIDTH 4) (- HEIGHT 5) MTS))
#;
(define (render-ball b) MTS)

(@template-origin Ball)

(@template
 (define (render-ball b)
   (... (ball-x b) 
        (ball-y b) 
        (ball-dx b) 
        (ball-dy b))))

(define (render-ball b)
  (place-image BALL (ball-x b) (ball-y b) MTS))


(@htdf place-ball)
(@signature Ball Image -> Image)
;; place BALL on image at appropriate x, y coordinate
(check-expect (place-ball (make-ball 1 2 3 4) MTS)
              (place-image BALL 1 2 MTS))
(check-expect (place-ball (make-ball 5 6 7 8) (square 10 "solid" "red"))
              (place-image BALL 5 6 (square 10 "solid" "red")))
#;
(define (place-ball b i) i)

(@template-origin Ball)

(@template
 (define (place-ball b)
   (... (ball-x b) 
        (ball-y b) 
        (ball-dx b) 
        (ball-dy b))))

(define (place-ball b i)
  (place-image BALL (ball-x b) (ball-y b) i))


(@htdf render-balls)
(@signature ListOfBall -> Image)
;; place BALLs on image at appropriate x, y coordinate
(check-expect (render-balls empty) MTS)
(check-expect (render-balls (list (make-ball 1 2 3 4)))
              (place-image BALL
                           1
                           2
                           MTS))
(check-expect (render-balls (list (make-ball 1 2 3 4)
                                  (make-ball 5 6 7 8)))
              (place-image BALL
                           1
                           2
                           (place-image BALL
                                        5
                                        6
                                        MTS)))


; (define (render-ball b) MTS) ; stub

(@template-origin ListOfBall)
(@template
 (define (render-balls lob)
   (cond [(empty? lob) (...)]
         [else
          (... (fn-for-ball (first lob))
               (render-balls (rest lob)))])))

(define (render-balls lob)
  (cond [(empty? lob) MTS]
        [else
         (place-ball (first lob)
                     (render-balls (rest lob)))]))


(@htdf next-ball) ; DO NOT CHANGE, used as helper
(@signature Ball -> Ball)
;; produce ball at next x,y; checks bounces off top/right/bottom/left wall
(check-expect (next-ball     (make-ball (+ LEF 1) TOP  3 -4))
              (bounce-top    (make-ball (+ LEF 1) TOP  3 -4)))
(check-expect (next-ball     (make-ball (+ LEF 1) BOT  3  4))
              (bounce-bottom (make-ball (+ LEF 1) BOT  3  4)))
(check-expect (next-ball     (make-ball LEF (+ TOP 1) -3 4))
              (bounce-left   (make-ball LEF (+ TOP 1) -3 4)))
(check-expect (next-ball     (make-ball RIG (+ TOP 1)  3 4))
              (bounce-right  (make-ball RIG (+ TOP 1)  3 4)))
(check-expect (next-ball     (make-ball (/ WIDTH 2) (/ HEIGHT 2) 3 4))
              (glide         (make-ball (/ WIDTH 2) (/ HEIGHT 2) 3 4)))

#;
(define (next-ball b) b)

(@template-origin Number) ; because b is treated as atomic

(@template
 (define (next-ball b)
   (... b)))

(define (next-ball b)
  (cond [(touch-top?    b) (bounce-top b)]
        [(touch-bottom? b) (bounce-bottom b)]
        [(touch-right?  b) (bounce-right b)]
        [(touch-left?   b) (bounce-left b)]
        [else
         (glide b)]))


(@htdf next-balls)
(@signature ListOfBall -> ListOfBall)
;; produce a ListOfBall with all balls at their next x, y location
(check-expect (next-balls empty) empty)
(check-expect (next-balls (list (make-ball 1 2 3 4)))
              (list (next-ball (make-ball 1 2 3 4))))
(check-expect (next-balls (list (make-ball 1 2 3 4)
                                (make-ball 5 6 7 8)))
              (list (next-ball (make-ball 1 2 3 4))
                    (next-ball (make-ball 5 6 7 8))))

; (define (next-balls lob) lob) ; stub

(@template-origin ListOfBall)
(@template
 (define (next-balls lob)
   (cond [(empty? lob) (...)]
         [else
          (... (fn-for-ball (first lob))
               (next-balls (rest lob)))])))

(define (next-balls lob)
  (cond [(empty? lob) lob]
        [else
         (cons (next-ball (first lob))
               (next-balls (rest lob)))]))


(@htdf handle-mouse)
(@signature ListOfBall Integer Integer MouseEvent -> ListOfBall)
;; add new ball when mouse is clicked
;; NOTE: uses random, so testing has to use check-random
(check-random (handle-mouse empty 100 200 "button-down")
              (cons (make-ball 100 200 (- 5 (random 11)) (- 5 (random 11)))
                    empty))
(check-random (handle-mouse (list (make-ball 1 2 3 4)) 300 400 "button-down")
              (cons (make-ball 300 400 (- 5 (random 11)) (- 5 (random 11)))
                    (list (make-ball 1 2 3 4))))
(check-random (handle-mouse empty 100 200 "button-up")
              empty)
(check-random (handle-mouse (list (make-ball 1 2 3 4)) 300 400 "button-up")
              (list (make-ball 1 2 3 4)))

#;
(define (handle-mouse lob x y me) lob)

(@template-origin MouseEvent)

(@template
 (define (handle-mouse lob x y me)
   (cond [(mouse=? me "button-down") (... lob x y)]
         [else
          (... lob x y)])))

(define (handle-mouse lob x y me)
  (cond [(mouse=? me "button-down")
         (cons (make-ball x y (- 5 (random 11)) (- 5 (random 11))) lob)]
        [else lob]))


(@htdf handle-key)
(@signature ListOfBall KeyEvent -> ListOfBall)
;; on space reset to zero balls

(check-expect (handle-key (list B1) " ") empty)
(check-expect (handle-key (list B1) "a") (list B1))

; (define (handle-key lob ke) lob) ; stub

(@template-origin KeyEvent)

(@template   
 (define (handle-key lob ke)
   (cond [(key=? ke " ") (... lob)]
         [else 
          (... lob)])))

(define (handle-key lob ke)
  (cond [(key=? ke " ") empty]
        [else lob]))



















(@htdf touch-top?)
(@signature Ball -> Boolean)
;; true if ball is going up and edge will hit or pass top edge of box
(check-expect (touch-top?    (make-ball LEF (+ TOP  5) 3 -4)) false)
(check-expect (touch-top?    (make-ball LEF (+ TOP  4) 3 -4)) true)
(check-expect (touch-top?    (make-ball LEF (+ TOP  1) 3 -2)) true)
(check-expect (touch-top?    (make-ball LEF (+ TOP  0) 3  2)) false)
#;
(define (touch-top? b) false)

(@template-origin Ball)

(@template
 (define (touch-top? b)
   (... (ball-x b) 
        (ball-y b) 
        (ball-dx b) 
        (ball-dy b))))

(define (touch-top? b)
  (<= (+ (ball-y b) (ball-dy b)) TOP))


(@htdf touch-bottom?)
(@signature Ball -> Boolean)
;; true if ball is going down and edge will hit or pass bottom edge of box
(check-expect (touch-bottom? (make-ball LEF (- BOT 3) 3  2)) false)
(check-expect (touch-bottom? (make-ball LEF (- BOT 2) 3  2)) true)
(check-expect (touch-bottom? (make-ball LEF (- BOT 0) 3  2)) true)
(check-expect (touch-bottom? (make-ball LEF (- BOT 0) 3 -2)) false)
#;
(define (touch-bottom? b) false)

(@template-origin Ball)

(@template
 (define (touch-bottom? b)
   (... (ball-x b) 
        (ball-y b) 
        (ball-dx b) 
        (ball-dy b))))

(define (touch-bottom? b)
  (>= (+ (ball-y b) (ball-dy b)) BOT))


(@htdf touch-left?)
(@signature Ball -> Boolean)
;; true if ball is going left and edge will hit or pass left  edge of box
(check-expect (touch-left?   (make-ball (+ LEF 6) TOP -5 2)) false)
(check-expect (touch-left?   (make-ball (+ LEF 5) TOP -5 2)) true)
(check-expect (touch-left?   (make-ball (+ LEF 0) TOP -5 2)) true)
(check-expect (touch-left?   (make-ball (+ LEF 0) TOP  3 2)) false)
#;
(define (touch-left? b) false)

(@template-origin Ball)

(@template
 (define (touch-left? b)
   (... (ball-x b) 
        (ball-y b) 
        (ball-dx b) 
        (ball-dy b))))

(define (touch-left? b)
  (<= (+ (ball-x b) (ball-dx b)) LEF))


(@htdf touch-right?)
(@signature Ball -> Boolean)
;; true if ball is going right and edge will hit or pass right edge of box
(check-expect (touch-right?  (make-ball (- RIG 6) TOP  5 2)) false)
(check-expect (touch-right?  (make-ball (- RIG 5) TOP  5 2)) true)
(check-expect (touch-right?  (make-ball (- RIG 0) TOP  5 2)) true)
(check-expect (touch-right?  (make-ball (- RIG 0) TOP -3 2)) false)
#;
(define (touch-right? b) false)

(@template-origin Ball)

(@template
 (define (touch-right? b)
   (... (ball-x b) 
        (ball-y b) 
        (ball-dx b) 
        (ball-dy b))))

(define (touch-right? b)
  (>= (+ (ball-x b) (ball-dx b)) RIG))


(@htdf bounce-top)
(@signature Ball -> Ball)
;; produce a ball with top edge 1 pixel off top of box, moving down
;; CONSTRAINT: assume ball is close to top edge and moving up
(check-expect (bounce-top (make-ball (+ LEF 1) (+ TOP 3) 2 -4))
              (make-ball (+ LEF 1) (+ TOP 1) 2  4))
(check-expect (bounce-top (make-ball (+ LEF 2) (+ TOP 6) 3 -7))
              (make-ball (+ LEF 2) (+ TOP 1) 3 7))
#;
(define (bounce-top b) b)

(@template-origin Ball)

(@template
 (define (bounce-top b)
   (... (ball-x b) 
        (ball-y b) 
        (ball-dx b) 
        (ball-dy b))))

(define (bounce-top b)
  (make-ball (ball-x b) (+ TOP 1) (ball-dx b) (- (ball-dy b))))


(@htdf bounce-bottom)
(@signature Ball -> Ball)
;; produce a ball with bottom edge 1 pixel off bottom of box, moving up
;; CONSTRAINT: assume ball is close to bottom edge and moving down
(check-expect (bounce-bottom (make-ball (+ LEF 1) (- BOT 3) 2 4))
              (make-ball (+ LEF 1) (- BOT 1) 2  -4))
(check-expect (bounce-bottom (make-ball (+ LEF 2) (- BOT 6) 3 7))
              (make-ball (+ LEF 2) (- BOT 1) 3 -7))
#;
(define (bounce-bottom b) b)

(@template-origin Ball)

(@template
 (define (bounce-bottom b)
   (... (ball-x b) 
        (ball-y b) 
        (ball-dx b) 
        (ball-dy b))))

(define (bounce-bottom b)
  (make-ball (ball-x b) (- BOT 1) (ball-dx b) (- (ball-dy b))))

(@htdf bounce-left)
(@signature Ball -> Ball)
;; produce a ball with left edge 1 pixel off left of box, moving right
;; CONSTRAINT: assume ball is close to left edge and moving left
(check-expect (bounce-left (make-ball (+ LEF 3) (+ TOP 2) -4 4))
              (make-ball (+ LEF 1) (+ TOP 2) 4 4))
(check-expect (bounce-left (make-ball (+ LEF 5) (+ TOP 2) -8 4))
              (make-ball (+ LEF 1) (+ TOP 2) 8 4))
#; 
(define (bounce-left b) b)

(@template-origin Ball)

(@template
 (define (bounce-left b)
   (... (ball-x b) 
        (ball-y b) 
        (ball-dx b) 
        (ball-dy b))))

(define (bounce-left b)
  (make-ball (+ LEF 1) (ball-y b) (- (ball-dx b)) (ball-dy b) ))


(@htdf bounce-right)
(@signature Ball -> Ball)
;; produce a ball with right edge 1 pixel off right of box, moving left
;; CONSTRAINT: assume ball is close to right edge and moving right
(check-expect (bounce-right (make-ball (- RIG 3) (+ TOP 1) 4 4))
              (make-ball (- RIG 1) (+ TOP 1) -4 4))
(check-expect (bounce-right (make-ball (- RIG 5) (+ TOP 1) 8 4))
              (make-ball (- RIG 1) (+ TOP 1) -8 4))
#;
(define (bounce-right b) b)

(@template-origin Ball)

(@template
 (define (bounce-right b)
   (... (ball-x b) 
        (ball-y b) 
        (ball-dx b) 
        (ball-dy b))))

(define (bounce-right b)
  (make-ball (- RIG 1) (ball-y b) (- (ball-dx b)) (ball-dy b)))



(@htdf glide)
(@signature Ball -> Ball)
;; move ball by dx dy
;; CONSTRAINT: ball is not touching or about to touch any edge of the box
(check-expect (glide (make-ball 100 200 2 3)) (make-ball 102 203 2 3))
(check-expect (glide (make-ball 50 220 -3 -2)) (make-ball 47 218 -3 -2))

;(define (glide b) b)

(@template-origin Ball)

(@template
 (define (glide b)
   (... (ball-x b) 
        (ball-y b) 
        (ball-dx b) 
        (ball-dy b))))

(define (glide b)
  (make-ball (+ (ball-x b) (ball-dx b))
             (+ (ball-y b) (ball-dy b))
             (ball-dx b)
             (ball-dy b)))

