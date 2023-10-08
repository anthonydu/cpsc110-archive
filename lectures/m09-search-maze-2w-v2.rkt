;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname m09-search-maze-2w-v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(@assignment lectures/m09-search-maze-2w)

(@cwl antdu) ;replace ??? with your cwl

;; maze-2w-v2.rkt

;; Solve simple square mazes

;; Data definitions:
(@problem 1)
(@htdd Maze)
;; Maze is (listof Boolean)
;; interp. a square maze
;;         each side length is (sqrt (length <maze>))
;;         true  (aka #t) means open, can move through this
;;         false (aka #f) means a wall, cannot move into or through a wall
;; CONSTRAINT: maze is square, so (sqrt (length <maze>)) is a Natural
;;
(define O true)  ;Open
(define W false) ;Wall

(define M0
  (list O W W W
        W W W W
        W W W W
        W W W W))

(define M1
  (list O W W W W
        O O W O O
        W O W W W 
        O O W W W
        O O O O O))

(define M2
  (list O O O O O
        O W W W O
        O W W W O
        O W W W O
        O W W W O))

(define M3             ;forces backtracking in this solver
  (list O O O O O
        O W W W W
        O W W W W
        O W W W W 
        O O O O O))

(define M4
  (list O O O O O
        O W W W O
        O W O O O
        O W O W W
        W W O O O))

(@htdd Position)

(define-struct pos (x y))
;; Position is (make-pos Natural Natural)
;; interp. an x, y position in the maze.
;;         0, 0 is upper left.
;;         a position is only valid for a given maze if:
;;            - (<= 0 x (sub1 <size>))
;;            - (<= 0 y (sub1 <size>))
;;            - there is a true in the given cell
;; 
(define P0 (make-pos 0 0)) ;upper left  in 4x4 maze
(define P1 (make-pos 3 0)) ;upper right  "  "   "
(define P2 (make-pos 0 3)) ;lower left   "  "   "
(define P3 (make-pos 3 3)) ;lower right  "  "   "

(define (fn-for-pos p)
  (... (pos-x p)
       (pos-y p)))

;; Functions:
(@problem 2)
(@htdf solvable?)
(@signature Maze -> Boolean)
;; produce true if maze is solvable, false otherwise
;; assume maze has an empty square at least in the upper left
(check-expect (solvable? M0) #f)
(check-expect (solvable? M1) #t)
(check-expect (solvable? M2) #t)
(check-expect (solvable? M3) #t) 
(check-expect (solvable? M4) #f)

; (define (solvable? m) #f)

(@template-origin genrec arb-tree encapsulated try-catch)

(@template
 (define (solveable? m)
   ;; trivial: reaches lower right
   ;; reduction: moves down and right when possible
   ;; argument: maze is finite, so moving will eventually
   ;;           reach trivial case or run out of moves
   (local [(define (fn-for-pos p)
             (cond [(trivial? p) (trivial-answer p)]
                   [else (fn-for-lop (pos-subs p))]))

           (define (fn-for-lop lop)
             (cond [(empty? lop) (...)]
                   [else
                    (local [try (fn-for-pos (first lop))]
                      (if (not (false? try)) 
                          try
                          (fn-for-lop (rest lop))))]))]

     (fn-for-pos (make-pos 0 0)))))

(define (solvable? m)
  (local [(define (fn-for-pos p)
            (cond [(valid? p) true]
                  [else (fn-for-lop (valid-next-positions p))]))  

          (define (fn-for-lop lop)
            (cond [(empty? lop) false]
                  [else
                   (local [(define try (fn-for-pos (first lop)))]
                     (if (not (false? try))
                         try
                         (fn-for-lop (rest lop))))]))

          (define maze-size (sqrt (length m)))

          ;; (@signature Position -> Boolean)
          ;; produce true if p represents the lower right corner of the maze
          (define (valid? p)
            (local [(define valid-pxy (sub1 maze-size))]
              (and (= (pos-x p) valid-pxy)
                   (= (pos-y p) valid-pxy))))

          ;; (@signature Position -> (listof Position)
          ;; produce list of up to 2 valid next positions (right and down)
          (define (valid-next-positions p)
            (local [(define right (make-pos (add1 (pos-x p)) (pos-y p)))
                    (define down (make-pos (pos-x p) (add1 (pos-y p))))]
              (append (if (get-pos-ow m right) (list right) empty) 
                      (if (get-pos-ow m down) (list down) empty))))
          
          ;; (@signature Maze Position -> Boolean)
          ;; produce if a maze position is open or wall, false if out of range
          (define (get-pos-ow m p)
            (if (and (< (pos-x p) maze-size)
                     (< (pos-y p) maze-size))
                (list-ref m (+ (pos-x p) (* (pos-y p) maze-size)))
                false))]
    
    (fn-for-pos (make-pos 0 0))))







(@htdf mref)
(@signature Maze Position -> Boolean)
;; produce contents of given square in given maze
(check-expect (mref (list #t #f #f #f) (make-pos 0 0)) #t)
(check-expect (mref (list #t #t #f #f) (make-pos 0 1)) #f)

(@template-origin Position)

(define (mref m p)
  (local [(define s (sqrt (length m))) ;each side length
          (define x (pos-x p))
          (define y (pos-y p))]
    (cond [(not (<= 0 x (sub1 s)))
           (error 'mref "x value " x " must be within [0, " (sub1 s) "].")]
          [(not (<= 0 y (sub1 s)))
           (error 'mref "y value " y " must be within [0, " (sub1 s) "].")]
          [else
           (list-ref m (+ x (* y s)))])))



(require 2htdp/image)

(define CELL-SIZE 30)
(define OPEN (square CELL-SIZE "solid" "light gray"))
(define WALL (square CELL-SIZE "solid" "black"))

(@htdf render)
(@signature Maze -> Image)
;; produce simple rendering of a square maze.
(check-expect (render empty) empty-image)
(check-expect (render (list #t #f #t
                            #t #f #f
                            #f #f #f))
              (above (beside OPEN WALL OPEN)
                     (beside OPEN WALL WALL)
                     (beside WALL WALL WALL)))

(@template-origin encapsulated fn-composition use-abstract-fn)

(define (render m)
  (local [(define SIZE (sqrt (length m)))          ;4x4 this is 4
          (define IDXS (build-list SIZE identity)) ;4x4 this is (list 0 1 2 3)
          
          (define (cv->image cv)
            (cond [cv   OPEN]
                  [else WALL]))]
    
    (foldr (lambda (i img)
             (above (foldr (lambda (j img)
                             (beside (cv->image (mref m (make-pos j i)))
                                     img))
                           empty-image
                           IDXS)
                    img))
           empty-image
           IDXS)))
