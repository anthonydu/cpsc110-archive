;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m04-lon-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment lectures/m04-lon)

(@cwl antdu) ;replace ??? with your cwl

(@problem 1)

(@htdd ListOfNumber)
;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 1 (cons 2 (cons 3 empty))))

(@dd-template-rules one-of             ;2 cases
                    atomic-distinct    ;empty
                    compound           ;(cons Number ListOfNumber)
                    self-ref)          ;(rest lon) is ListOfNumber

(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;;
;; Design a function that computes the sum of a list of numbers.
;; 

(@htdf sum)
(@signature ListOfNumber -> Number)

(check-expect (sum empty) 0)
(check-expect (sum (list 1 2 3 4 5)) 15)
(check-expect (sum (list 5 4 3 2 1)) 15)

; (define (sum lon) 0) ; stub

(@template-origin ListOfNumber)
(@template (define (sum lon)
             (cond [(empty? lon) (...)]
                   [else
                    (... (first lon)
                         (sum (rest lon)))])))

(define (sum lon)
  (cond [(empty? lon) 0]
        [else (+ (first lon) (sum (rest lon)))]))



(@problem 2)
;;
;; Design a function that computes the product of the numbers in a 
;; list of numbers.
;;

(@htdf product)
(@signature ListOfNumber -> Number)

(check-expect (product empty) 1)
(check-expect (product (list 0 1 2 3 4 5)) 0)
(check-expect (product (list 6 5 4 3 2 1)) 720)

; (define (product lon) 0) ; stub

(@template-origin ListOfNumber)
(@template (define (product lon)
             (cond [(empty? lon) (...)]
                   [else
                    (... (first lon)
                         (product (rest lon)))])))

(define (product lon)
  (cond [(empty? lon) 1]
        [else (* (first lon) (product (rest lon)))]))


(@problem 3)
;;
;; Design a function that counts the number of elements in a
;; list of numbers.
;; 

(@htdf count)
(@signature ListOfNumber -> Natural)

(check-expect (count empty) 0)
(check-expect (count (list 1 4 2 8 5 7)) 6)

; (define (count lon) 0) ; stub

(@template-origin ListOfNumber)
(@template (define (count lon)
             (cond [(empty? lon) (...)]
                   [else
                    (... (first lon)
                         (count (rest lon)))])))

(define (count lon)
  (cond [(empty? lon) 0]
        [else (+ 1 (count (rest lon)))]))


(@problem 4)
;;
;; Design a function that produces a new list, where each element  
;; is 2 times the corresponding element in the original list.
;;

(@htdf doubles)
(@signature ListOfNumber -> ListOfNumber)

(check-expect (doubles empty) empty)
(check-expect (doubles (list 0 1 2 3 4 5)) (list 0 2 4 6 8 10))

; (define (product lon) 0) ; stub

(@template-origin ListOfNumber)
(@template (define (doubles lon)
             (cond [(empty? lon) (...)]
                   [else
                    (... (first lon)
                         (doubles (rest lon)))])))

(define (doubles lon)
  (cond [(empty? lon) empty]
        [else (cons (* 2 (first lon)) (doubles (rest lon)))]))
