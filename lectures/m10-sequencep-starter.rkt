;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname m10-sequencep-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(@assignment lectures/m10-sequencep)
(@cwl antdu) ;replace ??? with your cwl

(@problem 1)
;;
;; PROBLEM:
;;
;; Design a function called sequence? that consumes a list of natural numbers
;; and produces true if the list is a sequence, meaning each number is 1 larger
;; than the number before it.  You may assume the list has at least one element.
;; So:
;;    (sequence? (list 2 3 4))  ==> true
;;    (sequence? (list 3 5 6))  ==> false
;;

(@htdf sequence?)
(@signature (listof Natural) -> Boolean)
;; produces true if the list is a strict sequence
;; CONSTRAINT: list has at least one element
(check-expect (sequence? (list 4)) true)
(check-expect (sequence? (list 2 3 4)) true)
(check-expect (sequence? (list 3 5 6)) false)

(@template-origin (listof Natural) accumulator)
#;
(define (sequence? lon)
  (cond [(= (length lon) 1) true]
        [else
         (and (= (add1 (first lon)) (second lon))
              (sequence? (rest lon)))]))

(define (sequence? lon0)
  ;; acc is Natural
  ;; invariant: the element of lon0 immediately before (first lon)
  (local [(define (sequence? lon acc)
            (cond [(empty? lon) true]
                  [else
                   (if (= (add1 acc) (first lon)) ; exploit
                       (sequence? (rest lon) (first lon)) ; preserve
                       ; because (= (add1 acc) (first lon))
                       ; therefore (add1 acc) can also be used here
                       false)]))]
    (sequence? (rest lon0) (first lon0))))










(define (fibseq1 n0)
  (local [(define (fib n)
            (cond [(= n 0) 0]
                  [(= n 1) 1]
                  [else (+ (fib (- n 1)) (fib (- n 2)))]))]
    (cond [(= n0 0) empty]
          [else (append (fibseq1 (sub1 n0))
                        (list (fib n0)))])))

(define (fibseq2 n0)
  ;; n-2, n-1 are Natural
  ;; invariant: the two numbers in the generated sequence that come before n
  (local [(define (fibseq count n-2 n-1)
            (cond [(zero? count) empty]
                  [else (cons (+ n-2 n-1)                           ; exploit
                              (fibseq (sub1 count) n-1 (+ n-2 n-1)) ; preserve
                              )]))]
    (cons 1 (fibseq (sub1 n0) 0 1))))

(fibseq1 1)
(fibseq2 1)
(fibseq1 2)
(fibseq2 2)
(fibseq1 10)
(fibseq2 10)