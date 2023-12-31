;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-04-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

(@assignment labs/lab-04)

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

;; A *SIMPLE* one line text editor
;;
;; The screen looks like:
;; 
;;     ab|c
;;
;;   where | is the cursor.
;;
;; Typing characters inserts them.
;; left and right arrow moves cursor
;; delete removes character before cursor

(@htdw Editor)
;; =================
;; Constants:

(define WIDTH  200)
(define HEIGHT  20)

(define TEXT-SIZE  18)
(define TEXT-COLOR "BLACK")

(define CURSOR (rectangle 1 20 "solid" "red"))

(define MTS (empty-scene WIDTH HEIGHT))


;; =================
;; Data Definitions:

(@htdd Editor)
(define-struct editor (pre post))
;; Editor is (make-editor String String)
;; interp. (make-editor pre post) is an editor
;;   pre  is text before cursor
;;   post is text after cursor

(define ED1 (make-editor ""    ""))  
(define ED2 (make-editor "abc" "d")) 
(define ED3 (make-editor "abcd"  ""))   
(define ED4 (make-editor ""    "abcd")) 

(@dd-template-rules compound) ;; 2 fields

(define (fn-for-editor e)
  (... (editor-pre e) 
       (editor-post e)))


;; =================
;; Functions:

(@htdf run-editor)
(@signature String -> Editor)
;; run an editor, with pre as the initial text preceding the cursor

(@template-origin htdw-main)
(define (run-editor pre)
  (big-bang (make-editor pre "")
    (to-draw render)        ; Editor -> Image
    (on-key  handle-key)))  ; Editor KeyEvent -> Editor


(@htdf render)
(@signature Editor -> Image)
;; place text with cursor at left, middle edge of MTS
(check-expect (render (make-editor "a" "bc"))
              (overlay/align "left"
                             "middle"
                             (beside (text "a" TEXT-SIZE TEXT-COLOR)
                                     CURSOR
                                     (text "bc" TEXT-SIZE TEXT-COLOR))
                             MTS))

; (define (render e) MTS) ; stub

(@template-origin Editor)

(@template
 (define (render e)
   (... (editor-pre e)
        (editor-post e))))

(define (render e)
  (overlay/align "left"
                 "middle"
                 (beside (text (editor-pre e) TEXT-SIZE TEXT-COLOR)
                         CURSOR
                         (text (editor-post e) TEXT-SIZE TEXT-COLOR))
                 MTS))


(@htdf handle-key)
(@signature Editor KeyEvent -> Editor)
;; call appropriate function for each keyboard command
(check-expect (handle-key (make-editor "a" "b") "left")
              (cursor-left (make-editor "a" "b")))
(check-expect (handle-key (make-editor "a" "b") "right")
              (cursor-right (make-editor "a" "b")))
(check-expect (handle-key (make-editor "a" "b") "\b")
              (backspace (make-editor "a" "b")))
(check-expect (handle-key (make-editor "a" "b") "x")
              (insert (make-editor "a" "b") "x"))
(check-expect (handle-key (make-editor "ab" "") "shift")
              (make-editor "ab" ""))

;(define (handle-key e key) e)

(@template-origin KeyEvent)

(@template 
 (define (handle-key e ke)
   (cond [(key=? ke " ") (... (editor-pre e) (editor-post e))]
         [else e])))

(define (handle-key e key)
  (cond [(key=? key "left")        (cursor-left e)]
        [(key=? key "right")       (cursor-right e)] 
        [(key=? key "\b")          (backspace e)] 
        
        [(= (string-length key) 1) (insert e key)] ;note, at this point key
        ;                                          ;is a string 1 long, or
        ;                                          ;what we call 1String
         
        [else e]))



(@htdf cursor-left)
(@signature Editor -> Editor)
;; moves the cursor left by 1
(check-expect (cursor-left  (make-editor ""   ""))   (make-editor ""   ""))
(check-expect (cursor-left  (make-editor ""   "ab")) (make-editor ""   "ab"))
(check-expect (cursor-left  (make-editor "ab" ""))   (make-editor "a"  "b"))

;(define (cursor-left e) e)

(@template-origin Editor)

(@template
 (define (cursor-left e)
   (... (editor-pre e)
        (editor-post e))))

(define (cursor-left e)
  (if (string=? (editor-pre e) "")
      e
      (make-editor (string-butlast (editor-pre e))
                   (string-append (string-last (editor-pre e))
                                  (editor-post e)))))


(@problem 2)
(@htdf cursor-right)
(@signature Editor -> Editor)
;; move the cursor right by 1
(check-expect (cursor-right  (make-editor ""   ""))   (make-editor ""   ""))
(check-expect (cursor-right  (make-editor ""   "ab")) (make-editor "a"   "b"))
(check-expect (cursor-right  (make-editor "ab" ""))   (make-editor "ab"  ""))
(check-expect (cursor-right  (make-editor "a" "b"))   (make-editor "ab"  ""))

; (define (cursor-right e) e) ; stub

(@template-origin Editor)

(@template
 (define (cursor-right e)
   (... (editor-pre e)
        (editor-post e))))

(define (cursor-right e)
  (if (string=? (editor-post e) "")
      e
      (make-editor (string-append (editor-pre e) (string-first (editor-post e)))
                   (string-butfirst (editor-post e)))))


(@htdf string-first)
(@signature String -> String)
;; produce the last character of a string
(check-expect (string-first "abcd") "a")

; (define (string-first str) "") ; stub

(@template-origin String)
(@template (define (string-first str) (... str)))

(define (string-first str)
  (substring str 0 1))


(@htdf string-butfirst)
(@signature String -> String)
;; produce all but the last character of a string
(check-expect (string-butfirst "abcd") "bcd")

; (define (string-butfirst str) "") ; stub

(@template-origin String)
(@template (define (string-butfirst str) (... str)))

(define (string-butfirst str)
  (substring str 1 (string-length str)))














(@problem 3)
(@htdf backspace)
(@signature Editor -> Editor)
;; delete character before cursor
(check-expect (backspace (make-editor "" "")) (make-editor "" ""))
(check-expect (backspace (make-editor "abcd" "")) (make-editor "abc" ""))
(check-expect (backspace (make-editor "" "abcd")) (make-editor "" "abcd"))
(check-expect (backspace (make-editor "ab" "cd")) (make-editor "a" "cd"))

; (define (backspace e) e) ; stub

(@template-origin Editor)
(@template
 (define (backspace e)
   (... (editor-pre e)
        (editor-post e))))

(define (backspace e)
  (if (string=? (editor-pre e) "")
      e
      (make-editor (string-butlast (editor-pre e))
                   (editor-post e))))














(@problem 4)
(@htdf insert)
(@signature Editor 1String -> Editor)
;; insert s into e at cursor
(check-expect (insert (make-editor "" "") "a") (make-editor "a" ""))
(check-expect (insert (make-editor "a" "cd") "b") (make-editor "ab" "cd"))
(check-expect (insert (make-editor "abc" "") "d") (make-editor "abcd" ""))

; (define (insert e s) e) ; stub

(@template-origin Editor)
(@template
 (define (insert e s)
   (... (editor-pre e)
        (editor-post e)
        s)))

(define (insert e s)
   (make-editor (string-append (editor-pre e) s)
        (editor-post e)))







(@htdf string-butlast)
(@signature String -> String)
;; produce all but the last character of a string
(check-expect (string-butlast "abcd") "abc")

;(define (string-butlast str) "")

(@template-origin String)
(define (string-butlast str)
  (substring str 0 (sub1 (string-length str))))


(@htdf string-last)
(@signature String -> String)
;; produce the last character of a string
(check-expect (string-last    "abcd") "d")

;(define (string-last str) "")

(@template-origin String)
(define (string-last str)
  (substring str (sub1 (string-length str))))
