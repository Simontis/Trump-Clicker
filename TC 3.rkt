#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require 2htdp/batch-io)

(struct State (money anim) #:transparent)

(define file-exists (file-exists? "Kapital"))
(if (not file-exists)
        (write-file "Kapital" "0")
        (read-file "Kapital"))

;defines
(define start-counter (string->number (read-file "Kapital")))
(define BACKGROUND (empty-scene 1366 768))
(define (savegame p) (write-file "Kapital" (number->string p)))

(define trump1 (bitmap "trump1.png"))
(define trump2 (bitmap "trump2.png"))
(define trump3 (bitmap "trump3.png"))
(define posns (list (make-posn 683 400)))


(define (draw_trump p)
    (define current (State-anim p))
    (cond
        ((= current 0)
            (place-images (list trump1) posns BACKGROUND))
        ((= current 1)
            (place-images (list trump2) posns BACKGROUND))
        ((= current 2) 
            (place-images (list trump3) posns BACKGROUND))
        ((= current 3) 
            (place-images (list trump2) posns BACKGROUND))))

(define (tick p)
    (define v (State-anim p))
        (if (= v 3) (struct-copy State p (anim 0)) 
        (struct-copy State p (anim (+ v 1)))))

;layout
; State -> Image
(define (draw_ampel state)
    (match-define (State money anim) state)
    (define torender (list 
      (text (string-append (number->string money)"$") 40 "black") 
      (draw_trump state)))
    (place-images torender (list (make-posn 683 100) (make-posn 683 400)) BACKGROUND))

;on-click money
; State Number Number String -> State
(define (mouse-input state mouse-x mouse-y mouse-event)
    (match-define (State money anim) state)
    (if (equal? mouse-event "button-down")
          (begin 
            (savegame (+ money 1)) 
            (struct-copy State state (money (+ money 1)))) 
          state))
    

;Output
(big-bang (State start-counter 0)
    (to-draw draw_ampel)
    (on-mouse mouse-input)
    (on-tick tick)) 