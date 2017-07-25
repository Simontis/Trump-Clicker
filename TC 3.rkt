#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require 2htdp/batch-io)

(struct State (money) #:transparent)

(define file-exists (file-exists? "Kapital"))
(if (not file-exists)
        (write-file "Kapital" "0")
        (read-file "Kapital"))

;defines
(define start-counter (string->number (read-file "Kapital")))
(define BACKGROUND (empty-scene 1366 768))
(define (savegame p) (write-file "Kapital" (number->string p)))

;layout
; State -> Image
(define (draw_ampel state)
    (match-define (State money) state)
    (define torender (list 
      (text (string-append (number->string money)"$") 40 "black") 
      (rectangle 100 100 "solid" "green")))
    (place-images torender (list (make-posn 683 100) (make-posn 683 400)) BACKGROUND))

;on-click money
; State Number Number String -> State
(define (mouse-input state mouse-x mouse-y mouse-event)
    (match-define (State money) state)
    (if (equal? mouse-event "button-down")
          (begin 
            (savegame (+ money 1)) 
            (State (+ money 1))) 
          state))
    

;Output
(big-bang (State start-counter)
    (to-draw draw_ampel)
    (on-mouse mouse-input))

  ;(savegame p)  