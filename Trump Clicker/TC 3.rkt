#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require 2htdp/batch-io)

(struct State (money anim) #:transparent)

;generate file
(define file-exists (file-exists? "Kapital"))
(if (not file-exists)
        (write-file "Kapital" "0")
        (read-file "Kapital"))

;defines
(define start-counter (string->number (read-file "Kapital")))
(define BACKGROUND (bitmap "background.png"))
(define (savegame p) (write-file "Kapital" (number->string p)))

;define trump animation
(define trump1 (bitmap "trump1.png"))
(define trump2 (bitmap "trump2.png"))
(define trump3 (bitmap "trump3.png"))
(define posns (list (make-posn 683 400)))

;define shop buttons
(define mexikaner-button (bitmap "mexikaner-button.png"))
(define muslim-button (bitmap "muslim-button.png"))

;close game
(define (handle-keys state key)
    (if (key=? key "escape") (exit) state))

;define hitbox
(define (overlaps ax ay aw ah bx by bw bh)
  (define al ax)
  (define ar (+ ax aw))
  (define at ay)
  (define ab (+ at ah))
  (define bl bx)
  (define br (+ bx bw))
  (define bt by)
  (define bb (+ bt bh))
  (and (> br al) (< bl ar) (> bb at) (< bt ab)))

  (define (overlaps-mex ax ay aw ah cx cy cw ch)
  (define al ax)
  (define ar (+ ax aw))
  (define at ay)
  (define ab (+ at ah))
  (define bl cx)
  (define br (+ cx cw))
  (define bt cy)
  (define bb (+ bt ch))
  (and (> br al) (< bl ar) (> bb at) (< bt ab)))

  (define (overlaps-mos ax ay aw ah dx dy dw dh)
  (define al ax)
  (define ar (+ ax aw))
  (define at ay)
  (define ab (+ at ah))
  (define bl dx)
  (define br (+ dx dw))
  (define bt dy)
  (define bb (+ bt dh))
  (and (> br al) (< bl ar) (> bb at) (< bt ab)))


;define draw trump
(define (draw_trump p)
    (define current (State-anim p))
    (cond
        ((= current 0)
            trump1)
        ((= current 1)
            trump2)
        ((= current 2) 
            trump3)
        ((= current 3) 
            trump2)))

;animation trump on-tick
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
            (draw_trump state)
            (text "100$" 15 "black")
            (text "100$" 15 "black")                                      
            mexikaner-button
            muslim-button))
    (place-images torender (list 
            (make-posn 683 95) 
            (make-posn 683 475)
            (make-posn 341.5 470)
            (make-posn 1024.5 485) 
            (make-posn 341.5 420) 
            (make-posn 1024.5 420)) BACKGROUND))

;on-click money
; State Number Number String -> State
(define (mouse-input state mouse-x mouse-y mouse-event)
    (match-define (State money anim) state)
    (if (equal? mouse-event "button-down")

    (cond ((overlaps mouse-x mouse-y 1 1 520 235 280 420)
          (begin 
            (savegame (+ money 1)) 
            (struct-copy State state (money (+ money 1)))))

          ((overlaps-mex mouse-x mouse-y 1 1 229 307 225 225)
          (begin
            (savegame (- money 100))
            (struct-copy State state (money (- money 100)))))

          ((overlaps-mos mouse-x mouse-y 1 1 912 307 225 225)
          (begin
            (savegame (- money 100))
            (struct-copy State state (money (- money 100)))))

          (money state))state))
    

;Output
(big-bang (State start-counter 0)
    (to-draw draw_ampel)
    (on-mouse mouse-input)
    (on-tick tick)
    (on-key handle-keys)
    (display-mode 'fullscreen)) 