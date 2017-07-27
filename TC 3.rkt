#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require 2htdp/batch-io)

(struct State (money anim costs-mex costs-mos amount-mex amount-mos time) #:transparent)

;generate file
(define file-exists (file-exists? "Kapital"))
(when (not file-exists)
        (write-file "Kapital" "0"))

(define file-exists-costs-mex (file-exists? "costs-mex"))
(when (not file-exists-costs-mex)
        (write-file "costs-mex" "100"))

(define file-exists-costs-mos (file-exists? "costs-mos"))
(when (not file-exists-costs-mos)
        (write-file "costs-mos" "100"))

(define file-exists-amount-mex (file-exists? "amount-mex"))
(when (not file-exists-amount-mex)
        (write-file "amount-mex" "0"))

(define file-exists-amount-mos (file-exists? "amount-mos"))
(when (not file-exists-amount-mos)
        (write-file "amount-mos" "0"))

;defines
(define start-counter (string->number (read-file "Kapital")))
(define BACKGROUND (bitmap "background.png"))
(define (savegame p) (write-file "Kapital" (number->string p)))

(define costs-mex (string->number (read-file "costs-mex")))
(define (save-costs-mex p) (write-file "costs-mex" (number->string p)))

(define costs-mos (string->number (read-file "costs-mos")))
(define (save-costs-mos p) (write-file "costs-mos" (number->string p)))

(define amount-mex (string->number (read-file "amount-mex")))
(define (save-amount-mex p) (write-file "amount-mex" (number->string p)))

(define amount-mos (string->number (read-file "amount-mos")))
(define (save-amount-mos p) (write-file "amount-mos" (number->string p)))

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
(define (tick state)
    (define anim (State-anim state))
    (define time (State-time state))
    (define now (current-milliseconds))
    (define money (State-money state))
    (define update-money? (> (- now time) 1000))
    (define money-delta (+ (State-amount-mos state) (State-amount-mex state)))
    (define money-new (if update-money? (+ money money-delta) money))
    (define time-new (if update-money? now time))
    (define anim-new (modulo (+ anim 1) 4))
    (struct-copy State state (anim anim-new)
        (money money-new) (time time-new)))

;layout
; State -> Image
(define (draw_ampel state)
    (match-define (State money anim costs-mex costs-mos amount-mex amount-mos time) state)
    (define torender (list 
      (text (string-append (number->string money)"$") 40 "black") 
            (draw_trump state) 
            (text (string-append (number->string costs-mex)"$" "  "(number->string amount-mex)"x") 15 "black")
            (text (string-append (number->string costs-mos)"$" "  "(number->string amount-mos)"x") 15 "black")                                    
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
    (match-define (State money anim costs-mex costs-mos amount-mex amount-mos time) state)
    (define overlap-trump? (overlaps mouse-x mouse-y 1 1 520 235 280 420))
    (define overlap-mex? (overlaps-mex mouse-x mouse-y 1 1 229 307 225 225))
    (define overlap-mos? (overlaps-mos mouse-x mouse-y 1 1 912 307 225 225))
    (define button-down? (equal? mouse-event "button-down"))

    (if (not button-down?)
        state
        (cond (overlap-trump?
          (begin 
            (savegame (+ money 1)) 
            (struct-copy State state (money (+ money 1)))))

          (overlap-mex?          
            (if (>= money costs-mex)
                (begin
                    (savegame (- money costs-mex))
                    (save-costs-mex (* costs-mex 2))
                    (save-amount-mex (+ amount-mex 1))
                    (struct-copy State state (money (- money costs-mex))
                                            (costs-mex (* costs-mex 2))
                                            (amount-mex (+ amount-mex 1))))
                state))

          (overlap-mos?         
            (if (>= money costs-mos)
                (begin
                    (savegame (- money costs-mos))
                    (save-costs-mos (* costs-mos 2))
                    (save-amount-mos (+ amount-mos 1))
                    (struct-copy State state (money (- money costs-mos))
                                            (costs-mos (* costs-mos 2))
                                            (amount-mos (+ amount-mos 1))))
                state))
          (else state))))
    
; (struct State (money anim costs-mex costs-mos amount-mex amount-mos time) #:transparent)

;Output
(big-bang (State  start-counter 0 costs-mex costs-mos amount-mex amount-mos (current-milliseconds))
    (to-draw draw_ampel)
    (on-mouse mouse-input)
    (on-tick tick 0.1)
    (on-key handle-keys)
    (display-mode 'fullscreen)) 