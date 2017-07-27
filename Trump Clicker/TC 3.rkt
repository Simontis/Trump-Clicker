#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require 2htdp/batch-io)

(struct State (money anim costs-mex costs-mos amount-mex amount-mos amount-PowerUp costs-PowerUp time) #:transparent)

;generate file
(define file-exists (file-exists? "Kapital"))
(when (not file-exists)
        (write-file "Kapital" "0"))

(define file-exists-PowerUp (file-exists? "amount-PowerUp"))
(when (not file-exists-PowerUp)
        (write-file "amount-PowerUp" "1"))

(define file-exists-costs-PowerUp (file-exists? "costs-PowerUp"))
(when (not file-exists-costs-PowerUp)
        (write-file "costs-PowerUp" "100"))

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

(define amount-PowerUp (string->number (read-file "amount-PowerUp")))
(define (save-amount-PowerUp p) (write-file "amount-PowerUp" (number->string p)))

(define costs-PowerUp (string->number (read-file "costs-PowerUp")))
(define (save-costs-PowerUp p) (write-file "costs-PowerUp" (number->string p)))

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
(define PowerUp (bitmap "PowerUp.png"))
(define fist-up (bitmap "godfist.png"))
(define john-cena-button (bitmap "john-cena-button.png"))

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

  (define (overlaps-PowerUp ax ay aw ah ex ey ew eh)
  (define al ax)
  (define ar (+ ax aw))
  (define at ay)
  (define ab (+ at ah))
  (define bl ex)
  (define br (+ ex ew))
  (define bt ey)
  (define bb (+ bt eh))
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
    (define update-money? (> (- now time) 900))
    (define money-delta (* (State-amount-PowerUp state)(+ (State-amount-mos state) (State-amount-mex state))))
    (define money-new (if update-money? (+ money money-delta) money))
    (define time-new (if update-money? now time))
    (define anim-new (modulo (+ anim 1) 4))
    (struct-copy State state (anim anim-new)
        (money money-new) (time time-new)))

;layout
; State -> Image
(define (draw_ampel state)
    (match-define (State money anim costs-mex costs-mos amount-mex amount-mos amount-PowerUp costs-PowerUp time) state)
    (define torender (list 
      (text/font (string-append (number->string money)"$") 40 "black" #f 'swiss 'normal 'bold #f) 
      (text/font (string-append (number->string costs-PowerUp)"$" "  " (number->string amount-PowerUp)"x") 25 "black" #f 'swiss 'normal 'bold #f)
            PowerUp
            (draw_trump state)
            (text/font (string-append (number->string costs-mex)"$" "  "(number->string amount-mex)"x") 15 "black" #f 'swiss 'normal 'bold #f)
            (text/font (string-append (number->string costs-mos)"$" "  "(number->string amount-mos)"x") 15 "black" #f 'swiss 'normal 'bold #f)                                    
            mexikaner-button
            muslim-button
            ))
    (place-images torender (list 
            (make-posn 683 95)
            (make-posn 240 655)
            (make-posn 250 580) 
            (make-posn 683 440)
            (make-posn 1024 650)
            (make-posn 1024 400) 
            (make-posn 1024 600) 
            (make-posn 1024 335)
            ) BACKGROUND))

;on-click money
; State Number Number String -> State
(define (mouse-input state mouse-x mouse-y mouse-event)
    (match-define (State money anim costs-mex costs-mos amount-mex amount-mos amount-PowerUp costs-PowerUp time) state)
    (define overlap-trump? (overlaps mouse-x mouse-y 1 1 520 200 280 420))
    (define overlap-mex? (overlaps-mex mouse-x mouse-y 1 1 912 487 225 225))
    (define overlap-mos? (overlaps-mos mouse-x mouse-y 1 1 912 222 225 225))
    (define overlap-PowerUp? (overlaps-PowerUp mouse-x mouse-y 1 1 128 543 400 225))
    
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
                    (save-costs-mex (* costs-mex 1.5))
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

          (overlap-PowerUp?         
            (if (>= money costs-PowerUp)
                (begin
                    (savegame (- money costs-PowerUp))
                    (save-costs-PowerUp (* costs-PowerUp 2))
                    (save-amount-PowerUp (+ amount-PowerUp 1))
                    (struct-copy State state (costs-PowerUp (* costs-PowerUp 2))
                                             (amount-PowerUp (+ amount-PowerUp 1))
                                             (money (- money costs-PowerUp))
                ))
                state))
          (else state))))
    
; (struct State (money anim costs-mex costs-mos amount-mex amount-mos amount-PowerUp costs-PowerUp time) #:transparent)

;Output
(big-bang (State  start-counter 0 costs-mex costs-mos amount-mex amount-mos amount-PowerUp costs-PowerUp (current-milliseconds))
    (to-draw draw_ampel)
    (on-mouse mouse-input)
    (on-tick tick 0.1)
    (on-key handle-keys)
    (display-mode 'fullscreen)) 