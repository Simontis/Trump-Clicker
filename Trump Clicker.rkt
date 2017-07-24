#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require rsound)

(struct Pos (x y))

(define WIDTH 1366)
(define HEIGHT 768)

(define BACKGROUND
  (rectangle WIDTH HEIGHT "solid" "transparent"))

  (define (draw state)
  (define score ( text "score" 14 "black"))
    (overlay (rectangle 100 100 "solid" "green") BACKGROUND)score)

(define (main state)
  (big-bang state
            [to-draw draw]
            [display-mode 'fullscreen]))

(main 0)


