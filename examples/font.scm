(use-modules (srfi srfi-9)
             (figl gl)
             (2d color)
             (2d font)
             (2d game)
             (2d vector2)
             (2d window))

(load "common.scm")

(define textbox
  (make-textbox (load-font "fonts/Boxy-Bold.ttf" 48)
                "The quick brown fox jumped over the lazy dog."
                (vector2 240 160)
                white
                'left
                200))

(add-hook! draw-hook (lambda (dt alpha) (draw-textbox textbox)))

(with-window (make-window #:title "Fonts")
  (run-game-loop))
