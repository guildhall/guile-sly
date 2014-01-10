(use-modules (2d game)
             (2d sprite)
             (2d vector2)
             (2d window))

(load "common.scm")

(define sprite (load-sprite "images/p1_front.png"
                            #:position (vector2 320 240)))

(add-hook! draw-hook (lambda (dt alpha) (draw-sprite sprite)))

(with-window (make-window #:title "Simple Sprite Demo")
  (run-game-loop))
