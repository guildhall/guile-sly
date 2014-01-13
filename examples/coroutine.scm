(use-modules (2d agenda)
             (2d coroutine)
             (2d game)
             (2d sprite)
             (2d vector2)
             (2d window))

(load "common.scm")

(define window-width 640)
(define window-height 480)

(define sprite (load-sprite "images/p1_front.png"
                            #:position (vector2 320 240)))

;; Simple script that moves the sprite to a random location every
;; second.
(codefine (script)
  (set-sprite-position!
   sprite
   (vector2 (random window-width)
            (random window-height)))
  (wait 15)
  (set-sprite-rotation! sprite (random 360))
  (wait 15)
  (script))

(schedule-next script)

(add-hook! draw-hook (lambda (dt alpha) (draw-sprite sprite)))

(with-window (make-window #:title "Coroutines"
                          #:resolution (vector2 window-width
                                                window-height))
  (run-game-loop))
