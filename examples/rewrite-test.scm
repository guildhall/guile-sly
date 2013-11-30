(use-modules (2d game)
             (2d signals)
             (2d sprite)
             (2d vector2))

(define sprite
  (delay (load-sprite "images/ghost.png"
                      #:position (vector2 320 240))))

(define quit-on-esc
  (signal-lift (lambda (down?)
                 (when down?
                   (quit-game)))
               (key-is-down 'escape)))

(define sprite-position
  (signal-lift (lambda (pos)
                 (when (game-running?)
                   (set-sprite-position! (force sprite) pos)))
               mouse-position))

(define (draw)
  (draw-sprite (force sprite)))

;; TODO: make the quit condition a signal

(define demo
  (make-game
   #:title "Simple Demo"
   #:draw draw))

(run-game demo)
