(use-modules (srfi srfi-1)
             (2d color)
             (2d game)
             (2d keyboard)
             (2d signals)
             (2d sprite)
             (2d texture)
             (2d time)
             (2d vector2)
             (2d window))

(with-window (make-window #:title "FRP is cool"
                          #:resolution (vector2 640 480)
                          #:fullscreen? #f)
  (define speed 4)

  ;; Move when arrow keys are pressed.
  (define move
    (signal-fold (lambda (direction position)
                   (v+ (vscale direction speed) position))
                 (vector2 320 240)
                 (time-every key-arrows)))

  (define ghost-texture (load-texture "images/ghost.png"))

  (define ghost (make-sprite ghost-texture #:position move))

  (define follower-count 8)

  (define followers
    (list-tabulate
     follower-count
     (lambda (i)
       (make-sprite ghost-texture
                    ;; Follow ghost with some delay.
                    #:position (time-delay (* (- follower-count i) 10) move)
                    ;; Make each ghost more translucent than the last.
                    #:color (let ((alpha (/ (1+ i)
                                            (* 2 follower-count))))
                              (make-color 1 1 1 alpha))))))

  (define quit-on-esc
    (signal-lift (lambda (down?)
                   (when down?
                     (quit-game)))
                 (key-down? 'escape)))

  (define (draw)
    (for-each draw-sprite followers)
    (draw-sprite ghost))

  (run-game #:draw draw))
