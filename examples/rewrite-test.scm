(use-modules (srfi srfi-1)
             (2d color)
             (2d game)
             (2d signals)
             (2d sprite)
             (2d texture)
             (2d time)
             (2d vector2)
             (2d window))

(with-window (make-window #:title "FRP is cool"
                          #:resolution (vector2 640 480)
                          #:fullscreen? #f)

  (define move
    (make-signal
     #:init (vector2 320 240)
     #:transformer (lambda (value old from)
                     (if (eq? from arrows)
                         old
                         (v+ (vscale (signal-ref arrows) 5)
                             old)))
     #:connectors (list arrows (time-every))))

  (define ghost-texture (load-texture "images/ghost.png"))

  (define sprite
    (make-sprite ghost-texture
                 #:position move))

  (define follower-count 8)
  (define followers
    (list-tabulate
     follower-count
     (lambda (i)
       (make-sprite ghost-texture
                    #:position (time-delay (* (- follower-count i) 1) move)
                    #:color (make-color 1 1 1 (/ (1+ i) 16))))))

  ;; Temporary hack. There shouldn't be side effects like this in
  ;; signals.
  (define quit-on-esc
    (signal-lift (lambda (down?)
                   (when down?
                     (quit-game)))
                 (key-down? 'escape)))

  (define (draw)
    (for-each draw-sprite followers)
    (draw-sprite sprite))

  (run-game #:draw draw))
