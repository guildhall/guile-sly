(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (2d color)
             (2d game)
             (2d keyboard)
             (2d math)
             (2d rect)
             (2d signals)
             (2d sprite)
             (2d texture)
             (2d time)
             (2d vector2)
             (2d window))

(define-record-type <paddle>
  (%make-paddle y)
  paddle?
  (y paddle-y)
  (speed paddle-speed))

(define-record-type <ball>
  (make-ball position velocity)
  ball?
  (position ball-position)
  (velocity ball-velocity))

(define window-width 640)
(define window-height 480)
(define half-width (/ window-width 2))
(define half-height (/ window-height 2))

(with-window (make-window #:title "FRP is cool"
                          #:resolution (vector2 window-width
                                                window-height)
                          #:fullscreen? #f)
  (define paddle-texture (load-texture "images/paddleBlue.png"))
  (define ball-texture (load-texture "images/ballBlue.png"))

  (define paddle-speed 5)
  (define ball-speed 4)
  (define ball-hitbox (make-rect -8 -8 16 16))

  (define (move-paddle directions)
    (signal-fold (lambda (new old)
                   (clamp 0 window-height (+ (* new paddle-speed) old)))
                 half-height
                 (signal-lift vy (time-every directions))))

  (define player1-paddle (move-paddle key-wasd))
  (define player2-paddle (move-paddle key-arrows))

  (define (wall-bounce position)
    (let ((rect (rect-move ball-hitbox position)))
      (cond ((> (rect-bottom rect) window-height)
             (vector2 (vx position)
                      (- (vy position))))
            (else
             position))))

  (define ball (signal-fold (lambda (direction position)
                              (wall-bounce (v+ (vscale direction ball-speed) position)))
                            (vector2 half-width half-height)
                            (time-every (make-signal #:init (vector2 1 1)))))

  (define quit-on-esc
    (signal-lift (lambda (down?)
                   (when down?
                     (quit-game)))
                 (key-down? 'escape)))

  (define draw
    (let ((paddle-sprite (make-sprite paddle-texture))
          (ball-sprite (make-sprite ball-texture)))
      (lambda ()
        (set-sprite-position! paddle-sprite (vector2 0 (signal-ref player1-paddle)))
        (draw-sprite paddle-sprite)
        (set-sprite-position! paddle-sprite (vector2 window-width (signal-ref player2-paddle)))
        (draw-sprite paddle-sprite)
        (set-sprite-position! ball-sprite ball)
        (draw-sprite ball-sprite))))

  (run-game #:draw draw))
