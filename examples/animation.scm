(use-modules (2d animation)
             (2d game)
             (2d sprite)
             (2d tileset)
             (2d vector2)
             (2d window))

(load "common.scm")

(define (make-demo-animation)
  "Load a texture, split it into 64x64 tiles, and build an animated
sprite out of it."
  (let* ((tiles (load-tileset "images/princess.png" 64 64))
         (frames (vector (tileset-ref tiles 19)
                         (tileset-ref tiles 20)
                         (tileset-ref tiles 21)
                         (tileset-ref tiles 22)
                         (tileset-ref tiles 23)
                         (tileset-ref tiles 24)
                         (tileset-ref tiles 25)
                         (tileset-ref tiles 26))))
    (make-animation frames 6 #t)))

(define sprite (make-sprite (make-demo-animation)
                            #:position (vector2 320 240)))

(add-hook! draw-hook (lambda (dt alpha) (draw-sprite sprite)))

(with-window (make-window #:title "Animation")
  (run-game-loop))
