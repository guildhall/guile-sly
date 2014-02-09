(use-modules (2d game)
             (2d shader)
             (2d sprite)
             (2d vector2)
             (2d window))

(load "common.scm")

(define vertex-shader
  (load-vertex-shader "shaders/vertex-shader.glsl"))
(define fragment-shader
  (load-fragment-shader "shaders/fragment-shader.glsl"))
(define program (make-shader-program vertex-shader fragment-shader))

(define window-width 800)
(define window-height 600)
(define sprite
  (load-sprite "images/p1_front.png"
               #:position (vector2 (/ window-width 2)
                                   (/ window-height 2))))

(define (draw dt alpha)
  (with-shader-program program
    ;; Shake the sprite around a bit.
    (uniforms ((angle (/ (random:uniform) 100)))
      (draw-sprite sprite))))

(add-hook! draw-hook draw)

(with-window (make-window #:title "Shaders"
                          #:resolution (vector2 window-width
                                                window-height))
  (run-game-loop))
