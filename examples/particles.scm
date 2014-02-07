;; load the SDL module and some useful srfi's
(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (2d agenda)
             (2d game)
             (2d sprite)
             (2d texture)
             (2d vector2)
             (2d window))

(load "common.scm")

(set! *random-state* (random-state-from-platform))

(define window-width 640)
(define window-height 480)

;;;
;;; Particles
;;;

(define-record-type <particle>
  (make-particle sprite position velocity)
  particle?
  (sprite particle-sprite)
  (position particle-position set-particle-position!)
  (velocity particle-velocity set-particle-velocity!))

(define (update-particle! particle)
  (set-particle-position! particle
                          (v+ (particle-position particle)
                              (particle-velocity particle))))

(define (generate-particles n)
  (let ((particle-image (load-texture "images/bullet.png")))
    (list-tabulate n (lambda (n)
                       (make-particle (make-sprite particle-image)
                                      (vector2 (random window-width)
                                               (random window-height))
                                      (vector2 (* (random:normal) 1)
                                               (* (random:normal) 1)))))))

(define particle-count 500)
(define batch (make-sprite-batch (* particle-count 4)))
(define background (load-sprite "images/stars.png"
                                #:anchor null-vector2))
(define particles (generate-particles particle-count))

(define (draw-particles particles)
  (with-sprite-batch batch
    (for-each
     (lambda (p)
       (let* ((sprite (particle-sprite p)))
         (set-sprite-position! sprite (particle-position p))
         (draw-sprite sprite)))
     particles)))

(define (draw dt alpha)
  (draw-sprite background)
  (draw-particles particles))

(define (update)
  (for-each update-particle! particles))

(schedule-each game-agenda update)
(add-hook! draw-hook draw)

(with-window (make-window #:title "Particles"
                          #:resolution (vector2 window-width
                                                window-height))
  (run-game-loop))
