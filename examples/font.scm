(use-modules (srfi srfi-9)
             (figl gl)
             (2d agenda)
             (2d fps)
             (2d color)
             (2d font)
             (2d game)
             (2d mouse)
             (2d signals)
             (2d vector2)
             (2d window))

(load "common.scm")

(define font (load-default-font 18))
(define position (vector2 320 240))
(define text "The quick brown fox jumped over the lazy dog.")
(define label (make-label font text position #:anchor 'center))

(define fps-label-position (vector2 0 0))
(define (make-fps-label)
  (make-label font (format #f "FPS: ~d" (fps)) fps-label-position))
(define fps-label (make-fps-label))

(define mouse-label-position (vector2 0 20))
(define mouse-label
  (signal-map (lambda (p)
                (let ((text (format #f "Mouse: (~d, ~d)" (vx p) (vy p))))
                  (make-label font text mouse-label-position)))
              mouse-position))

(define gc-label-position (vector2 0 40))
(define gc-counter (make-root-signal 0))
(define gc-label
  (signal-map (lambda (counter)
                (let ((text (format #f "GCs: ~d" counter)))
                  (make-label font text gc-label-position)))
              gc-counter))

(add-hook! after-gc-hook
           (lambda ()
             (signal-set! gc-counter (1+ (signal-ref gc-counter)))))

(schedule-interval game-agenda
                   (lambda ()
                     (set! fps-label (make-fps-label)))
                   60)

(add-hook! draw-hook (lambda (dt alpha)
                       (draw-label label)
                       (draw-label fps-label)
                       (draw-label (signal-ref mouse-label))
                       (draw-label (signal-ref gc-label))))

(with-window (make-window #:title "Fonts")
  (run-game-loop))
