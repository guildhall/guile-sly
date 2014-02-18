(use-modules (2d agenda)
             (2d fps)
             (2d color)
             (2d font)
             (2d game)
             (2d mouse)
             (2d signal)
             (2d vector2)
             (2d window))

(load "common.scm")

(define font (load-default-font 18))
(define label (make-label font "The quick brown fox jumped over the lazy dog."
                          (vector2 320 240) #:anchor 'center))

(define-signal fps-label
  (signal-map (lambda (fps)
                (let ((text (format #f "FPS: ~d" fps)))
                  (make-label font text (vector2 0 0))))
              (signal-sample game-agenda 60 fps)))

(define-signal mouse-label
  (signal-map (lambda (p)
                (let ((text (format #f "Mouse: (~d, ~d)" (vx p) (vy p))))
                  (make-label font text (vector2 0 20))))
              (signal-throttle game-agenda 5 mouse-position)))

(define-signal gc-counter (make-signal 0))
(define-signal gc-label
  (signal-map (lambda (counter)
                (let ((text (format #f "GCs: ~d" counter)))
                  (make-label font text (vector2 0 40))))
              gc-counter))

(add-hook! after-gc-hook
           (lambda ()
             (schedule game-agenda
                       (lambda ()
                         (signal-set! gc-counter
                                      (1+ (signal-ref gc-counter)))))))

(add-hook! draw-hook (lambda (dt alpha)
                       (draw-label label)
                       (draw-label (signal-ref fps-label))
                       (draw-label (signal-ref mouse-label))
                       (draw-label (signal-ref gc-label))))

(with-window (make-window #:title "Fonts")
  (run-game-loop))
