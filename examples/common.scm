(use-modules (2d agenda)
             (2d fps)
             (2d game)
             (2d keyboard)
             (2d repl)
             (2d signal)
             (2d window))

(add-hook! key-press-hook (lambda (key unicode)
                            (when (eq? key 'escape)
                              (quit-game))))

(add-hook! window-close-hook quit-game)

(schedule-interval game-agenda
                   (lambda ()
                     (format #t "FPS: ~d\n" (signal-ref fps)))
                   60)
