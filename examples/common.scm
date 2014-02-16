(use-modules (2d agenda)
             (2d fps)
             (2d game)
             (2d keyboard)
             (2d window)
             (2d repl server))

(spawn-server)

(add-hook! key-press-hook (lambda (key unicode)
                            (when (eq? key 'escape)
                              (quit-game))))

(add-hook! window-close-hook quit-game)

(schedule-interval game-agenda
                   (lambda ()
                     (display (fps))
                     (newline))
                   60)
