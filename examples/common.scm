(use-modules (2d keyboard)
             (2d repl server))

(spawn-server)

(add-hook! key-press-hook (lambda (key unicode)
                            (when (eq? key 'escape)
                              (quit-game))))
