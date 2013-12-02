;;; guile-2d
;;; Copyright (C) 2013 David Thompson <dthompson2@worcester.edu>
;;;
;;; Guile-2d is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Guile-2d is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Game data structure.
;;
;;; Code:

(define-module (2d game)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (figl gl)
  #:use-module (2d agenda)
  #:use-module (2d coroutine)
  #:use-module (2d game)
  #:use-module (2d mvars)
  #:use-module (2d repl server)
  #:use-module (2d repl repl)
  #:use-module (2d signals)
  #:use-module (2d vector2)
  #:export (run-game
            quit-game
            pause-game
            resume-game
            game-running?
            game-paused?
            window-size
            key-last-pressed
            key-down?
            key-directions
            key-arrows
            key-wasd
            mouse-position
            mouse-down?
            current-fps))

;;;
;;; Game Loop
;;;

(define (default-draw)
  #f)

(define (default-update)
  #f)

;; Possible states are:
;; * stopped
;; * running
;; * paused
(define %state 'stopped)
(define %draw #f)
(define %update #f)
;; TODO: Make this configurable
(define ticks-per-second 60)
(define tick-interval (floor (/ 1000 ticks-per-second)))

(define* (run-game #:optional #:key
                   (draw default-draw)
                   (update default-update))
  "Start the game loop."
  (set! %state 'running)
  (set! %draw draw)
  (set! %update update)
  (resume-game)
  (spawn-server)
  (game-loop (SDL:get-ticks) 0))

(define (draw dt)
  "Render a frame."
  (set-gl-matrix-mode (matrix-mode modelview))
  (gl-load-identity)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (%draw)
  (SDL:gl-swap-buffers)
  (accumulate-fps! dt))

(define (update accumulator)
  "Call the update callback. The update callback will be called as
many times as `tick-interval` can divide ACCUMULATOR. The return value
is the unused accumulator time."
  (if (>= accumulator tick-interval)
      (begin
        (handle-events)
        (update-agenda)
        (%update)
        (update (- accumulator tick-interval)))
      accumulator))

(define (update-and-render dt accumulator)
  (let ((remainder (update accumulator)))
    (run-repl)
    (draw dt)
    remainder))

(define (tick dt accumulator)
  "Advance the game by one frame."
  (if (game-paused?)
      (begin
        (run-repl)
        (SDL:delay tick-interval)
        accumulator)
      (catch #t
        (lambda ()
          (update-and-render dt accumulator))
        (lambda (key . args)
          (pause-game)
          accumulator)
        (lambda (key . args)
          (display-backtrace (make-stack #t)
                             (current-output-port))))))

(define (game-loop last-time accumulator)
  "Update game state, and render. LAST-TIME is the time in
milliseconds of the last iteration of the loop. ACCUMULATOR is the
time in milliseconds that has passed since the last game update."
  (when (game-running?)
    (let* ((current-time (SDL:get-ticks))
           (dt (- current-time last-time))
           (accumulator (+ accumulator dt)))
      (game-loop current-time (tick dt accumulator)))))

;;;
;;; State management
;;;

(define (game-running?)
  (or (eq? %state 'running)
      (eq? %state 'paused)))

(define (game-paused?)
  (eq? %state 'paused))

(define (pause-game)
  "Pauses the game loop. Useful when developing."
  (set! %state 'paused))

(define (resume-game)
  "Resumes the game loop."
  (when (game-paused?)
    (set! %state 'running)))

(define (quit-game)
  "Finish the current frame and terminate the game loop."
  (set! %state 'stopped))

;;;
;;; Event Handling
;;;

(define handle-events
  (let ((e (SDL:make-event)))
    (lambda ()
      "Handle all events in the SDL event queue."
      (while (SDL:poll-event e)
        (handle-event e)))))

;; Keyboard and mouse signals.
(define window-size (make-signal #:init (vector2 0 0)))
(define key-last-pressed (make-signal))
(define mouse-position (make-signal #:init (vector2 0 0)))
(define key-signals (make-hash-table))
(define mouse-signals (make-hash-table))

(define (signal-hash-ref hash key)
  (let ((signal (hashq-ref hash key)))
    (if (signal? signal)
        signal
        (let ((signal (make-signal)))
          (hashq-set! hash key signal)
          signal))))

(define (signal-hash-set! hash key value)
  (signal-set! (signal-hash-ref hash key) value))

(define (key-down? key)
  "Return a signal for KEY."
  (signal-hash-ref key-signals key))

(define (mouse-down? button)
  "Return a signal for BUTTON."
  (signal-hash-ref mouse-signals button))

(define (key-directions up down left right)
  (signal-lift4 (lambda (up? down? left? right?)
                  (let ((up (if up? -1 0))
                        (down (if down? 1 0))
                        (left (if left? -1 0))
                        (right (if right? 1 0)))
                    (vector2 (+ left right) (+ up down))))
                (key-down? up)
                (key-down? down)
                (key-down? left)
                (key-down? right)))

(define key-arrows (key-directions 'up 'down 'left 'right))
(define key-wasd (key-directions 'w 's 'a 'd))

(define (handle-event e)
  "Call the relevant callbacks for the event E."
  (case (SDL:event:type e)
    ((active)
     #f)
    ((video-resize)
     (signal-set! window-size (vector2 (SDL:event:resize:w e)
                                       (SDL:event:resize:h e))))
    ((quit)
     (quit-game))
    ((key-down)
     (let ((key (SDL:event:key:keysym:sym e)))
       (signal-hash-set! key-signals key #t)
       (signal-set! key-last-pressed key)))
    ((key-up)
     (signal-hash-set! key-signals (SDL:event:key:keysym:sym e) #f))
    ((mouse-motion)
     (signal-set! mouse-position
                  (vector2 (SDL:event:motion:x e)
                           (SDL:event:motion:y e))))
    ((mouse-button-down)
     (signal-hash-set! mouse-signals (SDL:event:button:button e) #t))
    ((mouse-button-up)
     (signal-hash-set! mouse-signals (SDL:event:button:button e) #f))))

;;;
;;; Frames Per Second
;;;

(define game-fps 0)

(define accumulate-fps!
  (let* ((elapsed-time 0)
         (fps 0))
    (lambda (dt)
      "Increment the frames-per-second counter. Resets to 0 every
second."
      (let ((new-time (+ elapsed-time dt))
            (new-fps (1+ fps)))
        (if (>= new-time 1000)
            (begin
              (set! game-fps new-fps)
              (set! fps 0)
              (set! elapsed-time 0))
            (begin
              (set! fps new-fps)
              (set! elapsed-time new-time)))))))

(define (current-fps)
  "Return the current FPS value."
  game-fps)

;;;
;;; REPL
;;;

(define (run-repl-thunk thunk input output error stack)
  "Run THUNK with the given REPL STACK. I/O is redirected to the given
INPUT, OUTPUT, and ERROR ports."
  (put-mvar
   repl-output-mvar
   (with-input-from-port input
     (lambda ()
       (with-output-to-port output
         (lambda ()
           (with-error-to-port error
             (lambda ()
               (with-fluids ((*repl-stack* stack))
                 (thunk))))))))))

(define (run-repl)
  "Execute a thunk from the REPL is there is one."
  (unless (mvar-empty? repl-input-mvar)
    (and-let* ((vals (try-take-mvar repl-input-mvar)))
              (apply run-repl-thunk vals))))
