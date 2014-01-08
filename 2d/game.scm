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
  #:export (ticks-per-second
            tick-interval
            draw-hook
            run-game-loop
            quit-game
            pause-game
            resume-game
            game-running?
            game-paused?
            register-event-handler
            current-fps))

;;;
;;; Game Loop
;;;

;; Possible states are:
;; * stopped
;; * running
;; * paused
(define game-loop-status (make-parameter 'stopped))
(define ticks-per-second 60)
(define tick-interval (make-parameter 0))
(define draw-hook (make-hook))

(define (run-game-loop)
  "Start the game loop."
  (parameterize ((game-loop-status 'running)
                 (tick-interval (floor (/ 1000 ticks-per-second))))
    (resume-game)
    (spawn-server)
    (game-loop (SDL:get-ticks) 0)))

(define (draw dt alpha)
  "Render a frame."
  (set-gl-matrix-mode (matrix-mode modelview))
  (gl-load-identity)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (run-hook draw-hook)
  (SDL:gl-swap-buffers)
  (accumulate-fps! dt))

(define (update accumulator)
  "Call the update callback. The update callback will be called as
many times as `tick-interval` can divide ACCUMULATOR. The return value
is the unused accumulator time."
  (if (>= accumulator (tick-interval))
      (begin
        (handle-events)
        (tick-agenda!)
        (update (- accumulator (tick-interval))))
      accumulator))

(define (update-and-render dt accumulator)
  (let ((remainder (update accumulator)))
    (run-repl)
    (draw dt (/ remainder (tick-interval)))
    remainder))

(define (tick dt accumulator)
  "Advance the game by one frame."
  (if (game-paused?)
      (begin
        (run-repl)
        (SDL:delay (tick-interval))
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
  (or (eq? (game-loop-status) 'running)
      (eq? (game-loop-status) 'paused)))

(define (game-paused?)
  (eq? (game-loop-status) 'paused))

(define (pause-game)
  "Pauses the game loop. Useful when developing."
  (game-loop-status 'paused))

(define (resume-game)
  "Resumes the game loop."
  (when (game-paused?)
    (game-loop-status 'running)))

(define (quit-game)
  "Finish the current frame and terminate the game loop."
  (game-loop-status 'stopped))

;;;
;;; Event Handling
;;;

(define handle-events
  (let ((e (SDL:make-event)))
    (lambda ()
      "Handle all events in the SDL event queue."
      (while (SDL:poll-event e)
        (handle-event e)))))

(define event-handlers '())

(define (register-event-handler event callback)
  "Register CALLBACK to respond to events of type EVENT."
  (set! event-handlers (acons event callback event-handlers)))

(define (handle-event e)
  "Call the relevant callback procedure for the event E."
  (let ((handler (assq-ref event-handlers (SDL:event:type e))))
    (when handler
      (handler e))))

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
