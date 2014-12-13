;;; Sly
;;; Copyright (C) 2014 David Thompson <dthompson2@worcester.edu>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Cooperative REPL server extension.
;;
;;; Code:

(define-module (sly repl)
  #:use-module (system repl coop-server)
  #:use-module (system repl server)
  #:use-module (sly agenda)
  #:use-module (sly game)
  #:export (start-sly-repl
            resume-game-loop))

(define* (start-sly-repl #:optional (port (make-tcp-server-socket #:port 37146)))
  "Start a cooperative REPL server that listens on the given PORT.  By
default, this port is 37146.  Additionally, a process is scheduled to
poll the REPL server upon every tick of the game loop."
  (let ((server (spawn-coop-repl-server port))
        (error-agenda (make-agenda)))
    (schedule-each
     (lambda ()
       (poll-coop-repl-server server)))
    ;; Pause game and handle errors when they occur.
    (add-hook! after-game-loop-error-hook
               (lambda ()
                 (call-with-prompt
                  'sly-repl-error-prompt
                  (lambda ()
                    (with-agenda error-agenda
                      (while #t
                        (poll-coop-repl-server server)
                        (agenda-tick!)
                        (usleep 10))))
                  (lambda (cont)
                    ;; Discard the continuation
                    #f))))))

(define (resume-game-loop)
  "Abort from the error handling loop prompt and resume the game
loop."
  ;; We need to use the agenda so that the prompt is not aborted
  ;; within the REPL, which would break the client session.
  (schedule
   (lambda ()
     (abort-to-prompt 'sly-repl-error-prompt))))
