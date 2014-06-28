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
  #:export (start-sly-repl))

(define* (start-sly-repl #:optional (port (make-tcp-server-socket #:port 37146)))
  "Start a cooperative REPL server that listens on the given PORT.  By
default, this port is 37146.  Additionally, a process is scheduled to
poll the REPL server upon every tick of the game loop."
  (let ((server (spawn-coop-repl-server port)))
    (schedule-each
     (lambda ()
       (poll-coop-repl-server server)))))
