;;; guile-2d
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

(define-module (2d repl)
  #:use-module (system repl coop-server)
  #:use-module (2d agenda)
  #:use-module (2d game))

(define server (spawn-coop-repl-server))

(define (poll-server)
  (poll-coop-repl-server server))

(schedule-interval game-agenda poll-server 2)
