;;; guile-2d
;;; Copyright (C) 2013, 2014 David Thompson <dthompson2@worcester.edu>
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
;; Frames per second counter.
;;
;;; Code:

(define-module (2d fps)
  #:use-module (2d game)
  #:use-module (2d signal)
  #:export (fps))

;; Current frames per second
(define-signal fps (make-signal 0))

(define accumulate-fps!
  (let* ((elapsed-time 0)
         (fps-counter 0))
    (lambda (dt alpha)
      (let ((new-time (+ elapsed-time dt))
            (new-fps (1+ fps-counter)))
        (if (>= new-time 1000)
            (begin
              (signal-set! fps new-fps)
              (set! fps-counter 0)
              (set! elapsed-time 0))
            (begin
              (set! fps-counter new-fps)
              (set! elapsed-time new-time)))))))

(add-hook! draw-hook accumulate-fps!)
