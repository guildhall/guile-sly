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
(define label
  (make-label font "The quick brown fox jumped over the lazy dog."
              (vector2 320 240) #:anchor 'center))

(define-signal fps-label
  (signal-map (lambda (fps)
                (let ((text (format #f "FPS: ~d" fps)))
                  (make-label font text (vector2 0 0))))
              fps))

(define-signal mouse-label
  (signal-map (lambda (p)
                (let ((text (format #f "Mouse: (~d, ~d)" (vx p) (vy p))))
                  (make-label font text (vector2 0 20))))
              (signal-throttle game-agenda 5 mouse-position)))

(add-hook! draw-hook (lambda (dt alpha)
                       (draw-label label)
                       (draw-label (signal-ref fps-label))
                       (draw-label (signal-ref mouse-label))))

(with-window (make-window #:title "Fonts")
  (run-game-loop))
