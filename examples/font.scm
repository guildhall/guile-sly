;;; Sly
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

(use-modules (sly game)
             (sly fps)
             (sly signal)
             (sly window)
             (sly math vector)
             (sly render camera)
             (sly render color)
             (sly render font)
             (sly render model)
             (sly render model)
             (sly input mouse))

(load "common.scm")

(enable-fonts)

(define font (load-default-font 18))

(define-signal message-label
  (move (vector2 320 240)
        (label font "The quick brown fox jumped over the lazy dog."
               #:anchor 'center)))

(define-signal fps-label
  (signal-map (lambda (fps)
                (let ((text (format #f "FPS: ~d" fps)))
                  (move (vector2 0 480) (label font text))))
              fps))

(define-signal mouse-label
  (signal-map (lambda (p)
                (let ((text (format #f "Mouse: (~d, ~d)" (vx p) (vy p))))
                  (move (vector2 0 460) (label font text))))
              (signal-throttle 5 mouse-position)))

(define-signal scene
  (signal-map group message-label fps-label mouse-label))

(define camera (orthographic-camera 640 480))

(add-hook! draw-hook (lambda _ (draw-model (signal-ref scene) camera)))

(with-window (make-window #:title "Fonts")
  (start-game-loop))

;;; Local Variables:
;;; compile-command: "../pre-inst-env guile font.scm"
;;; End:
