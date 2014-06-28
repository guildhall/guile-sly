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

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (sly agenda)
             (sly game)
             (sly sprite)
             (sly texture)
             (sly vector)
             (sly window))

(load "common.scm")

(set! *random-state* (random-state-from-platform))

(define window-width 640)
(define window-height 480)

;;;
;;; Particles
;;;

(define-record-type <particle>
  (make-particle sprite position velocity)
  particle?
  (sprite particle-sprite)
  (position particle-position set-particle-position!)
  (velocity particle-velocity set-particle-velocity!))

(define (update-particle! particle)
  (set-particle-position! particle
                          (v+ (particle-position particle)
                              (particle-velocity particle))))

(define (generate-particles n)
  (let ((particle-image (load-texture "images/bullet.png")))
    (list-tabulate n (lambda (n)
                       (make-particle (make-sprite particle-image)
                                      (vector (random window-width)
                                              (random window-height))
                                      (vector (* (random:normal) 1)
                                              (* (random:normal) 1)))))))

(define particle-count 500)
(define background (load-sprite "images/stars.png"
                                #:anchor #(0 0)))
(define particles (generate-particles particle-count))

(define (draw-particles particles)
  (for-each
   (lambda (p)
     (let* ((sprite (particle-sprite p)))
       (draw-sprite (set-sprite-position sprite (particle-position p)))))
   particles))

(define (draw dt alpha)
  (draw-sprite background)
  (draw-particles particles))

(define (update)
  (for-each update-particle! particles))

(schedule-each update)
(add-hook! draw-hook draw)

(with-window (make-window #:title "Particles"
                          #:resolution (vector window-width
                                               window-height))
  (start-game-loop))
