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
;; Wrappers over SDL mixer.
;;
;;; Code:

(define-module (2d audio)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-2)
  #:use-module ((sdl mixer) #:prefix SDL:))

(SDL:open-audio)

;; Wrapper over SDL audio objects.
(define-record-type <sample>
  (make-sample audio)
  sample?
  (audio sample-audio))

(define (load-sample filename)
  "Load audio sample from FILENAME or return #f if the file cannot be
loaded"
  (let ((audio (SDL:load-wave filename)))
    (if audio (make-sample audio) #f)))

(define (sample-volume)
  "Return the volume that all samples are played at."
  (SDL:volume))

(define (set-sample-volume volume)
  "Set the volume that all samples are played at to VOLUME."
  (SDL:volume volume)
  *unspecified*)

(define (play-sample sample)
  "Play the given audio SAMPLE."
  (SDL:play-channel (sample-audio sample))
  *unspecified*)

(export load-sample
        sample?
        sample-audio
        sample-volume
        set-sample-volume
        play-sample)

;; Wrapper over SDL music objects.
(define-record-type <music>
  (make-music audio)
  music?
  (audio music-audio))

(define (load-music filename)
  "Load music from FILENAME or return #f if the file cannot be
loaded."
  (let ((audio (SDL:load-music filename)))
    (if audio (make-music audio) #f)))

(define (music-volume)
  "Return the volume that music is played at."
  (SDL:music-volume))

(define (set-music-volume volume)
  "Set the volume that music is played at."
  (SDL:volume volume)
  *unspecified*)

(define (play-music music)
  "Play the given MUSIC."
  (SDL:play-music (music-audio music))
  *unspecified*)

(export load-music
        music?
        music-audio
        music-volume
        play-music
        set-music-volume)

(re-export (SDL:pause-music . pause-music)
           (SDL:resume-music . resume-music)
           (SDL:rewind-music . rewind-music)
           (SDL:halt-music . stop-music)
           (SDL:paused-music? . music-paused?)
           (SDL:playing-music? . music-playing?))
