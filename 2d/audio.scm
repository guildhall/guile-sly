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
;; Wrappers over SDL mixer.
;;
;;; Code:

(define-module (2d audio)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-2)
  #:use-module ((sdl mixer) #:prefix SDL:)
  #:export (load-sample
            sample?
            sample-audio
            sample-volume
            set-sample-volume
            play-sample
            load-music
            music?
            music-audio
            music-volume
            play-music
            set-music-volume
            pause-music
            resume-music
            rewind-music
            stop-music
            music-paused?
            music-playing?))

(SDL:open-audio)

;; Used to wrap SDL audio functions whose return values should be
;; ignored.
(define-syntax-rule (ignore-value body ...)
  (begin
    body ...
    *unspecified*))

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
  (ignore-value (SDL:volume volume)))

(define (play-sample sample)
  "Play the given audio SAMPLE."
  (ignore-value (SDL:play-channel (sample-audio sample))))

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
  "Set the volume that music is played at to VOLUME."
  (ignore-value (SDL:volume volume)))

(define (play-music music)
  "Play the given MUSIC."
  (ignore-value (SDL:play-music (music-audio music))))

(define (pause-music)
  "Pause the current music track."
  (ignore-value (SDL:pause-music)))

(define (resume-music)
  "Resume the current music track."
  (ignore-value (SDL:resume-music)))

(define (rewind-music)
  "Restart the current music track."
  (ignore-value (SDL:rewind-music)))

(define (stop-music)
  "Stop playing the current music track."
  (ignore-value (SDL:halt-music)))

(define (music-playing?)
  "Return #t if music is currently playing, otherwise return #f."
  (SDL:playing-music?))

(define (music-paused?)
  "Return #t if music is currently paused, otherwise return #f."
  (SDL:paused-music?))
