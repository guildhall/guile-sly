;;; Sly
;;; Copyright (C) 2014 David Thompson <davet@gnu.org>
;;;
;;; Sly is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Sly is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Development environment for GNU Guix.
;;
;;; Code:

(use-modules (guix packages)
             (guix licenses)
             (guix build-system gnu)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages gl)
             (gnu packages pkg-config)
             (gnu packages sdl)
             (gnu packages maths)
             (gnu packages image))

;; The development environment needs a tweaked LTDL_LIBRARY_PATH for
;; finding libfreeimage.
(define freeimage
  (package (inherit freeimage)
    (native-search-paths
     (list (search-path-specification
            (variable "LTDL_LIBRARY_PATH")
            (files '("lib")))))))

(package
  (name "sly")
  (version "0.0")
  (source ".")
  (build-system gnu-build-system)
  (inputs
   `(("pkg-config" ,pkg-config)
     ("autoconf" ,autoconf)
     ("automake" ,automake)
     ("guile" ,guile-2.0)
     ("guile-sdl" ,guile-sdl)
     ("guile-opengl" ,guile-opengl)
     ("gsl" ,gsl)
     ("freeimage" ,freeimage)
     ("mesa" ,mesa)))
  (synopsis "2D/3D game engine for GNU Guile")
  (description "Sly is a 2D/3D game engine written in Guile Scheme.
Sly differs from most game engines in that it emphasizes functional
reactive programming and live coding.")
  (home-page "https://gitorious.org/sly/sly")
  (license gpl3+))
