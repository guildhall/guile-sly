## guile-2d
## Copyright (C) 2013, 2014 David Thompson <dthompson2@worcester.edu>
##
## This program is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see
## <http://www.gnu.org/licenses/>.

include guile.am

moddir=$(prefix)/share/guile/site/2.0
godir=$(libdir)/guile/2.0/ccache

SOURCES = 					\
  2d/agenda.scm 				\
  2d/animation.scm				\
  2d/audio.scm					\
  2d/color.scm					\
  2d/config.scm					\
  2d/coroutine.scm				\
  2d/event.scm					\
  2d/font.scm					\
  2d/fps.scm					\
  2d/game.scm					\
  2d/helpers.scm				\
  2d/keyboard.scm				\
  2d/math.scm					\
  2d/mouse.scm					\
  2d/rect.scm					\
  2d/repl.scm					\
  2d/shader.scm					\
  2d/signal.scm					\
  2d/sprite.scm					\
  2d/texture.scm				\
  2d/tileset.scm				\
  2d/transform.scm				\
  2d/vector2.scm				\
  2d/window.scm					\
  $(WRAPPER_SOURCES)

WRAPPER_SOURCES =				\
  2d/wrappers/freeimage.scm			\
  2d/wrappers/gl.scm				\
  2d/wrappers/util.scm

2d/config.scm: Makefile
	sed -e "s|@pkgdatadir\@|$(pkgdatadir)|" $@.in > $@

EXTRA_DIST += env.in

SUBDIRS =					\
	data					\
	doc					\
	examples
