## Sly
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
  sly/utils.scm					\
  sly/agenda.scm 				\
  sly/audio.scm					\
  sly/config.scm					\
  sly/coroutine.scm				\
  sly/event.scm					\
  sly/fps.scm					\
  sly/game.scm					\
  sly/input/keyboard.scm			\
  sly/input/mouse.scm				\
  sly/input/joystick.scm			\
  sly/live-reload.scm				\
  sly/math.scm					\
  sly/math/quaternion.scm			\
  sly/math/rect.scm				\
  sly/math/transform.scm			\
  sly/math/tween.scm				\
  sly/math/vector.scm				\
  sly/repl.scm					\
  sly/signal.scm					\
  sly/window.scm					\
  sly/render/utils.scm				\
  sly/render/color.scm				\
  sly/render/camera.scm				\
  sly/render/font.scm				\
  sly/render/framebuffer.scm			\
  sly/render/mesh.scm				\
  sly/render/model.scm				\
  sly/render/texture.scm			\
  sly/render/shader.scm				\
  sly/render/shape.scm				\
  sly/render/sprite.scm				\
  sly/render/tileset.scm			\
  sly/render/tile-map.scm			\
  sly/render/context.scm			\
  sly/render/scene.scm				\
  $(WRAPPER_SOURCES)

WRAPPER_SOURCES =				\
  sly/wrappers/freeimage.scm			\
  sly/wrappers/gl.scm				\
  sly/wrappers/gsl.scm				\
  sly/wrappers/util.scm

sly/config.scm: Makefile
	sed -e "s|@pkgdatadir\@|$(pkgdatadir)|" $@.in > $@

EXTRA_DIST += env.in

SUBDIRS =					\
	data					\
	doc					\
	examples
