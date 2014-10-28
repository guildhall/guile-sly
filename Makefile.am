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
  sly/agenda.scm 				\
  sly/animation.scm				\
  sly/audio.scm					\
  sly/color.scm					\
  sly/config.scm					\
  sly/coroutine.scm				\
  sly/event.scm					\
  sly/font.scm					\
  sly/fps.scm					\
  sly/game.scm					\
  sly/helpers.scm				\
  sly/keyboard.scm				\
  sly/live-reload.scm				\
  sly/math.scm					\
  sly/math/vector.scm				\
  sly/mesh.scm					\
  sly/mouse.scm					\
  sly/quaternion.scm				\
  sly/rect.scm					\
  sly/repl.scm					\
  sly/scene.scm					\
  sly/shader.scm					\
  sly/shape.scm					\
  sly/signal.scm					\
  sly/sprite.scm					\
  sly/texture.scm				\
  sly/tileset.scm				\
  sly/transform.scm				\
  sly/transition.scm				\
  sly/window.scm					\
  sly/joystick.scm				\
  sly/render/utils.scm				\
  sly/render/camera.scm				\
  sly/render/framebuffer.scm			\
  sly/render/vertex-array.scm			\
  sly/render/renderer.scm			\
  $(WRAPPER_SOURCES)

WRAPPER_SOURCES =				\
  sly/wrappers/freeimage.scm			\
  sly/wrappers/gl.scm				\
  sly/wrappers/util.scm

sly/config.scm: Makefile
	sed -e "s|@pkgdatadir\@|$(pkgdatadir)|" $@.in > $@

EXTRA_DIST += env.in

SUBDIRS =					\
	data					\
	doc					\
	examples
