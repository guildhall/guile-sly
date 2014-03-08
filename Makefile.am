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
  2d/font.scm					\
  2d/fps.scm					\
  2d/game.scm					\
  2d/helpers.scm				\
  2d/keyboard.scm				\
  2d/math.scm					\
  2d/mouse.scm					\
  2d/mvars.scm					\
  2d/rect.scm					\
  2d/shader.scm					\
  2d/signal.scm					\
  2d/sprite.scm					\
  2d/texture.scm				\
  2d/tileset.scm				\
  2d/transform.scm				\
  2d/vector2.scm				\
  2d/window.scm					\
  $(REPL_SOURCES)				\
  $(WRAPPER_SOURCES)

REPL_SOURCES = 					\
  2d/repl/repl.scm				\
  2d/repl/server.scm

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
