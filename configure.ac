dnl -*- Autoconf -*-

AC_INIT(Sly, 0.2.0)
AC_CONFIG_SRCDIR(sly)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile doc/Makefile examples/Makefile data/Makefile])
AC_CONFIG_FILES([env], [chmod +x env])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])


PKG_CHECK_MODULES([SDL], [sdl <= 1.3 sdl >= 1.2])
PKG_CHECK_MODULES([SDL_tff], [SDL_ttf >= 2.0])
GUILE_PROGS([2.0.11])
PKG_CHECK_MODULES([GSL], [gsl])
# Why doesn't freeimage have a pkg-config file?
#PKG_CHECK_MODULES([freeimage], [freeimage >= 3.0])

AC_OUTPUT
