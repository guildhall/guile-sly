dnl -*- Autoconf -*-

AC_INIT(Sly, 0.1)
AC_CONFIG_SRCDIR(sly)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

AC_PATH_PROG([GUILE], [guile])
AC_CONFIG_FILES([Makefile doc/Makefile examples/Makefile data/Makefile])
AC_CONFIG_FILES([sly/config.scm])
AC_CONFIG_FILES([sandbox], [chmod +x sandbox])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

GUILE_PROGS([2.0.11])
GUILE_MODULE_REQUIRED([sdl sdl])
GUILE_MODULE_REQUIRED([sdl mixer])
GUILE_MODULE_REQUIRED([sdl ttf])
GUILE_MODULE_REQUIRED([gl])

dnl Freeimage doesn't have a pkg-config file, so use this primitive
dnl test instead.
AC_CHECK_LIB([freeimage], [FreeImage_GetVersion],,
  [AC_MSG_ERROR([freeimage not found.])])

PKG_CHECK_MODULES([GSL], [gsl])

AC_OUTPUT
