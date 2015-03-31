AC_PREREQ(2.62)

AC_INIT([cinnamon-menus], [2.4.2])
AC_CONFIG_SRCDIR(libmenu/gmenu-tree.h)

AM_INIT_AUTOMAKE([1.11 foreign no-dist-gzip dist-xz])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADERS(config.h)

AM_MAINTAINER_MODE
GNOME_MAINTAINER_MODE_DEFINES

# Before making a release, the LT_VERSION string should be modified.
# The string is of the form C:R:A.
# - If interfaces have been changed or added, but binary compatibility has
#   been preserved, change to C+1:0:A+1
# - If binary compatibility has been broken (eg removed or changed interfaces)
#   change to C+1:0:0
# - If the interface is the same as the previous version, change to C:R+1:A

LIB_MENU_LT_VERSION=0:1:0
AC_SUBST(LIB_MENU_LT_VERSION)

AC_ISC_POSIX
AC_PROG_CC
AC_STDC_HEADERS
AC_ARG_PROGRAM
AC_LIBTOOL_WIN32_DLL
AM_PROG_LIBTOOL

PKG_CHECK_MODULES(GIO_UNIX, gio-unix-2.0 >= 2.29.15)
AC_SUBST(GIO_UNIX_CFLAGS)
AC_SUBST(GIO_UNIX_LIBS)

GNOME_COMPILE_WARNINGS(yes)

AC_ARG_ENABLE(deprecation_flags,
              [AC_HELP_STRING([--enable-deprecation-flags],
                              [use *_DISABLE_DEPRECATED flags @<:@default=no@:>@])],,
              [enable_deprecation_flags=no])

if test "x$enable_deprecation_flags" = "xyes"; then
   DISABLE_DEPRECATED_CFLAGS=$DISABLE_DEPRECATED
   AC_SUBST(DISABLE_DEPRECATED_CFLAGS)
fi

AC_ARG_ENABLE(debug,
              [AS_HELP_STRING([--enable-debug=@<:@no/minimum/yes@:>@],
                              [turn on debugging @<:@default=debug_default@:>@])],,
              [enable_debug=debug_default])
if test "x$enable_debug" = "xyes"; then
  DEBUG_CFLAGS="-DG_ENABLE_DEBUG"
else
  if test "x$enable_debug" = "xno"; then
    DEBUG_CFLAGS="-DG_DISABLE_ASSERT -DG_DISABLE_CHECKS -DG_DISABLE_CAST_CHECKS"
  else
    DEBUG_CFLAGS="-DG_ENABLE_DEBUG -DG_DISABLE_CAST_CHECKS"
  fi
fi
AC_SUBST(DEBUG_CFLAGS)

GOBJECT_INTROSPECTION_CHECK([0.9.5])

AC_OUTPUT([
Makefile
libmenu/Makefile
libmenu/libcinnamon-menu-3.0.pc
libmenu/libcinnamon-menu-3.0-uninstalled.pc
])

dnl ---------------------------------------------------------------------------
dnl - Show summary
dnl ---------------------------------------------------------------------------

echo "
              cinnamon-menus $VERSION
              `echo cinnamon-menus $VERSION | sed "s/./=/g"`

        prefix:                       ${prefix}
        exec_prefix:                  ${exec_prefix}
        libdir:                       ${libdir}
        bindir:                       ${bindir}
        sbindir:                      ${sbindir}
        sysconfdir:                   ${sysconfdir}
        localstatedir:                ${localstatedir}
        datadir:                      ${datadir}
        source code location:         ${srcdir}
        compiler:                     ${CC}
        cflags:                       ${CFLAGS}
        Maintainer mode:              ${USE_MAINTAINER_MODE}
        Use *_DISABLE_DEPRECATED:     ${enable_deprecation_flags}

        Turn on debugging:            ${enable_debug}
        Build introspection support:  ${found_introspection}

"
