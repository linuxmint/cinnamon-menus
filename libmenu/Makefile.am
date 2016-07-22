lib_LTLIBRARIES = libcinnamon-menu-3.la

AM_CPPFLAGS =				\
	$(GIO_UNIX_CFLAGS)		\
	$(WARN_CFLAGS)			\
	-DGMENU_I_KNOW_THIS_IS_UNSTABLE	\
	$(DISABLE_DEPRECATED_CFLAGS)	\
	$(DEBUG_CFLAGS)

AM_CFLAGS = $(WARN_CFLAGS)

libcinnamon_menu_3_includedir = $(includedir)/cinnamon-menus-3.0
libcinnamon_menu_3_include_HEADERS =		\
	gmenu-tree.h

libcinnamon_menu_3_sources =			\
	canonicalize.c			\
	desktop-entries.c		\
	entry-directories.c		\
	gmenu-tree.c			\
	menu-layout.c			\
	menu-monitor.c			\
	menu-util.c

libcinnamon_menu_3_la_SOURCES =		\
	$(libcinnamon_menu_3_sources)	\
	canonicalize.h			\
	desktop-entries.h		\
	entry-directories.h		\
	gmenu-tree.h			\
	menu-layout.h			\
	menu-monitor.h			\
	menu-util.h

libcinnamon_menu_3_la_LIBADD = 		\
	$(GIO_UNIX_LIBS)

libcinnamon_menu_3_la_LDFLAGS =			\
	$(WARN_LDFLAGS)				\
	-version-info $(LIB_MENU_LT_VERSION)	\
	-no-undefined				\
	-export-symbols-regex gmenu_tree

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libcinnamon-menu-3.0.pc

EXTRA_DIST =				\
	libcinnamon-menu-3.0.pc.in		\
	libcinnamon-menu-3.0-uninstalled.pc.in

CLEANFILES =

# Introspection
-include $(INTROSPECTION_MAKEFILE)
INTROSPECTION_GIRS =
INTROSPECTION_SCANNER_ARGS = --warn-all --add-include-path=$(srcdir)
INTROSPECTION_COMPILER_ARGS = --includedir=$(srcdir)

if HAVE_INTROSPECTION
introspection_sources = $(libcinnamon_menu_3_include_HEADERS) gmenu-tree.c

CMenu-3.0.gir: libcinnamon-menu-3.la
CMenu_3_0_gir_INCLUDES = Gio-2.0
CMenu_3_0_gir_CFLAGS = $(AM_CPPFLAGS)
CMenu_3_0_gir_LIBS = libcinnamon-menu-3.la
CMenu_3_0_gir_SCANNERFLAGS = $(WARN_SCANNERFLAGS) --identifier-prefix=GMenu --symbol-prefix=gmenu --pkg-export=libcinnamon-menu-3.0 --c-include=gmenu-tree.h
CMenu_3_0_gir_FILES = $(addprefix $(srcdir)/,$(introspection_sources))
INTROSPECTION_GIRS += CMenu-3.0.gir

girdir = $(INTROSPECTION_GIRDIR)
gir_DATA = $(INTROSPECTION_GIRS)

typelibdir = $(INTROSPECTION_TYPELIBDIR)
typelib_DATA = $(INTROSPECTION_GIRS:.gir=.typelib)

CLEANFILES += $(gir_DATA) $(typelib_DATA)
endif

-include $(top_srcdir)/git.mk
