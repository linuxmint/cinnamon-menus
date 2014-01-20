noinst_PROGRAMS = gnome-menu-spec-test

AM_CPPFLAGS =							\
	$(GIO_UNIX_CFLAGS)					\
	-I$(srcdir)/../libmenu					\
	-DGMENU_I_KNOW_THIS_IS_UNSTABLE				\
	-DGNOMELOCALEDIR=\""$(prefix)/$(DATADIRNAME)/locale"\"	\
	$(DISABLE_DEPRECATED_CFLAGS)				\
	$(DEBUG_CFLAGS)

AM_CFLAGS = $(WARN_CFLAGS)

gnome_menu_spec_test_SOURCES =		\
	test-menu-spec.c

gnome_menu_spec_test_LDADD =		\
	$(GIO_UNIX_LIBS)		\
	../libmenu/libgnome-menu-3.la

EXTRA_DIST = gnome-menus-ls.js

-include $(top_srcdir)/git.mk
