directorydir = $(datadir)/desktop-directories

# Should be moved to gnome-control-center:
directory_in_controlcenterfiles =		\
	Hardware.directory.in			\
	Personal.directory.in			\
	System.directory.in

directory_in_files =				\
	AudioVideo.directory.in			\
	Development.directory.in		\
	Education.directory.in			\
	Game.directory.in			\
	Graphics.directory.in			\
	Network.directory.in			\
	Office.directory.in			\
	System-Tools.directory.in		\
	Utility.directory.in			\
	Utility-Accessibility.directory.in	\
	X-GNOME-Other.directory.in		\
	X-GNOME-Menu-Applications.directory.in	\
	X-GNOME-Sundry.directory.in 		\
	X-GNOME-Utilities.directory.in 		\
	X-GNOME-WebApplications.directory.in 	\
	X-GNOME-SystemSettings.directory.in	\
	$(directory_in_controlcenterfiles)

directory_DATA = $(directory_in_files:.directory.in=.directory)

@INTLTOOL_DIRECTORY_RULE@

EXTRA_DIST= $(directory_in_files)

DISTCLEANFILES = $(directory_DATA)

-include $(top_srcdir)/git.mk
