#!/bin/bash
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

PKG_NAME="cinnamon-menus"
REQUIRED_AUTOMAKE_VERSION=1.10

which gnome-autogen.sh || {
    echo "You need to install gnome-common from GNOME Subversion (or from"
    echo "your distribution's package manager)."
    exit 1
}
USE_GNOME2_MACROS=1 USE_COMMON_DOC_BUILD=yes . gnome-autogen.sh
