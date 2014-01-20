#!/usr/bin/env gjs
// Copyright (C) 2011 Red Hat, Inc.
//
// Authors: Colin Walters <walters@verbum.org>
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//


var GMenu = imports.gi.GMenu;

var tree = new GMenu.Tree({ menu_basename: "applications.menu", flags: 0 });

tree.load_sync();

var root = tree.get_root_directory();

function printDirectory(dir, parent_path) {
    var path;
    if (parent_path == null) {
        path = '/';
    } else {
        path = parent_path + dir.get_name() + "/";
    }

    var iter;
    iter = dir.iter();
    var nextType = iter.next();
    while (nextType != GMenu.TreeItemType.INVALID) {
	if (nextType == GMenu.TreeItemType.ENTRY) {
	    var entry = iter.get_entry();
	    print(path + "\t" + entry.get_app_info().get_name() + "\t" + entry.get_desktop_file_path());
	} else if (nextType == GMenu.TreeItemType.DIRECTORY) {
	    printDirectory(iter.get_directory(), path);
        }
	nextType = iter.next();
    }
}

printDirectory(root);
