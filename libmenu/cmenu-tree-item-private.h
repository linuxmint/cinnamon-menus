/* -*- mode:c; c-file-style: "gnu"; indent-tabs-mode: nil -*-
 * Copyright (C) 2003, 2004, 2011 Red Hat, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef __CMENU_TREE_ITEM_PRIV_H__
#define __CMENU_TREE_ITEM_PRIV_H__

#include <config.h>
#include "cmenu-tree-item.h"
#include "desktop-entries.h"

G_BEGIN_DECLS

#define CMENU_TREE_ITEM(i)      ((CMenuTreeItem *)(i))
#define CMENU_TREE_DIRECTORY(i) ((CMenuTreeDirectory *)(i))
#define CMENU_TREE_ENTRY(i)     ((CMenuTreeEntry *)(i))

struct CMenuTreeItem
{
    volatile gint refcount;

    CMenuTreeItemType type;

    CMenuTreeDirectory *parent;
};

struct CMenuTreeDirectory
{
    CMenuTreeItem   item;

    DesktopEntry   *directory_entry;
    char           *name;

    GSList         *entries;
    GSList         *subdirs;
    GSList         *contents;

    guint only_unallocated : 1;
    guint is_nodisplay : 1;
};

struct CMenuTreeEntry
{
    CMenuTreeItem item;

    DesktopEntry *desktop_entry;
    char         *desktop_file_id;

    guint is_excluded : 1;
    guint is_unallocated : 1;
};

struct CMenuTreeIter
{
    volatile gint refcount;

    CMenuTreeItem *item;
    GSList        *contents;
    GSList        *contents_iter;
};


// tree item
void                cmenu_tree_item_unref_and_unset_parent       (gpointer           itemp);

// tree directory
CMenuTreeDirectory *cmenu_tree_directory_new                     (CMenuTreeDirectory *parent,
                                                                  const char         *name);
void                cmenu_tree_directory_finalize                (CMenuTreeDirectory *directory);
void                cmenu_tree_directory_set_is_nodisplay        (CMenuTreeDirectory *directory,
                                                                  gboolean            nodisplay);
gboolean            cmenu_tree_directory_get_only_unallocated    (CMenuTreeDirectory *directory);
void                cmenu_tree_directory_set_only_unallocated    (CMenuTreeDirectory *directory,
                                                                  gboolean            only_unallocated);
DesktopEntry       *cmenu_tree_directory_get_directory_entry     (CMenuTreeDirectory *directory);
void                cmenu_tree_directory_set_directory_entry     (CMenuTreeDirectory *directory,
                                                                  DesktopEntry       *entry);
void                cmenu_tree_directory_flatten                 (CMenuTreeDirectory *directory);
int                 cmenu_tree_directory_compare                 (CMenuTreeItem      *a,
                                                                  CMenuTreeItem      *b);

// tree entry
CMenuTreeEntry     *cmenu_tree_entry_new                         (CMenuTreeDirectory *parent,
                                                                  DesktopEntry       *desktop_entry,
                                                                  const char         *desktop_file_id,
                                                                  gboolean            is_excluded,
                                                                  gboolean            is_unallocated);
void                cmenu_tree_entry_finalize                    (CMenuTreeEntry     *entry);
int                 cmenu_tree_entry_compare_by_id               (CMenuTreeItem      *a,
                                                                  CMenuTreeItem      *b);
int                 cmenu_tree_entry_compare_by_name             (CMenuTreeItem      *a,
                                                                  CMenuTreeItem      *b);
DesktopEntry       *cmenu_tree_entry_get_desktop_entry           (CMenuTreeEntry     *entry);

G_END_DECLS

#endif /* __CMENU_TREE_ITEM_PRIV_H__ */
