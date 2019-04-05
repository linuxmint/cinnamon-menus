/*
 * Copyright (C) 2004, 2011 Red Hat, Inc.
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

#ifndef __CMENU_TREE_ITEM_H__
#define __CMENU_TREE_ITEM_H__

#include <glib.h>
#include <gio/gdesktopappinfo.h>

G_BEGIN_DECLS

typedef struct CMenuTreeItem CMenuTreeItem;
typedef struct CMenuTreeDirectory CMenuTreeDirectory;
typedef struct CMenuTreeEntry     CMenuTreeEntry;
typedef struct CMenuTreeIter      CMenuTreeIter;

GType cmenu_tree_directory_get_type (void);
GType cmenu_tree_entry_get_type (void);
GType cmenu_tree_iter_get_type (void);

/**
 * CMenuTreeItemType:
 * @CMENU_TREE_ITEM_INVALID: Indicates a non-valid tree item.
 * @CMENU_TREE_ITEM_DIRECTORY: A container type tree item that holds a reference to other items.
 * @CMENU_TREE_ITEM_ENTRY: An entry type tree item representing a single node in the tree.
 * @CMENU_TREE_ITEM_SEPARATOR: A separator type tree item. This is no longer used and is only kept
 * for backwards compatibility.
 * @CMENU_TREE_ITEM_HEADER: An in-line header type tree item. This is no longer used and is only
 * kept for backwards compatibility.
 * @CMENU_TREE_ITEM_ALIAS: An in-line alias type tree item. This is no longer used and is only kept
 * for backwards compatibility.
 *
 * The type of tree item.
 */
typedef enum
{
    CMENU_TREE_ITEM_INVALID = 0,
    CMENU_TREE_ITEM_DIRECTORY,
    CMENU_TREE_ITEM_ENTRY,
    CMENU_TREE_ITEM_SEPARATOR,
    CMENU_TREE_ITEM_HEADER,
    CMENU_TREE_ITEM_ALIAS
} CMenuTreeItemType;

// tree item
gpointer                cmenu_tree_item_ref                         (gpointer item);
void                    cmenu_tree_item_unref                       (gpointer item);
CMenuTreeItemType       cmenu_tree_item_get_item_type               (CMenuTreeItem *item);

// tree directory
CMenuTreeIter          *cmenu_tree_directory_iter                   (CMenuTreeDirectory *directory);
CMenuTreeDirectory     *cmenu_tree_directory_get_parent             (CMenuTreeDirectory *directory);
const char             *cmenu_tree_directory_get_name               (CMenuTreeDirectory *directory);
const char             *cmenu_tree_directory_get_generic_name       (CMenuTreeDirectory *directory);
const char             *cmenu_tree_directory_get_comment            (CMenuTreeDirectory *directory);
GIcon                  *cmenu_tree_directory_get_icon               (CMenuTreeDirectory *directory);
const char             *cmenu_tree_directory_get_desktop_file_path  (CMenuTreeDirectory *directory);
const char             *cmenu_tree_directory_get_menu_id            (CMenuTreeDirectory *directory);
gboolean                cmenu_tree_directory_get_is_nodisplay       (CMenuTreeDirectory *directory);
char                   *cmenu_tree_directory_make_path              (CMenuTreeDirectory *directory,
                                                                     CMenuTreeEntry     *entry);

// tree entry
GDesktopAppInfo        *cmenu_tree_entry_get_app_info               (CMenuTreeEntry *entry);
CMenuTreeDirectory     *cmenu_tree_entry_get_parent                 (CMenuTreeEntry *entry);
const char             *cmenu_tree_entry_get_desktop_file_path      (CMenuTreeEntry *entry);
const char             *cmenu_tree_entry_get_desktop_file_id        (CMenuTreeEntry *entry);
gboolean                cmenu_tree_entry_get_is_nodisplay_recurse   (CMenuTreeEntry *entry);
gboolean                cmenu_tree_entry_get_is_excluded            (CMenuTreeEntry *entry);
gboolean                cmenu_tree_entry_get_is_unallocated         (CMenuTreeEntry *entry);

// tree iter
CMenuTreeIter          *cmenu_tree_iter_ref                         (CMenuTreeIter *iter);
void                    cmenu_tree_iter_unref                       (CMenuTreeIter *iter);
CMenuTreeItemType       cmenu_tree_iter_next                        (CMenuTreeIter *iter);
CMenuTreeDirectory     *cmenu_tree_iter_get_directory               (CMenuTreeIter *iter);
CMenuTreeEntry         *cmenu_tree_iter_get_entry                   (CMenuTreeIter *iter);

G_END_DECLS

#endif /* __CMENU_TREE_ITEM_H__ */
