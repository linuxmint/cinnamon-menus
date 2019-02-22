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

#ifndef __CMENU_TREE_H__
#define __CMENU_TREE_H__

#ifndef CMENU_I_KNOW_THIS_IS_UNSTABLE
#error "libgnome-menu should only be used if you understand that it's subject to frequent change, and is not supported as a fixed API/ABI or as part of the platform"
#endif

#include <gio/gdesktopappinfo.h>

G_BEGIN_DECLS

#define CMENU_TYPE_TREE         (cmenu_tree_get_type ())
#define CMENU_TREE(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), CMENU_TYPE_TREE, CMenuTree))
#define CMENU_TREE_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), CMENU_TYPE_TREE, CMenuTreeClass))
#define CMENU_IS_TREE(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), CMENU_TYPE_TREE))
#define CMENU_IS_TREE_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), CMENU_TYPE_TREE))
#define CMENU_TREE_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), G_TYPE_DESKTOP_APP_INFO, CMenuTreeClass))

typedef struct _CMenuTree        CMenuTree;
typedef struct _CMenuTreeClass   CMenuTreeClass;

struct _CMenuTreeClass
{
    GObjectClass parent_class;
};

GType cmenu_tree_get_type (void) G_GNUC_CONST;

typedef struct CMenuTreeIter      CMenuTreeIter;
typedef struct CMenuTreeDirectory CMenuTreeDirectory;
typedef struct CMenuTreeEntry     CMenuTreeEntry;
typedef struct CMenuTreeSeparator CMenuTreeSeparator;
typedef struct CMenuTreeHeader    CMenuTreeHeader;
typedef struct CMenuTreeAlias     CMenuTreeAlias;

typedef enum
{
    CMENU_TREE_ITEM_INVALID = 0,
    CMENU_TREE_ITEM_DIRECTORY,
    CMENU_TREE_ITEM_ENTRY,
    CMENU_TREE_ITEM_SEPARATOR,
    CMENU_TREE_ITEM_HEADER,
    CMENU_TREE_ITEM_ALIAS
} CMenuTreeItemType;

GType cmenu_tree_iter_get_type (void);

/* Explicitly skip item, it's a "hidden" base class */
GType cmenu_tree_directory_get_type (void);
GType cmenu_tree_entry_get_type (void);
GType cmenu_tree_separator_get_type (void);
GType cmenu_tree_header_get_type (void);
GType cmenu_tree_alias_get_type (void);

typedef enum
{
    CMENU_TREE_FLAGS_NONE                = 0,
    CMENU_TREE_FLAGS_INCLUDE_EXCLUDED    = 1 << 0,
    CMENU_TREE_FLAGS_INCLUDE_NODISPLAY   = 1 << 1,
    CMENU_TREE_FLAGS_INCLUDE_UNALLOCATED = 1 << 2,
    /* leave some space for more include flags */
    CMENU_TREE_FLAGS_SHOW_EMPTY          = 1 << 8,
    CMENU_TREE_FLAGS_SHOW_ALL_SEPARATORS = 1 << 9,
    /* leave some space for more show flags */
    CMENU_TREE_FLAGS_SORT_DISPLAY_NAME   = 1 << 16
} CMenuTreeFlags;

GType cmenu_tree_flags_get_type (void);

#define CMENU_TYPE_TREE_FLAGS (cmenu_tree_flags_get_type ())

CMenuTree *cmenu_tree_new (const char     *menu_basename,
                           CMenuTreeFlags  flags);

CMenuTree *cmenu_tree_new_for_path (const char     *menu_path,
                                    CMenuTreeFlags  flags);

gboolean   cmenu_tree_load_sync (CMenuTree  *tree,
                                 GError    **error);

const char         *cmenu_tree_get_canonical_menu_path  (CMenuTree  *tree);
CMenuTreeDirectory *cmenu_tree_get_root_directory       (CMenuTree  *tree);
CMenuTreeDirectory *cmenu_tree_get_directory_from_path  (CMenuTree  *tree,
                                                         const char *path);
CMenuTreeEntry     *cmenu_tree_get_entry_by_id          (CMenuTree  *tree,
                                                         const char *id);

gpointer cmenu_tree_item_ref   (gpointer item);
void     cmenu_tree_item_unref (gpointer item);

CMenuTreeDirectory *cmenu_tree_directory_get_parent     (CMenuTreeDirectory *directory);
const char *cmenu_tree_directory_get_name               (CMenuTreeDirectory *directory);
const char *cmenu_tree_directory_get_generic_name       (CMenuTreeDirectory *directory);
const char *cmenu_tree_directory_get_comment            (CMenuTreeDirectory *directory);
GIcon      *cmenu_tree_directory_get_icon               (CMenuTreeDirectory *directory);
const char *cmenu_tree_directory_get_desktop_file_path  (CMenuTreeDirectory *directory);
const char *cmenu_tree_directory_get_menu_id            (CMenuTreeDirectory *directory);
CMenuTree  *cmenu_tree_directory_get_tree               (CMenuTreeDirectory *directory);

gboolean cmenu_tree_directory_get_is_nodisplay          (CMenuTreeDirectory *directory);

CMenuTreeIter      *cmenu_tree_directory_iter           (CMenuTreeDirectory *directory);

CMenuTreeIter      *cmenu_tree_iter_ref                 (CMenuTreeIter *iter);
void                cmenu_tree_iter_unref               (CMenuTreeIter *iter);

CMenuTreeItemType   cmenu_tree_iter_next                (CMenuTreeIter *iter);
CMenuTreeDirectory *cmenu_tree_iter_get_directory       (CMenuTreeIter *iter);
CMenuTreeEntry     *cmenu_tree_iter_get_entry           (CMenuTreeIter *iter);
CMenuTreeHeader    *cmenu_tree_iter_get_header          (CMenuTreeIter *iter);
CMenuTreeAlias     *cmenu_tree_iter_get_alias           (CMenuTreeIter *iter);
CMenuTreeSeparator *cmenu_tree_iter_get_separator       (CMenuTreeIter *iter);

char *cmenu_tree_directory_make_path                    (CMenuTreeDirectory *directory,
                                                         CMenuTreeEntry     *entry);


GDesktopAppInfo    *cmenu_tree_entry_get_app_info       (CMenuTreeEntry *entry);
CMenuTreeDirectory *cmenu_tree_entry_get_parent         (CMenuTreeEntry *entry);
CMenuTree          *cmenu_tree_entry_get_tree           (CMenuTreeEntry *entry);

const char *cmenu_tree_entry_get_desktop_file_path      (CMenuTreeEntry *entry);
const char *cmenu_tree_entry_get_desktop_file_id        (CMenuTreeEntry *entry);

gboolean cmenu_tree_entry_get_is_nodisplay_recurse      (CMenuTreeEntry *entry);
gboolean cmenu_tree_entry_get_is_excluded               (CMenuTreeEntry *entry);
gboolean cmenu_tree_entry_get_is_unallocated            (CMenuTreeEntry *entry);

CMenuTreeDirectory *cmenu_tree_header_get_directory     (CMenuTreeHeader *header);
CMenuTree          *cmenu_tree_header_get_tree          (CMenuTreeHeader *header);
CMenuTreeDirectory *cmenu_tree_header_get_parent        (CMenuTreeHeader *header);

CMenuTreeDirectory *cmenu_tree_alias_get_directory         (CMenuTreeAlias *alias);
CMenuTreeItemType   cmenu_tree_alias_get_aliased_item_type (CMenuTreeAlias *alias);
CMenuTreeDirectory *cmenu_tree_alias_get_aliased_directory (CMenuTreeAlias *alias);
CMenuTreeEntry     *cmenu_tree_alias_get_aliased_entry     (CMenuTreeAlias *alias);
CMenuTree          *cmenu_tree_alias_get_tree              (CMenuTreeAlias *alias);
CMenuTreeDirectory *cmenu_tree_alias_get_parent            (CMenuTreeAlias *alias);

CMenuTree          *cmenu_tree_separator_get_tree   (CMenuTreeSeparator *separator);
CMenuTreeDirectory *cmenu_tree_separator_get_parent (CMenuTreeSeparator *separator);

G_END_DECLS

#endif /* __CMENU_TREE_H__ */
