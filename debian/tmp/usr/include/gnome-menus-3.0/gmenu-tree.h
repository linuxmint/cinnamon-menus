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

#ifndef __GMENU_TREE_H__
#define __GMENU_TREE_H__

#ifndef GMENU_I_KNOW_THIS_IS_UNSTABLE
#error "libgnome-menu should only be used if you understand that it's subject to frequent change, and is not supported as a fixed API/ABI or as part of the platform"
#endif

#include <gio/gdesktopappinfo.h>

G_BEGIN_DECLS

#define GMENU_TYPE_TREE         (gmenu_tree_get_type ())
#define GMENU_TREE(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), GMENU_TYPE_TREE, GMenuTree))
#define GMENU_TREE_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), GMENU_TYPE_TREE, GMenuTreeClass))
#define GMENU_IS_TREE(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), GMENU_TYPE_TREE))
#define GMENU_IS_TREE_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), GMENU_TYPE_TREE))
#define GMENU_TREE_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), G_TYPE_DESKTOP_APP_INFO, GMenuTreeClass))

typedef struct _GMenuTree        GMenuTree;
typedef struct _GMenuTreeClass   GMenuTreeClass;

struct _GMenuTreeClass
{
  GObjectClass parent_class;
};

GType gmenu_tree_get_type (void) G_GNUC_CONST;

typedef struct GMenuTreeIter      GMenuTreeIter;
typedef struct GMenuTreeDirectory GMenuTreeDirectory;
typedef struct GMenuTreeEntry     GMenuTreeEntry;
typedef struct GMenuTreeSeparator GMenuTreeSeparator;
typedef struct GMenuTreeHeader    GMenuTreeHeader;
typedef struct GMenuTreeAlias     GMenuTreeAlias;

typedef enum
{
  GMENU_TREE_ITEM_INVALID = 0,
  GMENU_TREE_ITEM_DIRECTORY,
  GMENU_TREE_ITEM_ENTRY,
  GMENU_TREE_ITEM_SEPARATOR,
  GMENU_TREE_ITEM_HEADER,
  GMENU_TREE_ITEM_ALIAS
} GMenuTreeItemType;

GType gmenu_tree_iter_get_type (void);

/* Explicitly skip item, it's a "hidden" base class */
GType gmenu_tree_directory_get_type (void);
GType gmenu_tree_entry_get_type (void);
GType gmenu_tree_separator_get_type (void);
GType gmenu_tree_header_get_type (void);
GType gmenu_tree_alias_get_type (void);

typedef enum
{
  GMENU_TREE_FLAGS_NONE                = 0,
  GMENU_TREE_FLAGS_INCLUDE_EXCLUDED    = 1 << 0,
  GMENU_TREE_FLAGS_INCLUDE_NODISPLAY   = 1 << 1,
  GMENU_TREE_FLAGS_INCLUDE_UNALLOCATED = 1 << 2,
  /* leave some space for more include flags */
  GMENU_TREE_FLAGS_SHOW_EMPTY          = 1 << 8,
  GMENU_TREE_FLAGS_SHOW_ALL_SEPARATORS = 1 << 9,
  /* leave some space for more show flags */
  GMENU_TREE_FLAGS_SORT_DISPLAY_NAME   = 1 << 16
} GMenuTreeFlags;
GType gmenu_tree_flags_get_type (void);
#define GMENU_TYPE_TREE_FLAGS (gmenu_tree_flags_get_type ())

GMenuTree *gmenu_tree_new (const char     *menu_basename,
                           GMenuTreeFlags  flags);

GMenuTree *gmenu_tree_new_for_path (const char     *menu_path,
                                    GMenuTreeFlags  flags);

gboolean   gmenu_tree_load_sync (GMenuTree  *tree,
                                 GError    **error);

const char         *gmenu_tree_get_canonical_menu_path (GMenuTree  *tree);
GMenuTreeDirectory *gmenu_tree_get_root_directory      (GMenuTree  *tree);
GMenuTreeDirectory *gmenu_tree_get_directory_from_path (GMenuTree  *tree,
							const char *path);
GMenuTreeEntry     *gmenu_tree_get_entry_by_id         (GMenuTree  *tree,
							const char *id);

gpointer gmenu_tree_item_ref   (gpointer item);
void     gmenu_tree_item_unref (gpointer item);

GMenuTreeDirectory *gmenu_tree_directory_get_parent    (GMenuTreeDirectory *directory);
const char *gmenu_tree_directory_get_name              (GMenuTreeDirectory *directory);
const char *gmenu_tree_directory_get_generic_name      (GMenuTreeDirectory *directory);
const char *gmenu_tree_directory_get_comment           (GMenuTreeDirectory *directory);
GIcon      *gmenu_tree_directory_get_icon              (GMenuTreeDirectory *directory);
const char *gmenu_tree_directory_get_desktop_file_path (GMenuTreeDirectory *directory);
const char *gmenu_tree_directory_get_menu_id           (GMenuTreeDirectory *directory);
GMenuTree  *gmenu_tree_directory_get_tree              (GMenuTreeDirectory *directory);

gboolean gmenu_tree_directory_get_is_nodisplay (GMenuTreeDirectory *directory);

GMenuTreeIter      *gmenu_tree_directory_iter            (GMenuTreeDirectory *directory);

GMenuTreeIter      *gmenu_tree_iter_ref                  (GMenuTreeIter *iter);
void                gmenu_tree_iter_unref                (GMenuTreeIter *iter);

GMenuTreeItemType   gmenu_tree_iter_next                 (GMenuTreeIter *iter);
GMenuTreeDirectory *gmenu_tree_iter_get_directory        (GMenuTreeIter *iter);
GMenuTreeEntry     *gmenu_tree_iter_get_entry            (GMenuTreeIter *iter);
GMenuTreeHeader    *gmenu_tree_iter_get_header           (GMenuTreeIter *iter);
GMenuTreeAlias     *gmenu_tree_iter_get_alias            (GMenuTreeIter *iter);
GMenuTreeSeparator *gmenu_tree_iter_get_separator        (GMenuTreeIter *iter);

char *gmenu_tree_directory_make_path (GMenuTreeDirectory *directory,
				      GMenuTreeEntry     *entry);


GDesktopAppInfo    *gmenu_tree_entry_get_app_info       (GMenuTreeEntry *entry);
GMenuTreeDirectory *gmenu_tree_entry_get_parent         (GMenuTreeEntry *entry);
GMenuTree          *gmenu_tree_entry_get_tree           (GMenuTreeEntry *entry);

const char *gmenu_tree_entry_get_desktop_file_path (GMenuTreeEntry *entry);
const char *gmenu_tree_entry_get_desktop_file_id   (GMenuTreeEntry *entry);

gboolean gmenu_tree_entry_get_is_nodisplay_recurse  (GMenuTreeEntry *entry);
gboolean gmenu_tree_entry_get_is_excluded  (GMenuTreeEntry *entry);
gboolean gmenu_tree_entry_get_is_unallocated  (GMenuTreeEntry *entry);

GMenuTreeDirectory *gmenu_tree_header_get_directory (GMenuTreeHeader *header);
GMenuTree          *gmenu_tree_header_get_tree      (GMenuTreeHeader *header);
GMenuTreeDirectory *gmenu_tree_header_get_parent    (GMenuTreeHeader *header);

GMenuTreeDirectory *gmenu_tree_alias_get_directory         (GMenuTreeAlias *alias);
GMenuTreeItemType   gmenu_tree_alias_get_aliased_item_type (GMenuTreeAlias *alias);
GMenuTreeDirectory *gmenu_tree_alias_get_aliased_directory (GMenuTreeAlias *alias);
GMenuTreeEntry     *gmenu_tree_alias_get_aliased_entry     (GMenuTreeAlias *alias);
GMenuTree          *gmenu_tree_alias_get_tree              (GMenuTreeAlias *alias);
GMenuTreeDirectory *gmenu_tree_alias_get_parent            (GMenuTreeAlias *alias);

GMenuTree          *gmenu_tree_separator_get_tree (GMenuTreeSeparator *separator);
GMenuTreeDirectory *gmenu_tree_separator_get_parent (GMenuTreeSeparator *separator);

G_END_DECLS

#endif /* __GMENU_TREE_H__ */
