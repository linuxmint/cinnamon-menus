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

#include "cmenu-tree-item-private.h"
#include "menu-layout.h"

static void
cmenu_tree_item_set_parent (CMenuTreeItem      *item,
                            CMenuTreeDirectory *parent)
{
    g_return_if_fail (item != NULL);

    item->parent = parent;
}

/**
 * cmenu_tree_item_ref:
 * @item: a #CMenuTreeItem
 *
 * Returns: (transfer full): The same @item, or %NULL if @item is not a valid #CMenuTreeItem
 */
gpointer
cmenu_tree_item_ref (gpointer itemp)
{
    CMenuTreeItem *item;

    item = (CMenuTreeItem *) itemp;

    g_return_val_if_fail (item != NULL, NULL);
    g_return_val_if_fail (item->refcount > 0, NULL);

    g_atomic_int_inc (&item->refcount);

    return item;
}

void
cmenu_tree_item_unref (gpointer itemp)
{
    CMenuTreeItem *item;

    item = (CMenuTreeItem *) itemp;

    g_return_if_fail (item != NULL);
    g_return_if_fail (item->refcount > 0);

    if (g_atomic_int_dec_and_test (&(item->refcount)))
    {
        switch (item->type)
        {
            case CMENU_TREE_ITEM_DIRECTORY:
                cmenu_tree_directory_finalize (CMENU_TREE_DIRECTORY (item));
                break;

            case CMENU_TREE_ITEM_ENTRY:
                cmenu_tree_entry_finalize (CMENU_TREE_ENTRY (item));
                break;

            default:
                g_assert_not_reached ();
                break;
        }
    }
}

void
cmenu_tree_item_unref_and_unset_parent (gpointer itemp)
{
    CMenuTreeItem *item;

    item = (CMenuTreeItem *) itemp;

    g_return_if_fail (item != NULL);

    cmenu_tree_item_set_parent (item, NULL);
    cmenu_tree_item_unref (item);
}

const char *
cmenu_tree_item_compare_get_name_helper (CMenuTreeItem *item,
                                         gboolean       sort_display_name)
{
    const char *name;

    name = NULL;

    switch (item->type)
    {
        case CMENU_TREE_ITEM_DIRECTORY:
            if (CMENU_TREE_DIRECTORY (item)->directory_entry)
                name = desktop_entry_get_name (CMENU_TREE_DIRECTORY (item)->directory_entry);
            else
                name = CMENU_TREE_DIRECTORY (item)->name;
            break;

        case CMENU_TREE_ITEM_ENTRY:
            if (sort_display_name)
                name = g_app_info_get_display_name (G_APP_INFO (cmenu_tree_entry_get_app_info (CMENU_TREE_ENTRY (item))));
            else
                name = desktop_entry_get_name (CMENU_TREE_ENTRY (item)->desktop_entry);
            break;

        default:
            g_assert_not_reached ();
            break;
    }

    return name;
}

int
cmenu_tree_item_compare (CMenuTreeItem *a,
                         CMenuTreeItem *b,
                         gpointer       sort_display_name)
{
    const char       *name_a;
    const char       *name_b;

    name_a = cmenu_tree_item_compare_get_name_helper (a, GPOINTER_TO_INT (sort_display_name));
    name_b = cmenu_tree_item_compare_get_name_helper (b, GPOINTER_TO_INT (sort_display_name));

    return g_utf8_collate (name_a, name_b);
}

CMenuTreeItemType
cmenu_tree_item_get_item_type (CMenuTreeItem *item)
{
    return item->type;
}

static CMenuTreeDirectory *
get_parent (CMenuTreeItem *item)
{
    g_return_val_if_fail (item != NULL, NULL);
    return item->parent ? cmenu_tree_item_ref (item->parent) : NULL;
}

/**
 * cmenu_tree_directory_get_parent:
 * @directory: a #CMenuTreeDirectory
 *
 * Returns: (transfer full): The parent directory, or %NULL if none
 */
CMenuTreeDirectory *
cmenu_tree_directory_get_parent (CMenuTreeDirectory *directory)
{
    return get_parent ((CMenuTreeItem *)directory);
}

/**
 * cmenu_tree_directory_iter:
 * @directory: directory
 *
 * Returns: (transfer full): A new iterator over the directory contents
 */
CMenuTreeIter *
cmenu_tree_directory_iter (CMenuTreeDirectory *directory)
{
    CMenuTreeIter *iter;

    g_return_val_if_fail (directory != NULL, NULL);

    iter = g_slice_new0 (CMenuTreeIter);
    iter->refcount = 1;

    iter->contents = g_slist_copy (directory->contents);
    iter->contents_iter = iter->contents;
    g_slist_foreach (iter->contents, (GFunc) cmenu_tree_item_ref, NULL);

    return iter;
}

const char *
cmenu_tree_directory_get_name (CMenuTreeDirectory *directory)
{
    g_return_val_if_fail (directory != NULL, NULL);

    if (!directory->directory_entry)
        return directory->name;

    return desktop_entry_get_name (directory->directory_entry);
}

const char *
cmenu_tree_directory_get_generic_name (CMenuTreeDirectory *directory)
{
    g_return_val_if_fail (directory != NULL, NULL);

    if (!directory->directory_entry)
        return NULL;

    return desktop_entry_get_generic_name (directory->directory_entry);
}

const char *
cmenu_tree_directory_get_comment (CMenuTreeDirectory *directory)
{
    g_return_val_if_fail (directory != NULL, NULL);

    if (!directory->directory_entry)
        return NULL;

    return desktop_entry_get_comment (directory->directory_entry);
}

/**
 * cmenu_tree_directory_get_icon:
 * @directory: a #CMenuTreeDirectory
 *
 * Gets the icon for the directory.
 *
 * Returns: (transfer none): The #GIcon for this directory
 */
GIcon *
cmenu_tree_directory_get_icon (CMenuTreeDirectory *directory)
{
    g_return_val_if_fail (directory != NULL, NULL);

    if (!directory->directory_entry)
        return NULL;

    return desktop_entry_get_icon (directory->directory_entry);
}

const char *
cmenu_tree_directory_get_desktop_file_path (CMenuTreeDirectory *directory)
{
    g_return_val_if_fail (directory != NULL, NULL);

    if (!directory->directory_entry)
        return NULL;

    return desktop_entry_get_path (directory->directory_entry);
}

const char *
cmenu_tree_directory_get_menu_id (CMenuTreeDirectory *directory)
{
    g_return_val_if_fail (directory != NULL, NULL);

    return directory->name;
}

gboolean
cmenu_tree_directory_get_is_nodisplay (CMenuTreeDirectory *directory)
{
    g_return_val_if_fail (directory != NULL, FALSE);

    return directory->is_nodisplay;
}

void
cmenu_tree_directory_set_is_nodisplay (CMenuTreeDirectory *directory,
                                       gboolean            nodisplay)
{
    directory->is_nodisplay = nodisplay;
}

static void
append_directory_path (CMenuTreeDirectory *directory,
                       GString            *path)
{
    if (!directory->item.parent)
    {
        g_string_append_c (path, G_DIR_SEPARATOR);
        return;
    }

    append_directory_path (directory->item.parent, path);

    g_string_append (path, directory->name);
    g_string_append_c (path, G_DIR_SEPARATOR);
}

char *
cmenu_tree_directory_make_path (CMenuTreeDirectory *directory,
                                CMenuTreeEntry     *entry)
{
    GString *path;

    g_return_val_if_fail (directory != NULL, NULL);

    path = g_string_new (NULL);

    append_directory_path (directory, path);

    if (entry != NULL)
        g_string_append (path, desktop_entry_get_basename (entry->desktop_entry));

    return g_string_free (path, FALSE);
}

CMenuTreeDirectory *
cmenu_tree_directory_new (CMenuTreeDirectory *parent,
                          const char         *name)
{
    CMenuTreeDirectory *retval;

    retval = g_slice_new0 (CMenuTreeDirectory);

    retval->item.type     = CMENU_TREE_ITEM_DIRECTORY;
    retval->item.parent   = parent;
    retval->item.refcount = 1;

    retval->name                = g_strdup (name);
    retval->directory_entry     = NULL;
    retval->entries             = NULL;
    retval->subdirs             = NULL;
    retval->default_layout_info = NULL;
    retval->layout_info         = NULL;
    retval->contents            = NULL;
    retval->only_unallocated    = FALSE;
    retval->is_nodisplay        = FALSE;

    return retval;
}

void
cmenu_tree_directory_finalize (CMenuTreeDirectory *directory)
{
    g_assert (directory->item.refcount == 0);

    g_slist_foreach (directory->contents,
                     (GFunc) cmenu_tree_item_unref_and_unset_parent,
                     NULL);
    g_slist_free (directory->contents);
    directory->contents = NULL;

    g_slist_foreach (directory->default_layout_info,
                     (GFunc) menu_layout_node_unref,
                     NULL);
    g_slist_free (directory->default_layout_info);
    directory->default_layout_info = NULL;

    g_slist_foreach (directory->layout_info,
                     (GFunc) menu_layout_node_unref,
                     NULL);
    g_slist_free (directory->layout_info);
    directory->layout_info = NULL;

    g_slist_foreach (directory->subdirs,
                     (GFunc) cmenu_tree_item_unref_and_unset_parent,
                     NULL);
    g_slist_free (directory->subdirs);
    directory->subdirs = NULL;

    g_slist_foreach (directory->entries,
                     (GFunc) cmenu_tree_item_unref_and_unset_parent,
                     NULL);
    g_slist_free (directory->entries);
    directory->entries = NULL;

    if (directory->directory_entry)
    desktop_entry_unref (directory->directory_entry);
    directory->directory_entry = NULL;

    g_free (directory->name);
    directory->name = NULL;

    g_slice_free (CMenuTreeDirectory, directory);
}

GType
cmenu_tree_directory_get_type (void)
{
    static GType gtype = G_TYPE_INVALID;
    if (gtype == G_TYPE_INVALID)
    {
        gtype = g_boxed_type_register_static ("CMenuTreeDirectory",
                                              (GBoxedCopyFunc) cmenu_tree_item_ref,
                                              (GBoxedFreeFunc) cmenu_tree_item_unref);
    }
    return gtype;
}

void
cmenu_tree_directory_set_default_layout_info (CMenuTreeDirectory *directory,
                                              GSList             *default_layout_info)
{
    g_slist_foreach (directory->default_layout_info,
                     (GFunc) menu_layout_node_unref,
                     NULL);
    g_slist_free (directory->default_layout_info);

    directory->default_layout_info = default_layout_info;
}

void
cmenu_tree_directory_set_layout_info (CMenuTreeDirectory *directory,
                                      GSList             *layout_info)
{
    g_slist_foreach (directory->layout_info,
                     (GFunc) menu_layout_node_unref,
                     NULL);
    g_slist_free (directory->layout_info);

    directory->layout_info = layout_info;
}

GSList *
cmenu_tree_directory_get_layout (CMenuTreeDirectory *directory)
{
    CMenuTreeDirectory *iter;

    if (directory->layout_info != NULL)
    {
        return directory->layout_info;
    }

    iter = directory;
    while (iter != NULL)
    {
        if (iter->default_layout_info != NULL)
        {
            return iter->default_layout_info;
        }

        iter = CMENU_TREE_ITEM (iter)->parent;
    }

    return NULL;
}

gboolean
cmenu_tree_directory_get_only_unallocated (CMenuTreeDirectory *directory)
{
    return directory->only_unallocated;
}

void
cmenu_tree_directory_set_only_unallocated (CMenuTreeDirectory *directory,
                                           gboolean            only_unallocated)
{
    directory->only_unallocated = only_unallocated;
}

DesktopEntry *
cmenu_tree_directory_get_directory_entry (CMenuTreeDirectory *directory)
{
    return directory->directory_entry;
}

void
cmenu_tree_directory_set_directory_entry (CMenuTreeDirectory *directory,
                                          DesktopEntry       *entry)
{
    if (directory->directory_entry)
        desktop_entry_unref (directory->directory_entry);
    directory->directory_entry = desktop_entry_ref (entry);
}

/**
 * cmenu_tree_entry_get_parent:
 * @entry: a #CMenuTreeEntry
 *
 * Returns: (transfer full): The parent directory, or %NULL if none
 */
CMenuTreeDirectory *
cmenu_tree_entry_get_parent (CMenuTreeEntry *entry)
{
    return get_parent ((CMenuTreeItem *)entry);
}

/**
 * cmenu_tree_entry_get_app_info:
 * @entry: a #CMenuTreeEntry
 *
 * Returns: (transfer none): The #GDesktopAppInfo for this entry
 */
GDesktopAppInfo *
cmenu_tree_entry_get_app_info (CMenuTreeEntry *entry)
{
    g_return_val_if_fail (entry != NULL, NULL);

    return desktop_entry_get_app_info (entry->desktop_entry);
}

const char *
cmenu_tree_entry_get_desktop_file_path (CMenuTreeEntry *entry)
{
    g_return_val_if_fail (entry != NULL, NULL);

    return desktop_entry_get_path (entry->desktop_entry);
}

const char *
cmenu_tree_entry_get_desktop_file_id (CMenuTreeEntry *entry)
{
    g_return_val_if_fail (entry != NULL, NULL);

    return entry->desktop_file_id;
}

gboolean
cmenu_tree_entry_get_is_nodisplay_recurse (CMenuTreeEntry *entry)
{
    CMenuTreeDirectory *directory;
    GDesktopAppInfo *app_info;

    g_return_val_if_fail (entry != NULL, FALSE);

    app_info = cmenu_tree_entry_get_app_info (entry);

    if (g_desktop_app_info_get_nodisplay (app_info))
        return TRUE;

    directory = entry->item.parent;
    while (directory != NULL)
    {
        if (directory->is_nodisplay)
            return TRUE;

        directory = directory->item.parent;
    }

    return FALSE;
}

gboolean
cmenu_tree_entry_get_is_excluded (CMenuTreeEntry *entry)
{
    g_return_val_if_fail (entry != NULL, FALSE);

    return entry->is_excluded;
}

gboolean
cmenu_tree_entry_get_is_unallocated (CMenuTreeEntry *entry)
{
    g_return_val_if_fail (entry != NULL, FALSE);

    return entry->is_unallocated;
}

CMenuTreeEntry *
cmenu_tree_entry_new (CMenuTreeDirectory *parent,
                      DesktopEntry       *desktop_entry,
                      const char         *desktop_file_id,
                      gboolean            is_excluded,
                      gboolean            is_unallocated)
{
    CMenuTreeEntry *retval;

    retval = g_slice_new0 (CMenuTreeEntry);

    retval->item.type     = CMENU_TREE_ITEM_ENTRY;
    retval->item.parent   = parent;
    retval->item.refcount = 1;

    retval->desktop_entry   = desktop_entry_ref (desktop_entry);
    retval->desktop_file_id = g_strdup (desktop_file_id);
    retval->is_excluded     = is_excluded != FALSE;
    retval->is_unallocated  = is_unallocated != FALSE;

    return retval;
}

void
cmenu_tree_entry_finalize (CMenuTreeEntry *entry)
{
    g_assert (entry->item.refcount == 0);

    g_free (entry->desktop_file_id);
    entry->desktop_file_id = NULL;

    if (entry->desktop_entry)
        desktop_entry_unref (entry->desktop_entry);
    entry->desktop_entry = NULL;

    g_slice_free (CMenuTreeEntry, entry);
}

int
cmenu_tree_entry_compare_by_id (CMenuTreeItem *a,
                                CMenuTreeItem *b)
{
    return strcmp (CMENU_TREE_ENTRY (a)->desktop_file_id,
                   CMENU_TREE_ENTRY (b)->desktop_file_id);
}

GType
cmenu_tree_entry_get_type (void)
{
    static GType gtype = G_TYPE_INVALID;
    if (gtype == G_TYPE_INVALID)
    {
        gtype = g_boxed_type_register_static ("CMenuTreeEntry",
                                              (GBoxedCopyFunc) cmenu_tree_item_ref,
                                              (GBoxedFreeFunc) cmenu_tree_item_unref);
    }
    return gtype;
}

DesktopEntry *
cmenu_tree_entry_get_desktop_entry (CMenuTreeEntry *entry)
{
    return entry->desktop_entry;
}

/**
 * cmenu_tree_iter_ref: (skip)
 * @iter: iter
 *
 * Increment the reference count of @iter
 */
CMenuTreeIter *
cmenu_tree_iter_ref (CMenuTreeIter *iter)
{
    g_atomic_int_inc (&iter->refcount);
    return iter;
}

/**
 * cmenu_tree_iter_unref: (skip)
 * @iter: iter
 *
 * Decrement the reference count of @iter
 */
void
cmenu_tree_iter_unref (CMenuTreeIter *iter)
{
    if (!g_atomic_int_dec_and_test (&iter->refcount))
        return;

    g_slist_foreach (iter->contents, (GFunc)cmenu_tree_item_unref, NULL);
    g_slist_free (iter->contents);

    g_slice_free (CMenuTreeIter, iter);
}

/**
 * cmenu_tree_iter_next:
 * @iter: iter
 *
 * Change the iterator to the next item, and return its type.  If
 * there are no more items, %CMENU_TREE_ITEM_INVALID is returned.
 *
 * Returns: The type of the next item that can be retrieved from the iterator
 */
CMenuTreeItemType
cmenu_tree_iter_next (CMenuTreeIter *iter)
{
    g_return_val_if_fail (iter != NULL, CMENU_TREE_ITEM_INVALID);

    if (iter->contents_iter)
    {
        iter->item = iter->contents_iter->data;
        iter->contents_iter = iter->contents_iter->next;
        return iter->item->type;
    }
    else
        return CMENU_TREE_ITEM_INVALID;
}

/**
 * cmenu_tree_iter_get_directory:
 * @iter: iter
 *
 * This method may only be called if cmenu_tree_iter_next()
 * returned CMENU_TREE_ITEM_DIRECTORY.
 *
 * Returns: (transfer full): A directory
 */
CMenuTreeDirectory *
cmenu_tree_iter_get_directory (CMenuTreeIter *iter)
{
    g_return_val_if_fail (iter != NULL, NULL);
    g_return_val_if_fail (iter->item != NULL, NULL);
    g_return_val_if_fail (iter->item->type == CMENU_TREE_ITEM_DIRECTORY, NULL);

    return (CMenuTreeDirectory*)cmenu_tree_item_ref (iter->item);
}

/**
 * cmenu_tree_iter_get_entry:
 * @iter: iter
 *
 * This method may only be called if cmenu_tree_iter_next()
 * returned CMENU_TREE_ITEM_ENTRY.
 *
 * Returns: (transfer full): An entry
 */
CMenuTreeEntry *
cmenu_tree_iter_get_entry (CMenuTreeIter *iter)
{
    g_return_val_if_fail (iter != NULL, NULL);
    g_return_val_if_fail (iter->item != NULL, NULL);
    g_return_val_if_fail (iter->item->type == CMENU_TREE_ITEM_ENTRY, NULL);

    return (CMenuTreeEntry*)cmenu_tree_item_ref (iter->item);
}

GType
cmenu_tree_iter_get_type (void)
{
    static GType gtype = G_TYPE_INVALID;
    if (gtype == G_TYPE_INVALID)
    {
        gtype = g_boxed_type_register_static ("CMenuTreeIter",
                                              (GBoxedCopyFunc) cmenu_tree_iter_ref,
                                              (GBoxedFreeFunc) cmenu_tree_iter_unref);
    }
    return gtype;
}
