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

#include <config.h>

#include "cmenu-tree.h"

#include <string.h>
#include <errno.h>
#include <stdlib.h>

#include "menu-layout.h"
#include "menu-monitor.h"
#include "menu-util.h"

#include "cmenu-tree-enums.h"
#include "cmenu-tree-item-private.h"

enum {
    PROP_0,
    PROP_MENU_BASENAME,
    PROP_MENU_PATH,
    PROP_FLAGS
};

/* Signals */
enum
{
    CHANGED,
    LAST_SIGNAL
};

static guint cmenu_tree_signals [LAST_SIGNAL] = { 0 };

struct _CMenuTree
{
    GObject       parent_instance;

    char *basename;
    char *non_prefixed_basename;
    char *path;
    char *canonical_path;

    CMenuTreeFlags flags;

    GSList *menu_file_monitors;

    MenuLayoutNode *layout;
    CMenuTreeDirectory *root;
    GHashTable *entries_by_id;

    guint canonical : 1;
    guint loaded    : 1;
};

G_DEFINE_TYPE (CMenuTree, cmenu_tree, G_TYPE_OBJECT)

static gboolean  cmenu_tree_load_layout          (CMenuTree       *tree,
                                                  GError         **error);
static void      cmenu_tree_force_reload         (CMenuTree       *tree);
static gboolean  cmenu_tree_build_from_layout    (CMenuTree       *tree,
                                                  GError         **error);
static void      cmenu_tree_force_rebuild        (CMenuTree       *tree);
static void      cmenu_tree_resolve_files        (CMenuTree       *tree,
                                                  GHashTable      *loaded_menu_files,
                                                  MenuLayoutNode  *layout);
static void      cmenu_tree_force_recanonicalize (CMenuTree       *tree);
static void      cmenu_tree_invoke_monitors      (CMenuTree       *tree);

static void      process_layout_info             (CMenuTree          *tree,
                                                  CMenuTreeDirectory *directory);
static void      preprocess_layout_info          (CMenuTree          *tree,
                                                  CMenuTreeDirectory *directory);

typedef enum
{
    MENU_FILE_MONITOR_INVALID = 0,
    MENU_FILE_MONITOR_FILE,
    MENU_FILE_MONITOR_NONEXISTENT_FILE,
    MENU_FILE_MONITOR_DIRECTORY
} MenuFileMonitorType;

typedef struct
{
    MenuFileMonitorType  type;
    MenuMonitor         *monitor;
} MenuFileMonitor;

static void
handle_nonexistent_menu_file_changed (MenuMonitor      *monitor,
                                      MenuMonitorEvent  event,
                                      const char       *path,
                                      CMenuTree        *tree)
{
    if (event == MENU_MONITOR_EVENT_CHANGED ||
        event == MENU_MONITOR_EVENT_CREATED)
    {
        menu_verbose ("\"%s\" %s, marking tree for recanonicalization\n",
                      path,
                      event == MENU_MONITOR_EVENT_CREATED ? "created" : "changed");

        cmenu_tree_force_recanonicalize (tree);
        cmenu_tree_invoke_monitors (tree);
    }
}

static void
handle_menu_file_changed (MenuMonitor      *monitor,
                          MenuMonitorEvent  event,
                          const char       *path,
                          CMenuTree        *tree)
{
    menu_verbose ("\"%s\" %s, marking tree for recanicalization\n",
                  path,
                  event == MENU_MONITOR_EVENT_CREATED ? "created" :
                  event == MENU_MONITOR_EVENT_CHANGED ? "changed" : "deleted");

    cmenu_tree_force_recanonicalize (tree);
    cmenu_tree_invoke_monitors (tree);
}

static void
handle_menu_file_directory_changed (MenuMonitor      *monitor,
                                    MenuMonitorEvent  event,
                                    const char       *path,
                                    CMenuTree        *tree)
{
    if (!g_str_has_suffix (path, ".menu"))
        return;

      menu_verbose ("\"%s\" %s, marking tree for recanicalization\n",
                    path,
                    event == MENU_MONITOR_EVENT_CREATED ? "created" :
                    event == MENU_MONITOR_EVENT_CHANGED ? "changed" : "deleted");

    cmenu_tree_force_recanonicalize (tree);
    cmenu_tree_invoke_monitors (tree);
}

static void
cmenu_tree_add_menu_file_monitor (CMenuTree           *tree,
                                  const char          *path,
                                  MenuFileMonitorType  type)
{
    MenuFileMonitor *monitor;

    monitor = g_slice_new0 (MenuFileMonitor);

    monitor->type = type;

    switch (type)
    {
        case MENU_FILE_MONITOR_FILE:
            menu_verbose ("Adding a menu file monitor for \"%s\"\n", path);

            monitor->monitor = menu_get_file_monitor (path);
            menu_monitor_add_notify (monitor->monitor,
                                     (MenuMonitorNotifyFunc) handle_menu_file_changed,
                                     tree);
            break;

        case MENU_FILE_MONITOR_NONEXISTENT_FILE:
            menu_verbose ("Adding a menu file monitor for non-existent \"%s\"\n", path);

            monitor->monitor = menu_get_file_monitor (path);
            menu_monitor_add_notify (monitor->monitor,
                                     (MenuMonitorNotifyFunc) handle_nonexistent_menu_file_changed,
                                     tree);
            break;

        case MENU_FILE_MONITOR_DIRECTORY:
            menu_verbose ("Adding a menu directory monitor for \"%s\"\n", path);

            monitor->monitor = menu_get_directory_monitor (path);
            menu_monitor_add_notify (monitor->monitor,
                                     (MenuMonitorNotifyFunc) handle_menu_file_directory_changed,
                                     tree);
            break;

        default:
            g_assert_not_reached ();
            break;
    }

    tree->menu_file_monitors = g_slist_prepend (tree->menu_file_monitors, monitor);
}

static void
remove_menu_file_monitor (MenuFileMonitor *monitor,
                          CMenuTree       *tree)
{
    switch (monitor->type)
    {
        case MENU_FILE_MONITOR_FILE:
            menu_monitor_remove_notify (monitor->monitor,
                                        (MenuMonitorNotifyFunc) handle_menu_file_changed,
                                        tree);
            break;

        case MENU_FILE_MONITOR_NONEXISTENT_FILE:
            menu_monitor_remove_notify (monitor->monitor,
                                        (MenuMonitorNotifyFunc) handle_nonexistent_menu_file_changed,
                                        tree);
            break;

        case MENU_FILE_MONITOR_DIRECTORY:
            menu_monitor_remove_notify (monitor->monitor,
                                        (MenuMonitorNotifyFunc) handle_menu_file_directory_changed,
                                        tree);
            break;

        default:
            g_assert_not_reached ();
            break;
    }

    menu_monitor_unref (monitor->monitor);
    monitor->monitor = NULL;

    monitor->type = MENU_FILE_MONITOR_INVALID;

    g_slice_free (MenuFileMonitor, monitor);
}

static void
cmenu_tree_remove_menu_file_monitors (CMenuTree *tree)
{
    menu_verbose ("Removing all menu file monitors\n");

    g_slist_foreach (tree->menu_file_monitors,
                     (GFunc) remove_menu_file_monitor,
                     tree);
    g_slist_free (tree->menu_file_monitors);
    tree->menu_file_monitors = NULL;
}

static gboolean
canonicalize_path (CMenuTree  *tree,
                   const char *path)
{
    tree->canonical_path = realpath (path, NULL);
    if (tree->canonical_path)
    {
        tree->canonical = TRUE;
        cmenu_tree_add_menu_file_monitor (tree,
                                          tree->canonical_path,
                                          MENU_FILE_MONITOR_FILE);
    }
    else
    {
        cmenu_tree_add_menu_file_monitor (tree,
                                          path,
                                          MENU_FILE_MONITOR_NONEXISTENT_FILE);
    }

    return tree->canonical;
}

static gboolean
canonicalize_basename_with_config_dir (CMenuTree   *tree,
                                       const char *basename,
                                       const char *config_dir)
{
    gboolean  ret;
    char     *path;

    path = g_build_filename (config_dir, "menus",  basename,  NULL);
    ret = canonicalize_path (tree, path);
    g_free (path);

    return ret;
}

static void
canonicalize_basename (CMenuTree  *tree,
                       const char *basename)
{
    if (!canonicalize_basename_with_config_dir (tree,
                                                basename,
                                                g_get_user_config_dir ()))
    {
        const char * const *system_config_dirs;
        int                 i;

        system_config_dirs = g_get_system_config_dirs ();

        i = 0;
        while (system_config_dirs[i] != NULL)
        {
            if (canonicalize_basename_with_config_dir (tree,
                                                       basename,
                                                       system_config_dirs[i]))
            break;

            ++i;
        }
    }
}

static char *
prefix_menu_name (const char *orig_name)
{
    char *prefix;
    prefix = (char *) g_getenv ("XDG_MENU_PREFIX");
    if (prefix == NULL)
        prefix = "gnome-";
    return g_strconcat (prefix, orig_name, NULL);
}

static gboolean
cmenu_tree_canonicalize_path (CMenuTree *tree,
                              GError   **error)
{
    const char *menu_file = NULL;

    if (tree->canonical)
        return TRUE;

    g_assert (tree->canonical_path == NULL);

    cmenu_tree_remove_menu_file_monitors (tree);

    if (tree->path)
    {
        menu_file = tree->path;
        canonicalize_path (tree, tree->path);
    }
    else
    {
        const gchar *xdg_menu_prefix;

        menu_file = tree->basename;
        xdg_menu_prefix = g_getenv ("XDG_MENU_PREFIX");

        if (xdg_menu_prefix == NULL)
            xdg_menu_prefix = "gnome-";

        if (xdg_menu_prefix != NULL)
        {
            gchar *prefixed_basename;

            prefixed_basename = g_strdup_printf ("%sapplications.menu",
                                                 xdg_menu_prefix);

            /* Some gnome-menus using applications just use "applications.menu"
             * as the basename and expect gnome-menus to prefix it. Others (e.g.
             * Alacarte) explicitly use "${XDG_MENU_PREFIX}applications.menu" as
             * the basename, because they want to save changes to the right files
             * in ~. In both cases, we want to use "applications-merged" as the
             * merge directory (as required by the fd.o menu spec), so we save
             * the non-prefixed basename and use it later when calling
             * menu_layout_load().
             */
            if (!g_strcmp0 (tree->basename, "applications.menu") ||
                !g_strcmp0 (tree->basename, prefixed_basename))
            {
                canonicalize_basename (tree, prefixed_basename);
                g_free (tree->non_prefixed_basename);
                tree->non_prefixed_basename = g_strdup ("applications.menu");
            }
            g_free (prefixed_basename);
        }

        if (!tree->canonical)
            canonicalize_basename (tree, tree->basename);
    }

    if (tree->canonical)
    {
        menu_verbose ("Successfully looked up menu_file for \"%s\": %s\n",
                      menu_file, tree->canonical_path);
        return TRUE;
    }
    else
    {
        g_set_error (error,
                     G_IO_ERROR,
                     G_IO_ERROR_FAILED,
                     "Failed to look up menu_file for \"%s\"\n",
                     menu_file);
        return FALSE;
    }
}

static void
cmenu_tree_force_recanonicalize (CMenuTree *tree)
{
    cmenu_tree_remove_menu_file_monitors (tree);

    if (tree->canonical)
    {
        cmenu_tree_force_reload (tree);

        g_free (tree->canonical_path);
        tree->canonical_path = NULL;

        tree->canonical = FALSE;
    }
}

/**
 * cmenu_tree_new:
 * @menu_basename: Basename of menu file
 * @flags: Flags controlling menu content
 *
 * Returns: (transfer full): A new #CMenuTree instance
 */
CMenuTree *
cmenu_tree_new (const char     *menu_basename,
                CMenuTreeFlags  flags)
{
    g_return_val_if_fail (menu_basename != NULL, NULL);

    return g_object_new (CMENU_TYPE_TREE,
                         "menu-basename", menu_basename,
                         "flags", flags,
                         NULL);
}

/**
 * cmenu_tree_new_fo_path:
 * @menu_path: Path of menu file
 * @flags: Flags controlling menu content
 *
 * Returns: (transfer full): A new #CMenuTree instance
 */
CMenuTree *
cmenu_tree_new_for_path (const char     *menu_path,
                         CMenuTreeFlags  flags)
{
    g_return_val_if_fail (menu_path != NULL, NULL);

    return g_object_new (CMENU_TYPE_TREE,
                         "menu-path", menu_path,
                         "flags", flags,
                         NULL);
}

static GObject *
cmenu_tree_constructor (GType                  type,
                        guint                  n_construct_properties,
                        GObjectConstructParam *construct_properties)
{
    GObject   *obj;
    CMenuTree *self;

    obj = G_OBJECT_CLASS (cmenu_tree_parent_class)->constructor (type,
                                                                 n_construct_properties,
                                                                 construct_properties);

    /* If CMenuTree:menu-path is set, then we should make sure that
     * CMenuTree:menu-basename is unset (especially as it has a default
     * value). This has to be done here, in the constructor, since the
     * properties are construct-only. */

    self = CMENU_TREE (obj);

    if (self->path != NULL)
        g_object_set (self, "menu-basename", NULL, NULL);

    return obj;
}

static void
cmenu_tree_set_property (GObject         *object,
                         guint            prop_id,
                         const GValue    *value,
                         GParamSpec      *pspec)
{
    CMenuTree *self = CMENU_TREE (object);

    switch (prop_id)
    {
        case PROP_MENU_BASENAME:
            self->basename = g_value_dup_string (value);
            break;

        case PROP_MENU_PATH:
            self->path = g_value_dup_string (value);
            break;

        case PROP_FLAGS:
            self->flags = g_value_get_flags (value);
            break;

        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
            break;
    }
}

static void
cmenu_tree_get_property (GObject         *object,
                         guint            prop_id,
                         GValue          *value,
                         GParamSpec      *pspec)
{
    CMenuTree *self = CMENU_TREE (object);

    switch (prop_id)
    {
        case PROP_MENU_BASENAME:
            g_value_set_string (value, self->basename);
            break;
        case PROP_MENU_PATH:
            g_value_set_string (value, self->path);
            break;
        case PROP_FLAGS:
            g_value_set_flags (value, self->flags);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
            break;
    }
}

static void
cmenu_tree_finalize (GObject *object)
{
    CMenuTree *tree = CMENU_TREE (object);

    cmenu_tree_force_recanonicalize (tree);

    if (tree->basename != NULL)
        g_free (tree->basename);
    tree->basename = NULL;

    g_free (tree->non_prefixed_basename);
    tree->non_prefixed_basename = NULL;

    if (tree->path != NULL)
        g_free (tree->path);
    tree->path = NULL;

    if (tree->canonical_path != NULL)
        g_free (tree->canonical_path);
    tree->canonical_path = NULL;

    g_hash_table_destroy (tree->entries_by_id);
    tree->entries_by_id = NULL;

    G_OBJECT_CLASS (cmenu_tree_parent_class)->finalize (object);
}

static void
cmenu_tree_init (CMenuTree *self)
{
    self->entries_by_id = g_hash_table_new (g_str_hash, g_str_equal);
}

static void
cmenu_tree_class_init (CMenuTreeClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

    gobject_class->constructor = cmenu_tree_constructor;
    gobject_class->get_property = cmenu_tree_get_property;
    gobject_class->set_property = cmenu_tree_set_property;
    gobject_class->finalize = cmenu_tree_finalize;

    /**
    * CMenuTree:menu-basename:
    *
    * The name of the menu file; must be a basename or a relative path. The file
    * will be looked up in $XDG_CONFIG_DIRS/menus/. See the Desktop Menu
    * specification.
    */
    g_object_class_install_property (gobject_class,
                                     PROP_MENU_BASENAME,
                                     g_param_spec_string ("menu-basename", "", "",
                                                          "applications.menu",
                                                          G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
    /**
     * CMenuTree:menu-path:
     *
     * The full path of the menu file. If set, CMenuTree:menu-basename will get
     * ignored.
     */
    g_object_class_install_property (gobject_class,
                                     PROP_MENU_PATH,
                                     g_param_spec_string ("menu-path", "", "",
                                                          NULL,
                                                          G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
    /**
     * CMenuTree:flags:
     *
     * Flags controlling the content of the menu.
     */
    g_object_class_install_property (gobject_class,
                                     PROP_FLAGS,
                                     g_param_spec_flags ("flags", "", "",
                                                         CMENU_TYPE_TREE_FLAGS,
                                                         CMENU_TREE_FLAGS_NONE,
                                                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

    /**
     * CMenuTree:changed:
     *
     * This signal is emitted when applications are added, removed, or
     * upgraded.  But note the new data will only be visible after
     * cmenu_tree_load_sync() or a variant thereof is invoked.
     */
    cmenu_tree_signals[CHANGED] =
        g_signal_new ("changed",
                      G_TYPE_FROM_CLASS (klass),
                      G_SIGNAL_RUN_LAST,
                      0,
                      NULL, NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE, 0);
}

/**
 * cmenu_tree_get_canonical_menu_path:
 * @tree: a #CMenuTree
 *
 * This function is only available if the tree has been loaded via
 * cmenu_tree_load_sync() or a variant thereof.
 *
 * Returns: The absolute and canonicalized path to the loaded menu file
 */
const char *
cmenu_tree_get_canonical_menu_path (CMenuTree *tree)
{
    g_return_val_if_fail (CMENU_IS_TREE (tree), NULL);
    g_return_val_if_fail (tree->loaded, NULL);

    return tree->canonical_path;
}

/**
 * cmenu_tree_load_sync:
 * @tree: a #CMenuTree
 * @error: a #GError
 *
 * Synchronously load the menu contents.  This function
 * performs a significant amount of blocking I/O if the
 * tree has not been loaded yet.
 *
 * Returns: %TRUE on success, %FALSE on error
 */
gboolean
cmenu_tree_load_sync (CMenuTree  *tree,
                      GError    **error)
{
    GError *local_error = NULL;

    if (tree->loaded)
        return TRUE;

    if (!cmenu_tree_build_from_layout (tree, &local_error))
    {
        if (local_error)
            g_propagate_error (error, local_error);
        return FALSE;
    }

    tree->loaded = TRUE;

    return TRUE;
}

/**
 * cmenu_tree_get_root_directory:
 * @tree: a #CMenuTree
 *
 * Get the root directory; you must have loaded the tree first (at
 * least once) via cmenu_tree_load_sync() or a variant thereof.
 *
 * Returns: (transfer full): Root of the tree
 */
CMenuTreeDirectory *
cmenu_tree_get_root_directory (CMenuTree *tree)
{
    g_return_val_if_fail (tree != NULL, NULL);
    g_return_val_if_fail (tree->loaded, NULL);

    return cmenu_tree_item_ref (tree->root);
}

static CMenuTreeDirectory *
find_path (CMenuTreeDirectory *directory,
           const char         *path)
{
    const char *name;
    char       *slash;
    char       *freeme;
    GSList     *tmp;

    while (path[0] == G_DIR_SEPARATOR) path++;

    if (path[0] == '\0')
        return directory;

    freeme = NULL;
    slash = strchr (path, G_DIR_SEPARATOR);
    if (slash)
    {
        name = freeme = g_strndup (path, slash - path);
        path = slash + 1;
    }
    else
    {
        name = path;
        path = NULL;
    }

    tmp = directory->contents;
    while (tmp != NULL)
    {
        CMenuTreeItem *item = tmp->data;

        if (cmenu_tree_item_get_item_type (item) != CMENU_TREE_ITEM_DIRECTORY)
        {
            tmp = tmp->next;
            continue;
        }

        if (!strcmp (name, cmenu_tree_directory_get_name (CMENU_TREE_DIRECTORY (item))))
        {
            g_free (freeme);

            if (path)
                return find_path (CMENU_TREE_DIRECTORY (item), path);
            else
                return CMENU_TREE_DIRECTORY (item);
        }

        tmp = tmp->next;
    }

    g_free (freeme);

    return NULL;
}

CMenuTreeDirectory *
cmenu_tree_get_directory_from_path (CMenuTree  *tree,
                                    const char *path)
{
    CMenuTreeDirectory *root;
    CMenuTreeDirectory *directory;

    g_return_val_if_fail (tree != NULL, NULL);
    g_return_val_if_fail (path != NULL, NULL);

    if (path[0] != G_DIR_SEPARATOR)
        return NULL;

    if (!(root = cmenu_tree_get_root_directory (tree)))
        return NULL;

    directory = find_path (root, path);

    cmenu_tree_item_unref (root);

    return directory ? cmenu_tree_item_ref (directory) : NULL;
}

/**
 * cmenu_tree_get_entry_by_id:
 * @tree: a #CMenuTree
 * @id: a desktop file ID
 *
 * Look up the entry corresponding to the given "desktop file id".
 *
 * Returns: (transfer full): A newly referenced #CMenuTreeEntry, or %NULL if none
 */
CMenuTreeEntry *
cmenu_tree_get_entry_by_id (CMenuTree  *tree,
                            const char *id)
{
    CMenuTreeEntry *entry;

    g_return_val_if_fail (tree->loaded, NULL);

    entry = g_hash_table_lookup (tree->entries_by_id, id);
    if (entry != NULL)
        cmenu_tree_item_ref (entry);

    return entry;
}

static void
cmenu_tree_invoke_monitors (CMenuTree *tree)
{
    g_signal_emit (tree, cmenu_tree_signals[CHANGED], 0);
}

static MenuLayoutNode *
find_menu_child (MenuLayoutNode *layout)
{
    MenuLayoutNode *child;

    child = menu_layout_node_get_children (layout);
    while (child && menu_layout_node_get_type (child) != MENU_LAYOUT_NODE_MENU)
        child = menu_layout_node_get_next (child);

    return child;
}

static void
merge_resolved_children (CMenuTree      *tree,
                         GHashTable     *loaded_menu_files,
                         MenuLayoutNode *where,
                         MenuLayoutNode *from)
{
    MenuLayoutNode *insert_after;
    MenuLayoutNode *menu_child;
    MenuLayoutNode *from_child;

    cmenu_tree_resolve_files (tree, loaded_menu_files, from);

    insert_after = where;
    g_assert (menu_layout_node_get_type (insert_after) != MENU_LAYOUT_NODE_ROOT);
    g_assert (menu_layout_node_get_parent (insert_after) != NULL);

    /* skip root node */
    menu_child = find_menu_child (from);
    g_assert (menu_child != NULL);
    g_assert (menu_layout_node_get_type (menu_child) == MENU_LAYOUT_NODE_MENU);

    /* merge children of toplevel <Menu> */
    from_child = menu_layout_node_get_children (menu_child);
    while (from_child != NULL)
    {
        MenuLayoutNode *next;

        next = menu_layout_node_get_next (from_child);

        menu_verbose ("Merging ");
        menu_debug_print_layout (from_child, FALSE);
        menu_verbose (" after ");
        menu_debug_print_layout (insert_after, FALSE);

        switch (menu_layout_node_get_type (from_child))
        {
            case MENU_LAYOUT_NODE_NAME:
                menu_layout_node_unlink (from_child); /* delete this */
                break;

            default:
                menu_layout_node_steal (from_child);
                menu_layout_node_insert_after (insert_after, from_child);
                menu_layout_node_unref (from_child);

                insert_after = from_child;
                break;
        }

        from_child = next;
    }
}

static gboolean
load_merge_file (CMenuTree      *tree,
                 GHashTable     *loaded_menu_files,
                 const char     *filename,
                 gboolean        is_canonical,
                 gboolean        add_monitor,
                 MenuLayoutNode *where)
{
    MenuLayoutNode *to_merge;
    const char     *canonical;
    char           *freeme;
    gboolean        retval;

    freeme = NULL;
    retval = FALSE;

    if (!is_canonical)
    {
        canonical = freeme = realpath (filename, NULL);
        if (canonical == NULL)
        {
            if (add_monitor)
              cmenu_tree_add_menu_file_monitor (tree,
                                                filename,
                                                MENU_FILE_MONITOR_NONEXISTENT_FILE);

            menu_verbose ("Failed to canonicalize merge file path \"%s\": %s\n",
                          filename, g_strerror (errno));
            goto out;
        }
    }
    else
    {
        canonical = filename;
    }

    if (g_hash_table_lookup (loaded_menu_files, canonical) != NULL)
    {
        g_warning ("Not loading \"%s\": recursive loop detected in .menu files",
                   canonical);
        retval = TRUE;
        goto out;
    }

    menu_verbose ("Merging file \"%s\"\n", canonical);

    to_merge = menu_layout_load (canonical, tree->non_prefixed_basename, NULL);
    if (to_merge == NULL)
    {
        menu_verbose ("No menu for file \"%s\" found when merging\n",
                      canonical);
        goto out;
    }

    retval = TRUE;

    g_hash_table_insert (loaded_menu_files, (char *) canonical, GUINT_TO_POINTER (TRUE));

    if (add_monitor)
        cmenu_tree_add_menu_file_monitor (tree,
                                          canonical,
                                          MENU_FILE_MONITOR_FILE);

    merge_resolved_children (tree, loaded_menu_files, where, to_merge);

    g_hash_table_remove (loaded_menu_files, canonical);

    menu_layout_node_unref (to_merge);

    out:
    if (freeme)
        g_free (freeme);

    return retval;
}

static gboolean
load_merge_file_with_config_dir (CMenuTree      *tree,
                                 GHashTable     *loaded_menu_files,
                                 const char     *menu_file,
                                 const char     *config_dir,
                                 MenuLayoutNode *where)
{
    char     *merge_file;
    gboolean  loaded;

    loaded = FALSE;

    merge_file = g_build_filename (config_dir, "menus", menu_file, NULL);

    if (load_merge_file (tree, loaded_menu_files, merge_file, FALSE, TRUE, where))
        loaded = TRUE;

    g_free (merge_file);

    return loaded;
}

static gboolean
compare_basedir_to_config_dir (const char *canonical_basedir,
                               const char *config_dir)
{
    char     *dirname;
    char     *canonical_menus_dir;
    gboolean  retval;

    menu_verbose ("Checking to see if basedir '%s' is in '%s'\n",
            canonical_basedir, config_dir);

    dirname = g_build_filename (config_dir, "menus", NULL);

    retval = FALSE;

    canonical_menus_dir = realpath (dirname, NULL);
    if (canonical_menus_dir != NULL &&
        strcmp (canonical_basedir, canonical_menus_dir) == 0)
    {
        retval = TRUE;
    }

    g_free (canonical_menus_dir);
    g_free (dirname);

    return retval;
}

static gboolean
load_parent_merge_file_from_basename (CMenuTree      *tree,
                                      GHashTable     *loaded_menu_files,
                                      MenuLayoutNode *layout,
                                      const char     *menu_file,
                                      const char     *canonical_basedir)
{
    gboolean            found_basedir;
    const char * const *system_config_dirs;
    int                 i;

    /* We're not interested in menu files that are in directories which are not a
    * parent of the base directory of this menu file */
    found_basedir = compare_basedir_to_config_dir (canonical_basedir,
                                                   g_get_user_config_dir ());

    system_config_dirs = g_get_system_config_dirs ();

    i = 0;
    while (system_config_dirs[i] != NULL)
    {
        if (!found_basedir)
        {
            found_basedir = compare_basedir_to_config_dir (canonical_basedir,
                                                           system_config_dirs[i]);
        }
        else
        {
            menu_verbose ("Looking for parent menu file '%s' in '%s'\n",
                          menu_file, system_config_dirs[i]);

            if (load_merge_file_with_config_dir (tree,
                                                 loaded_menu_files,
                                                 menu_file,
                                                 system_config_dirs[i],
                                                 layout))
            {
                break;
            }
        }

        ++i;
    }

    return system_config_dirs[i] != NULL;
}

static gboolean
load_parent_merge_file (CMenuTree      *tree,
                        GHashTable     *loaded_menu_files,
                        MenuLayoutNode *layout)
{
    MenuLayoutNode     *root;
    const char         *basedir;
    const char         *menu_name;
    char               *canonical_basedir;
    char               *menu_file;
    gboolean            found;

    root = menu_layout_node_get_root (layout);

    basedir   = menu_layout_node_root_get_basedir (root);
    menu_name = menu_layout_node_root_get_name (root);

    canonical_basedir = realpath (basedir, NULL);
    if (canonical_basedir == NULL)
    {
        menu_verbose ("Menu basedir '%s' no longer exists, not merging parent\n",
                      basedir);
        return FALSE;
    }

    found = FALSE;
    menu_file = g_strconcat (menu_name, ".menu", NULL);

    if (strcmp (menu_file, "applications.menu") == 0)
    {
        char *prefixed_basename;
        prefixed_basename = prefix_menu_name (menu_file);
        found = load_parent_merge_file_from_basename (tree, loaded_menu_files,
                                                      layout, prefixed_basename,
                                                      canonical_basedir);
        g_free (prefixed_basename);
    }

    if (!found)
    {
        found = load_parent_merge_file_from_basename (tree, loaded_menu_files,
                                                      layout, menu_file,
                                                      canonical_basedir);
    }

    g_free (menu_file);
    g_free (canonical_basedir);

    return found;
}

static void
load_merge_dir (CMenuTree      *tree,
                GHashTable     *loaded_menu_files,
                const char     *dirname,
                MenuLayoutNode *where)
{
    GDir       *dir;
    const char *menu_file;

    menu_verbose ("Loading merge dir \"%s\"\n", dirname);

    cmenu_tree_add_menu_file_monitor (tree,
                                      dirname,
                                      MENU_FILE_MONITOR_DIRECTORY);

    if ((dir = g_dir_open (dirname, 0, NULL)) == NULL)
        return;

    while ((menu_file = g_dir_read_name (dir)))
    {
        if (g_str_has_suffix (menu_file, ".menu"))
        {
            char *full_path;

            full_path = g_build_filename (dirname, menu_file, NULL);

            load_merge_file (tree, loaded_menu_files, full_path, TRUE, FALSE, where);

            g_free (full_path);
        }
    }

    g_dir_close (dir);
}

static void
load_merge_dir_with_config_dir (CMenuTree      *tree,
                                GHashTable     *loaded_menu_files,
                                const char     *config_dir,
                                const char     *dirname,
                                MenuLayoutNode *where)
{
    char *path;

    path = g_build_filename (config_dir, "menus", dirname, NULL);

    load_merge_dir (tree, loaded_menu_files, path, where);

    g_free (path);
}

static void
resolve_merge_file (CMenuTree      *tree,
                    GHashTable     *loaded_menu_files,
                    MenuLayoutNode *layout)
{
    char *filename;

    if (menu_layout_node_merge_file_get_type (layout) == MENU_MERGE_FILE_TYPE_PARENT)
    {
        if (load_parent_merge_file (tree, loaded_menu_files, layout))
            return;
    }

    filename = menu_layout_node_get_content_as_path (layout);
    if (filename == NULL)
    {
        menu_verbose ("didn't get node content as a path, not merging file\n");
    }
    else
    {
        load_merge_file (tree, loaded_menu_files, filename, FALSE, TRUE, layout);

        g_free (filename);
    }

    /* remove the now-replaced node */
    menu_layout_node_unlink (layout);
}

static void
resolve_merge_dir (CMenuTree      *tree,
                   GHashTable     *loaded_menu_files,
                   MenuLayoutNode *layout)
{
    char *path;

    path = menu_layout_node_get_content_as_path (layout);
    if (path == NULL)
    {
        menu_verbose ("didn't get layout node content as a path, not merging dir\n");
    }
    else
    {
        load_merge_dir (tree, loaded_menu_files, path, layout);

        g_free (path);
    }

    /* remove the now-replaced node */
    menu_layout_node_unlink (layout);
}

static MenuLayoutNode *
add_app_dir (CMenuTree      *tree,
             MenuLayoutNode *before,
             const char     *data_dir)
{
    MenuLayoutNode *tmp;
    char           *dirname;

    tmp = menu_layout_node_new (MENU_LAYOUT_NODE_APP_DIR);
    dirname = g_build_filename (data_dir, "applications", NULL);
    menu_layout_node_set_content (tmp, dirname);
    menu_layout_node_insert_before (before, tmp);
    menu_layout_node_unref (before);

    menu_verbose ("Adding <AppDir>%s</AppDir> in <DefaultAppDirs/>\n",
                dirname);

    g_free (dirname);

    return tmp;
}

static void
resolve_default_app_dirs (CMenuTree      *tree,
                          MenuLayoutNode *layout)
{
    MenuLayoutNode     *before;
    const char * const *system_data_dirs;
    int                 i;

    system_data_dirs = g_get_system_data_dirs ();

    before = add_app_dir (tree,
                          menu_layout_node_ref (layout),
                          g_get_user_data_dir ());

    i = 0;
    while (system_data_dirs[i] != NULL)
    {
        before = add_app_dir (tree, before, system_data_dirs[i]);

        ++i;
    }

    menu_layout_node_unref (before);

    /* remove the now-replaced node */
    menu_layout_node_unlink (layout);
}

static MenuLayoutNode *
add_directory_dir (CMenuTree      *tree,
                   MenuLayoutNode *before,
                   const char     *data_dir)
{
    MenuLayoutNode *tmp;
    char           *dirname;

    tmp = menu_layout_node_new (MENU_LAYOUT_NODE_DIRECTORY_DIR);
    dirname = g_build_filename (data_dir, "desktop-directories", NULL);
    menu_layout_node_set_content (tmp, dirname);
    menu_layout_node_insert_before (before, tmp);
    menu_layout_node_unref (before);

    menu_verbose ("Adding <DirectoryDir>%s</DirectoryDir> in <DefaultDirectoryDirs/>\n",
                  dirname);

    g_free (dirname);

    return tmp;
}

static void
resolve_default_directory_dirs (CMenuTree      *tree,
                                MenuLayoutNode *layout)
{
    MenuLayoutNode     *before;
    const char * const *system_data_dirs;
    int                 i;

    system_data_dirs = g_get_system_data_dirs ();

    before = add_directory_dir (tree,
                                menu_layout_node_ref (layout),
                                g_get_user_data_dir ());

    i = 0;
    while (system_data_dirs[i] != NULL)
    {
        before = add_directory_dir (tree, before, system_data_dirs[i]);

        ++i;
    }

    menu_layout_node_unref (before);

    /* remove the now-replaced node */
    menu_layout_node_unlink (layout);
}

static void
resolve_default_merge_dirs (CMenuTree      *tree,
                            GHashTable     *loaded_menu_files,
                            MenuLayoutNode *layout)
{
    MenuLayoutNode     *root;
    const char         *menu_name;
    char               *merge_name;
    const char * const *system_config_dirs;
    int                 i;

    root = menu_layout_node_get_root (layout);
    menu_name = menu_layout_node_root_get_name (root);

    merge_name = g_strconcat (menu_name, "-merged", NULL);

    system_config_dirs = g_get_system_config_dirs ();

    /* Merge in reverse order */
    i = 0;
    while (system_config_dirs[i] != NULL) i++;
    while (i > 0)
    {
        i--;
        load_merge_dir_with_config_dir (tree,
                                        loaded_menu_files,
                                        system_config_dirs[i],
                                        merge_name,
                                        layout);
    }

    load_merge_dir_with_config_dir (tree,
                                    loaded_menu_files,
                                    g_get_user_config_dir (),
                                    merge_name,
                                    layout);

    g_free (merge_name);

    /* remove the now-replaced node */
    menu_layout_node_unlink (layout);
}

static void
cmenu_tree_resolve_files (CMenuTree      *tree,
                          GHashTable     *loaded_menu_files,
                          MenuLayoutNode *layout)
{
    MenuLayoutNode *child;

    menu_verbose ("Resolving files in: ");
    menu_debug_print_layout (layout, TRUE);

    switch (menu_layout_node_get_type (layout))
    {
        case MENU_LAYOUT_NODE_MERGE_FILE:
            resolve_merge_file (tree, loaded_menu_files, layout);
            break;

        case MENU_LAYOUT_NODE_MERGE_DIR:
            resolve_merge_dir (tree, loaded_menu_files, layout);
            break;

        case MENU_LAYOUT_NODE_DEFAULT_APP_DIRS:
            resolve_default_app_dirs (tree, layout);
            break;

        case MENU_LAYOUT_NODE_DEFAULT_DIRECTORY_DIRS:
            resolve_default_directory_dirs (tree, layout);
            break;

        case MENU_LAYOUT_NODE_DEFAULT_MERGE_DIRS:
            resolve_default_merge_dirs (tree, loaded_menu_files, layout);
            break;

        case MENU_LAYOUT_NODE_LEGACY_DIR:
            menu_verbose ("Ignoring obsolete legacy dir");
            break;

        case MENU_LAYOUT_NODE_KDE_LEGACY_DIRS:
            menu_verbose ("Ignoring obsolete KDE legacy dirs");
            break;

        case MENU_LAYOUT_NODE_PASSTHROUGH:
            /* Just get rid of these, we don't need the memory usage */
            menu_layout_node_unlink (layout);
            break;

        default:
            /* Recurse */
            child = menu_layout_node_get_children (layout);
            while (child != NULL)
            {
                MenuLayoutNode *next = menu_layout_node_get_next (child);

                cmenu_tree_resolve_files (tree, loaded_menu_files, child);

                child = next;
            }
            break;
    }
}

static void
move_children (MenuLayoutNode *from,
               MenuLayoutNode *to)
{
    MenuLayoutNode *from_child;
    MenuLayoutNode *insert_before;

    insert_before = menu_layout_node_get_children (to);
    from_child    = menu_layout_node_get_children (from);

    while (from_child != NULL)
    {
        MenuLayoutNode *next;

        next = menu_layout_node_get_next (from_child);

        menu_layout_node_steal (from_child);

        if (menu_layout_node_get_type (from_child) == MENU_LAYOUT_NODE_NAME)
        {
            ; /* just drop the Name in the old <Menu> */
        }
        else if (insert_before)
        {
            menu_layout_node_insert_before (insert_before, from_child);
            g_assert (menu_layout_node_get_next (from_child) == insert_before);
        }
        else
        {
            menu_layout_node_append_child (to, from_child);
        }

        menu_layout_node_unref (from_child);

        from_child = next;
    }
}

static int
null_safe_strcmp (const char *a,
                  const char *b)
{
    if (a == NULL && b == NULL)
        return 0;
    else if (a == NULL)
        return -1;
    else if (b == NULL)
        return 1;
    else
        return strcmp (a, b);
}

static int
node_compare_func (const void *a,
                   const void *b)
{
    MenuLayoutNode *node_a = (MenuLayoutNode*) a;
    MenuLayoutNode *node_b = (MenuLayoutNode*) b;
    MenuLayoutNodeType t_a = menu_layout_node_get_type (node_a);
    MenuLayoutNodeType t_b = menu_layout_node_get_type (node_b);

    if (t_a < t_b)
        return -1;
    else if (t_a > t_b)
        return 1;
    else
    {
        const char *c_a = menu_layout_node_get_content (node_a);
        const char *c_b = menu_layout_node_get_content (node_b);

        return null_safe_strcmp (c_a, c_b);
    }
}

static int
node_menu_compare_func (const void *a,
                        const void *b)
{
    MenuLayoutNode *node_a = (MenuLayoutNode*) a;
    MenuLayoutNode *node_b = (MenuLayoutNode*) b;
    MenuLayoutNode *parent_a = menu_layout_node_get_parent (node_a);
    MenuLayoutNode *parent_b = menu_layout_node_get_parent (node_b);

    if (parent_a < parent_b)
        return -1;
    else if (parent_a > parent_b)
        return 1;
    else
        return null_safe_strcmp (menu_layout_node_menu_get_name (node_a),
                                 menu_layout_node_menu_get_name (node_b));
}

static void
cmenu_tree_strip_duplicate_children (CMenuTree      *tree,
                                     MenuLayoutNode *layout)
{
    MenuLayoutNode *child;
    GSList         *simple_nodes;
    GSList         *menu_layout_nodes;
    GSList         *prev;
    GSList         *tmp;

    /* to strip dups, we find all the child nodes where
    * we want to kill dups, sort them,
    * then nuke the adjacent nodes that are equal
    */

    simple_nodes = NULL;
    menu_layout_nodes = NULL;

    child = menu_layout_node_get_children (layout);
    while (child != NULL)
    {
        switch (menu_layout_node_get_type (child))
        {
            /* These are dups if their content is the same */
            case MENU_LAYOUT_NODE_APP_DIR:
            case MENU_LAYOUT_NODE_DIRECTORY_DIR:
            case MENU_LAYOUT_NODE_DIRECTORY:
                simple_nodes = g_slist_prepend (simple_nodes, child);
                break;

              /* These have to be merged in a more complicated way,
               * and then recursed
               */
            case MENU_LAYOUT_NODE_MENU:
                menu_layout_nodes = g_slist_prepend (menu_layout_nodes, child);
                break;

            default:
                break;
        }

        child = menu_layout_node_get_next (child);
    }

    /* Note that the lists are all backward. So we want to keep
    * the items that are earlier in the list, because they were
    * later in the file
    */

    /* stable sort the simple nodes */
    simple_nodes = g_slist_sort (simple_nodes,
                               node_compare_func);

    prev = NULL;
    tmp = simple_nodes;
    while (tmp != NULL)
    {
        GSList *next = tmp->next;

        if (prev)
        {
            MenuLayoutNode *p = prev->data;
            MenuLayoutNode *n = tmp->data;

            if (node_compare_func (p, n) == 0)
            {
                /* nuke it! */
                menu_layout_node_unlink (n);
                simple_nodes = g_slist_delete_link (simple_nodes, tmp);
                tmp = prev;
            }
        }

        prev = tmp;
        tmp = next;
    }

    g_slist_free (simple_nodes);
    simple_nodes = NULL;

    /* stable sort the menu nodes (the sort includes the
    * parents of the nodes in the comparison). Remember
    * the list is backward.
    */
    menu_layout_nodes = g_slist_sort (menu_layout_nodes,
                                      node_menu_compare_func);

    prev = NULL;
    tmp = menu_layout_nodes;
    while (tmp != NULL)
    {
        GSList *next = tmp->next;

        if (prev)
        {
            MenuLayoutNode *p = prev->data;
            MenuLayoutNode *n = tmp->data;

            if (node_menu_compare_func (p, n) == 0)
            {
                /* Move children of first menu to the start of second
                 * menu and nuke the first menu
                 */
                move_children (n, p);
                menu_layout_node_unlink (n);
                menu_layout_nodes = g_slist_delete_link (menu_layout_nodes, tmp);
                tmp = prev;
            }
        }

        prev = tmp;
        tmp = next;
    }

    g_slist_free (menu_layout_nodes);
    menu_layout_nodes = NULL;

    /* Recursively clean up all children */
    child = menu_layout_node_get_children (layout);
    while (child != NULL)
    {
        if (menu_layout_node_get_type (child) == MENU_LAYOUT_NODE_MENU)
            cmenu_tree_strip_duplicate_children (tree, child);

        child = menu_layout_node_get_next (child);
    }
}

static MenuLayoutNode *
find_submenu (MenuLayoutNode *layout,
              const char     *path,
              gboolean        create_if_not_found)
{
    MenuLayoutNode *child;
    const char     *slash;
    const char     *next_path;
    char           *name;

    menu_verbose (" (splitting \"%s\")\n", path);

    if (path[0] == '\0' || path[0] == G_DIR_SEPARATOR)
        return NULL;

    slash = strchr (path, G_DIR_SEPARATOR);
    if (slash != NULL)
    {
        name = g_strndup (path, slash - path);
        next_path = slash + 1;
        if (*next_path == '\0')
            next_path = NULL;
    }
    else
    {
        name = g_strdup (path);
        next_path = NULL;
    }

    child = menu_layout_node_get_children (layout);
    while (child != NULL)
    {
        switch (menu_layout_node_get_type (child))
        {
            case MENU_LAYOUT_NODE_MENU:
                if (strcmp (name, menu_layout_node_menu_get_name (child)) == 0)
                {
                    menu_verbose ("MenuNode %p found for path component \"%s\"\n",
                                  child, name);

                    g_free (name);

                    if (!next_path)
                    {
                        menu_verbose (" Found menu node %p parent is %p\n",
                                      child, layout);
                        return child;
                    }

                    return find_submenu (child, next_path, create_if_not_found);
                }
            break;

        default:
            break;
        }

      child = menu_layout_node_get_next (child);
    }

    if (create_if_not_found)
    {
        MenuLayoutNode *name_node;

        child = menu_layout_node_new (MENU_LAYOUT_NODE_MENU);
        menu_layout_node_append_child (layout, child);

        name_node = menu_layout_node_new (MENU_LAYOUT_NODE_NAME);
        menu_layout_node_set_content (name_node, name);
        menu_layout_node_append_child (child, name_node);
        menu_layout_node_unref (name_node);

        menu_verbose (" Created menu node %p parent is %p\n",
                      child, layout);

        menu_layout_node_unref (child);
        g_free (name);

        if (!next_path)
            return child;

        return find_submenu (child, next_path, create_if_not_found);
    }
    else
    {
        g_free (name);
        return NULL;
    }
}

/* To call this you first have to strip duplicate children once,
 * otherwise when you move a menu Foo to Bar then you may only
 * move one of Foo, not all the merged Foo.
 */
static void
cmenu_tree_execute_moves (CMenuTree      *tree,
                          MenuLayoutNode *layout,
                          gboolean       *need_remove_dups_p)
{
    MenuLayoutNode *child;
    gboolean        need_remove_dups;
    GSList         *move_nodes;
    GSList         *tmp;

    need_remove_dups = FALSE;

    move_nodes = NULL;

    child = menu_layout_node_get_children (layout);
    while (child != NULL)
    {
        switch (menu_layout_node_get_type (child))
        {
            case MENU_LAYOUT_NODE_MENU:
                /* Recurse - we recurse first and process the current node
                 * second, as the spec dictates.
                 */
                cmenu_tree_execute_moves (tree, child, &need_remove_dups);
                break;

            case MENU_LAYOUT_NODE_MOVE:
                move_nodes = g_slist_prepend (move_nodes, child);
                break;

            default:
                break;
        }

        child = menu_layout_node_get_next (child);
    }

    /* We need to execute the move operations in the order that they appear */
    move_nodes = g_slist_reverse (move_nodes);

    tmp = move_nodes;
    while (tmp != NULL)
    {
        MenuLayoutNode *move_node = tmp->data;
        MenuLayoutNode *old_node;
        GSList         *next = tmp->next;
        const char     *old;
        const char     *new;

        old = menu_layout_node_move_get_old (move_node);
        new = menu_layout_node_move_get_new (move_node);
        g_assert (old != NULL && new != NULL);

        menu_verbose ("executing <Move> old = \"%s\" new = \"%s\"\n",
                      old, new);

        old_node = find_submenu (layout, old, FALSE);
        if (old_node != NULL)
        {
            MenuLayoutNode *new_node;

            /* here we can create duplicates anywhere below the
             * node
             */
            need_remove_dups = TRUE;

            /* look up new node creating it and its parents if
             * required
             */
            new_node = find_submenu (layout, new, TRUE);
            g_assert (new_node != NULL);

            move_children (old_node, new_node);

            menu_layout_node_unlink (old_node);
        }

        menu_layout_node_unlink (move_node);

        tmp = next;
    }

    g_slist_free (move_nodes);

    /* This oddness is to ensure we only remove dups once,
     * at the root, instead of recursing the tree over
     * and over.
     */
    if (need_remove_dups_p)
        *need_remove_dups_p = need_remove_dups;
    else if (need_remove_dups)
        cmenu_tree_strip_duplicate_children (tree, layout);
}

static gboolean
cmenu_tree_load_layout (CMenuTree  *tree,
                        GError    **error)
{
    GHashTable *loaded_menu_files;

    if (tree->layout)
        return TRUE;

    if (!cmenu_tree_canonicalize_path (tree, error))
        return FALSE;

    menu_verbose ("Loading menu layout from \"%s\"\n",
                  tree->canonical_path);

    tree->layout = menu_layout_load (tree->canonical_path,
                                     tree->non_prefixed_basename,
                                     error);
    if (!tree->layout)
        return FALSE;

    loaded_menu_files = g_hash_table_new (g_str_hash, g_str_equal);
    g_hash_table_insert (loaded_menu_files, tree->canonical_path, GUINT_TO_POINTER (TRUE));
    cmenu_tree_resolve_files (tree, loaded_menu_files, tree->layout);
    g_hash_table_destroy (loaded_menu_files);

    cmenu_tree_strip_duplicate_children (tree, tree->layout);
    cmenu_tree_execute_moves (tree, tree->layout, NULL);

    return TRUE;
}

static void
cmenu_tree_force_reload (CMenuTree *tree)
{
    cmenu_tree_force_rebuild (tree);

    if (tree->layout)
        menu_layout_node_unref (tree->layout);
    tree->layout = NULL;
}

typedef struct
{
    DesktopEntrySet *set;
    const char      *category;
} GetByCategoryForeachData;

static void
get_by_category_foreach (const char               *file_id,
                         DesktopEntry             *entry,
                         GetByCategoryForeachData *data)
{
    if (desktop_entry_has_category (entry, data->category))
        desktop_entry_set_add_entry (data->set, entry, file_id);
}

static void
get_by_category (DesktopEntrySet *entry_pool,
                 DesktopEntrySet *set,
                 const char      *category)
{
    GetByCategoryForeachData data;

    data.set      = set;
    data.category = category;

    desktop_entry_set_foreach (entry_pool,
                               (DesktopEntrySetForeachFunc) get_by_category_foreach,
                               &data);
}

static DesktopEntrySet *
process_include_rules (MenuLayoutNode  *layout,
                       DesktopEntrySet *entry_pool)
{
    DesktopEntrySet *set = NULL;

    switch (menu_layout_node_get_type (layout))
    {
        case MENU_LAYOUT_NODE_AND:
            {
                MenuLayoutNode *child;

                menu_verbose ("Processing <And>\n");

                child = menu_layout_node_get_children (layout);
                while (child != NULL)
                {
                    DesktopEntrySet *child_set;

                    child_set = process_include_rules (child, entry_pool);

                    if (set == NULL)
                    {
                        set = child_set;
                    }
                    else
                    {
                        desktop_entry_set_intersection (set, child_set);
                        desktop_entry_set_unref (child_set);
                    }

                    /* as soon as we get empty results, we can bail,
                     * because it's an AND
                     */
                    if (desktop_entry_set_get_count (set) == 0)
                        break;

                    child = menu_layout_node_get_next (child);
                }
                menu_verbose ("Processed <And>\n");
            }
            break;

    case MENU_LAYOUT_NODE_OR:
        {
            MenuLayoutNode *child;

            menu_verbose ("Processing <Or>\n");

            child = menu_layout_node_get_children (layout);
            while (child != NULL)
            {
                DesktopEntrySet *child_set;

                child_set = process_include_rules (child, entry_pool);

                if (set == NULL)
                {
                    set = child_set;
                }
                else
                {
                    desktop_entry_set_union (set, child_set);
                    desktop_entry_set_unref (child_set);
                }

                child = menu_layout_node_get_next (child);
            }
            menu_verbose ("Processed <Or>\n");
        }
        break;

    case MENU_LAYOUT_NODE_NOT:
        {

            /* First get the OR of all the rules */
            MenuLayoutNode *child;

            menu_verbose ("Processing <Not>\n");

            child = menu_layout_node_get_children (layout);
            while (child != NULL)
            {
                DesktopEntrySet *child_set;

                child_set = process_include_rules (child, entry_pool);

                if (set == NULL)
                {
                    set = child_set;
                }
                else
                {
                    desktop_entry_set_union (set, child_set);
                    desktop_entry_set_unref (child_set);
                }

                child = menu_layout_node_get_next (child);
            }

            if (set != NULL)
            {
                DesktopEntrySet *inverted;

                /* Now invert the result */
                inverted = desktop_entry_set_new ();
                desktop_entry_set_union (inverted, entry_pool);
                desktop_entry_set_subtract (inverted, set);
                desktop_entry_set_unref (set);
                set = inverted;
            }
            menu_verbose ("Processed <Not>\n");
        }
        break;

    case MENU_LAYOUT_NODE_ALL:
        menu_verbose ("Processing <All>\n");
        set = desktop_entry_set_new ();
        desktop_entry_set_union (set, entry_pool);
        menu_verbose ("Processed <All>\n");
        break;

    case MENU_LAYOUT_NODE_FILENAME:
        {
            DesktopEntry *entry;

            menu_verbose ("Processing <Filename>%s</Filename>\n",
                          menu_layout_node_get_content (layout));

            entry = desktop_entry_set_lookup (entry_pool,
                                              menu_layout_node_get_content (layout));
            if (entry != NULL)
            {
                set = desktop_entry_set_new ();
                desktop_entry_set_add_entry (set,
                                             entry,
                                             menu_layout_node_get_content (layout));
            }
            menu_verbose ("Processed <Filename>%s</Filename>\n",
                          menu_layout_node_get_content (layout));
        }
        break;

    case MENU_LAYOUT_NODE_CATEGORY:
        menu_verbose ("Processing <Category>%s</Category>\n",
                      menu_layout_node_get_content (layout));
        set = desktop_entry_set_new ();
        get_by_category (entry_pool, set, menu_layout_node_get_content (layout));
        menu_verbose ("Processed <Category>%s</Category>\n",
                      menu_layout_node_get_content (layout));
        break;

    default:
        break;
    }

    if (set == NULL)
        set = desktop_entry_set_new (); /* create an empty set */

    menu_verbose ("Matched %d entries\n", desktop_entry_set_get_count (set));

    return set;
}

static void
entries_listify_foreach (const char         *desktop_file_id,
                         DesktopEntry       *desktop_entry,
                         CMenuTreeDirectory *directory)
{
    directory->entries =
        g_slist_prepend (directory->entries,
                         cmenu_tree_entry_new (directory,
                                               desktop_entry,
                                               desktop_file_id,
                                               FALSE,
                                               FALSE));
}

static void
excluded_entries_listify_foreach (const char         *desktop_file_id,
                                  DesktopEntry       *desktop_entry,
                                  CMenuTreeDirectory *directory)
{
    directory->entries =
        g_slist_prepend (directory->entries,
                         cmenu_tree_entry_new (directory,
                                               desktop_entry,
                                               desktop_file_id,
                                               TRUE,
                                               FALSE));
}

static void
unallocated_entries_listify_foreach (const char         *desktop_file_id,
                                     DesktopEntry       *desktop_entry,
                                     CMenuTreeDirectory *directory)
{
    directory->entries =
        g_slist_prepend (directory->entries,
                         cmenu_tree_entry_new (directory,
                                               desktop_entry,
                                               desktop_file_id,
                                               FALSE,
                                               TRUE));
}

static CMenuTreeDirectory *
process_layout (CMenuTree          *tree,
                CMenuTreeDirectory *parent,
                MenuLayoutNode     *layout,
                DesktopEntrySet    *allocated)
{
    MenuLayoutNode     *layout_iter;
    CMenuTreeDirectory *directory;
    DesktopEntrySet    *entry_pool;
    DesktopEntrySet    *entries;
    DesktopEntrySet    *allocated_set;
    DesktopEntrySet    *excluded_set;
    DesktopEntry       *directory_entry;
    gboolean            deleted;
    gboolean            only_unallocated;
    GSList             *tmp;

    g_assert (menu_layout_node_get_type (layout) == MENU_LAYOUT_NODE_MENU);
    g_assert (menu_layout_node_menu_get_name (layout) != NULL);

    directory = cmenu_tree_directory_new (parent, menu_layout_node_menu_get_name (layout));

    menu_verbose ("=== Menu name = %s ===\n", cmenu_tree_directory_get_name (directory));


    deleted = FALSE;
    only_unallocated = FALSE;

    entries = desktop_entry_set_new ();
    allocated_set = desktop_entry_set_new ();

    if (tree->flags & CMENU_TREE_FLAGS_INCLUDE_EXCLUDED)
        excluded_set = desktop_entry_set_new ();
    else
        excluded_set = NULL;

    entry_pool = _entry_directory_list_get_all_desktops (menu_layout_node_menu_get_app_dirs (layout));

    layout_iter = menu_layout_node_get_children (layout);
    while (layout_iter != NULL)
    {
        switch (menu_layout_node_get_type (layout_iter))
        {
            case MENU_LAYOUT_NODE_MENU:
                {
                    /* recurse */
                    CMenuTreeDirectory *child_dir;

                    menu_verbose ("Processing <Menu>\n");

                    child_dir = process_layout (tree,
                                                directory,
                                                layout_iter,
                                                allocated);
                    if (child_dir)
                        directory->subdirs = g_slist_prepend (directory->subdirs,
                                                              child_dir);

                    menu_verbose ("Processed <Menu>\n");
                }
                break;

            case MENU_LAYOUT_NODE_INCLUDE:
                {
                    /* The match rule children of the <Include> are
                     * independent (logical OR) so we can process each one by
                     * itself
                     */
                    MenuLayoutNode *rule;

                    menu_verbose ("Processing <Include> (%d entries)\n",
                           desktop_entry_set_get_count (entries));

                    rule = menu_layout_node_get_children (layout_iter);
                    while (rule != NULL)
                    {
                        DesktopEntrySet *rule_set;

                        rule_set = process_include_rules (rule, entry_pool);
                        if (rule_set != NULL)
                        {
                            desktop_entry_set_union (entries, rule_set);
                            desktop_entry_set_union (allocated_set, rule_set);
                            if (excluded_set != NULL)
                                desktop_entry_set_subtract (excluded_set, rule_set);
                            desktop_entry_set_unref (rule_set);
                        }

                        rule = menu_layout_node_get_next (rule);
                    }

                    menu_verbose ("Processed <Include> (%d entries)\n",
                                  desktop_entry_set_get_count (entries));
                }
                break;

            case MENU_LAYOUT_NODE_EXCLUDE:
                {
                    /* The match rule children of the <Exclude> are
                     * independent (logical OR) so we can process each one by
                     * itself
                     */
                    MenuLayoutNode *rule;

                    menu_verbose ("Processing <Exclude> (%d entries)\n",
                                  desktop_entry_set_get_count (entries));

                    rule = menu_layout_node_get_children (layout_iter);
                    while (rule != NULL)
                    {
                        DesktopEntrySet *rule_set;

                        rule_set = process_include_rules (rule, entry_pool);
                        if (rule_set != NULL)
                        {
                            if (excluded_set != NULL)
                                desktop_entry_set_union (excluded_set, rule_set);
                            desktop_entry_set_subtract (entries, rule_set);
                            desktop_entry_set_unref (rule_set);
                        }

                        rule = menu_layout_node_get_next (rule);
                    }

                    menu_verbose ("Processed <Exclude> (%d entries)\n",
                                  desktop_entry_set_get_count (entries));
                }
                break;

            case MENU_LAYOUT_NODE_DIRECTORY:
                {

                    menu_verbose ("Processing <Directory>%s</Directory>\n",
                                  menu_layout_node_get_content (layout_iter));

                    /*
                     * The last <Directory> to exist wins, so we always try overwriting
                     */
                    directory_entry = entry_directory_list_get_directory (menu_layout_node_menu_get_directory_dirs (layout),
                                                                menu_layout_node_get_content (layout_iter));

                    if (directory_entry != NULL)
                    {
                        if (!desktop_entry_get_hidden (directory_entry))
                        {
                            cmenu_tree_directory_set_directory_entry (directory, directory_entry); /* pass ref ownership */
                        }
                        else
                        {
                            desktop_entry_unref (directory_entry);
                        }
                    }

                    menu_verbose ("Processed <Directory> new directory entry = %p (%s)\n",
                                  directory_entry, directory_entry ? desktop_entry_get_path (directory_entry) : "null");
                }
                break;

            case MENU_LAYOUT_NODE_DELETED:
                menu_verbose ("Processed <Deleted/>\n");
                deleted = TRUE;
                break;

            case MENU_LAYOUT_NODE_NOT_DELETED:
                menu_verbose ("Processed <NotDeleted/>\n");
                deleted = FALSE;
                break;

            case MENU_LAYOUT_NODE_ONLY_UNALLOCATED:
                menu_verbose ("Processed <OnlyUnallocated/>\n");
                only_unallocated = TRUE;
                break;

            case MENU_LAYOUT_NODE_NOT_ONLY_UNALLOCATED:
                menu_verbose ("Processed <NotOnlyUnallocated/>\n");
                only_unallocated = FALSE;
                break;

            case MENU_LAYOUT_NODE_DEFAULT_LAYOUT:
            case MENU_LAYOUT_NODE_LAYOUT:
                break;

            default:
                break;
        }

        layout_iter = menu_layout_node_get_next (layout_iter);
    }

    desktop_entry_set_unref (entry_pool);

    cmenu_tree_directory_set_only_unallocated (directory, only_unallocated);

    if (!only_unallocated)
        desktop_entry_set_union (allocated, allocated_set);

    desktop_entry_set_unref (allocated_set);

    directory_entry = cmenu_tree_directory_get_directory_entry (directory);
    if (directory_entry)
    {
        if (desktop_entry_get_no_display (directory_entry))
        {
            cmenu_tree_directory_set_is_nodisplay (directory, TRUE);

            if (!(tree->flags & CMENU_TREE_FLAGS_INCLUDE_NODISPLAY))
            {
                menu_verbose ("Not showing menu %s because NoDisplay=true\n",
                              desktop_entry_get_name (directory_entry));
                deleted = TRUE;
            }
        }

        if (!desktop_entry_get_show_in (directory_entry))
        {
            menu_verbose ("Not showing menu %s because OnlyShowIn!=$DESKTOP or NotShowIn=$DESKTOP (with $DESKTOP=${XDG_CURRENT_DESKTOP:-GNOME})\n",
                          desktop_entry_get_name (directory_entry));
            deleted = TRUE;
        }
    }

    if (deleted)
    {
        if (excluded_set != NULL)
            desktop_entry_set_unref (excluded_set);
        desktop_entry_set_unref (entries);
        cmenu_tree_item_unref (directory);
        return NULL;
    }

    desktop_entry_set_foreach (entries,
                               (DesktopEntrySetForeachFunc) entries_listify_foreach,
                               directory);
    desktop_entry_set_unref (entries);

    if (excluded_set != NULL)
    {
        desktop_entry_set_foreach (excluded_set,
                                   (DesktopEntrySetForeachFunc) excluded_entries_listify_foreach,
                                   directory);
        desktop_entry_set_unref (excluded_set);
    }

    tmp = directory->entries;
    while (tmp != NULL)
    {
        CMenuTreeEntry *entry = tmp->data;
        GSList         *next  = tmp->next;
        gboolean        delete = FALSE;

        /* If adding a new condition to delete here, it has to be added to
         * get_still_unallocated_foreach() too */

        if (desktop_entry_get_hidden (cmenu_tree_entry_get_desktop_entry (entry)))
        {
            menu_verbose ("Deleting %s because Hidden=true\n",
                          desktop_entry_get_name (cmenu_tree_entry_get_desktop_entry (entry)));
            delete = TRUE;
        }

        if (!(tree->flags & CMENU_TREE_FLAGS_INCLUDE_NODISPLAY) &&
            desktop_entry_get_no_display (cmenu_tree_entry_get_desktop_entry (entry)))
        {
            menu_verbose ("Deleting %s because NoDisplay=true\n",
                          desktop_entry_get_name (cmenu_tree_entry_get_desktop_entry (entry)));
            delete = TRUE;
        }

        if (!desktop_entry_get_show_in (cmenu_tree_entry_get_desktop_entry (entry)))
        {
            menu_verbose ("Deleting %s because OnlyShowIn!=$DESKTOP or NotShowIn=$DESKTOP (with $DESKTOP=${XDG_CURRENT_DESKTOP:-GNOME})\n",
                          desktop_entry_get_name (cmenu_tree_entry_get_desktop_entry (entry)));
            delete = TRUE;
        }

        /* No need to filter out based on TryExec since GDesktopAppInfo cannot
         * deal with .desktop files with a failed TryExec. */

        if (delete)
        {
            directory->entries = g_slist_delete_link (directory->entries, tmp);
            cmenu_tree_item_unref_and_unset_parent (entry);
        }

        tmp = next;
    }

    g_assert (cmenu_tree_directory_get_name (directory) != NULL);

    return directory;
}

static void
process_only_unallocated (CMenuTree          *tree,
                          CMenuTreeDirectory *directory,
                          DesktopEntrySet    *allocated,
                          DesktopEntrySet    *unallocated_used)
{
    GSList *tmp;

    /* For any directory marked only_unallocated, we have to remove any
     * entries that were in fact allocated.
     */

    if (cmenu_tree_directory_get_only_unallocated (directory))
    {
        tmp = directory->entries;
        while (tmp != NULL)
        {
            CMenuTreeEntry *entry = tmp->data;
            GSList         *next  = tmp->next;

            if (desktop_entry_set_lookup (allocated, cmenu_tree_entry_get_desktop_file_id (entry)))
            {
                directory->entries = g_slist_delete_link (directory->entries, tmp);
                cmenu_tree_item_unref_and_unset_parent (entry);
            }
            else
            {
                desktop_entry_set_add_entry (unallocated_used,
                                             cmenu_tree_entry_get_desktop_entry (entry),
                                             cmenu_tree_entry_get_desktop_file_id (entry));
            }

            tmp = next;
        }
    }

    tmp = directory->subdirs;
    while (tmp != NULL)
    {
        CMenuTreeDirectory *subdir = tmp->data;

        process_only_unallocated (tree, subdir, allocated, unallocated_used);

        tmp = tmp->next;
   }
}

typedef struct
{
    CMenuTree *tree;
    DesktopEntrySet *allocated;
    DesktopEntrySet *unallocated_used;
    DesktopEntrySet *still_unallocated;
} GetStillUnallocatedForeachData;

static void
get_still_unallocated_foreach (const char                     *file_id,
                               DesktopEntry                   *entry,
                               GetStillUnallocatedForeachData *data)
{
    if (desktop_entry_set_lookup (data->allocated, file_id))
        return;

    if (desktop_entry_set_lookup (data->unallocated_used, file_id))
        return;

    /* Same rules than at the end of process_layout() */
    if (desktop_entry_get_hidden (entry))
        return;

    if (!(data->tree->flags & CMENU_TREE_FLAGS_INCLUDE_NODISPLAY) &&
        desktop_entry_get_no_display (entry))
        return;

    if (!desktop_entry_get_show_in (entry))
        return;

    desktop_entry_set_add_entry (data->still_unallocated, entry, file_id);
}

static void
handle_entries_changed (MenuLayoutNode *layout,
                        CMenuTree       *tree)
{
    if (tree->layout == layout)
    {
        cmenu_tree_force_rebuild (tree);
        cmenu_tree_invoke_monitors (tree);
    }
}

static void
update_entry_index (CMenuTree           *tree,
                    CMenuTreeDirectory  *dir)
{
    CMenuTreeIter *iter = cmenu_tree_directory_iter (dir);
    CMenuTreeItemType next_type;

    while ((next_type = cmenu_tree_iter_next (iter)) != CMENU_TREE_ITEM_INVALID)
    {
        gpointer item = NULL;

        switch (next_type)
        {
            case CMENU_TREE_ITEM_ENTRY:
                {
                    const char *id;

                    item = cmenu_tree_iter_get_entry (iter);
                    id = cmenu_tree_entry_get_desktop_file_id (item);
                    if (id != NULL)
                        g_hash_table_insert (tree->entries_by_id, (char*)id, item);
                }
                break;
            case CMENU_TREE_ITEM_DIRECTORY:
                item = cmenu_tree_iter_get_directory (iter);
                update_entry_index (tree, (CMenuTreeDirectory*)item);
                break;
            default:
                break;
        }
        if (item != NULL)
            cmenu_tree_item_unref (item);
    }

    cmenu_tree_iter_unref (iter);
}

static gboolean
cmenu_tree_build_from_layout (CMenuTree  *tree,
                              GError    **error)
{
    DesktopEntrySet *allocated;
    GSList *tmp;

    if (tree->root)
        return TRUE;

    if (!cmenu_tree_load_layout (tree, error))
        return FALSE;

    menu_verbose ("Building menu tree from layout\n");

    allocated = desktop_entry_set_new ();

    /* create the menu structure */
    tree->root = process_layout (tree,
                                 NULL,
                                 find_menu_child (tree->layout),
                                 allocated);
    if (tree->root)
    {
        DesktopEntrySet *unallocated_used;

        unallocated_used = desktop_entry_set_new ();

        process_only_unallocated (tree, tree->root, allocated, unallocated_used);
        if (tree->flags & CMENU_TREE_FLAGS_INCLUDE_UNALLOCATED)
        {
            DesktopEntrySet *entry_pool;
            DesktopEntrySet *still_unallocated;
            GetStillUnallocatedForeachData data;

            entry_pool = _entry_directory_list_get_all_desktops (menu_layout_node_menu_get_app_dirs (find_menu_child (tree->layout)));
            still_unallocated = desktop_entry_set_new ();

            data.tree = tree;
            data.allocated = allocated;
            data.unallocated_used = unallocated_used;
            data.still_unallocated = still_unallocated;

            desktop_entry_set_foreach (entry_pool,
                                       (DesktopEntrySetForeachFunc) get_still_unallocated_foreach,
                                       &data);

            desktop_entry_set_unref (entry_pool);

            desktop_entry_set_foreach (still_unallocated,
                                       (DesktopEntrySetForeachFunc) unallocated_entries_listify_foreach,
                                       tree->root);

            desktop_entry_set_unref (still_unallocated);
        }

        desktop_entry_set_unref (unallocated_used);

        // flatten and sort
        tmp = tree->root->subdirs;
        while (tmp != NULL)
        {
            CMenuTreeDirectory *directory = tmp->data;

            cmenu_tree_directory_flatten (directory);
            directory->contents = g_slist_sort (directory->entries, (GCompareFunc) cmenu_tree_entry_compare_by_name);

            directory->entries = NULL;

            tmp = tmp->next;
        }

        tree->root->contents = g_slist_sort (tree->root->subdirs, (GCompareFunc) cmenu_tree_directory_compare);
        tree->root->subdirs = NULL;

        update_entry_index (tree, tree->root);

        menu_layout_node_root_add_entries_monitor (tree->layout,
                                                   (MenuLayoutNodeEntriesChangedFunc) handle_entries_changed,
                                                   tree);
    }

    desktop_entry_set_unref (allocated);

    return TRUE;
}

static void
cmenu_tree_force_rebuild (CMenuTree *tree)
{
    if (tree->root)
    {
        g_hash_table_remove_all (tree->entries_by_id);
        cmenu_tree_item_unref (tree->root);
        tree->root = NULL;
        tree->loaded = FALSE;

        g_assert (tree->layout != NULL);

        menu_layout_node_root_remove_entries_monitor (tree->layout,
                                                      (MenuLayoutNodeEntriesChangedFunc) handle_entries_changed,
                                                      tree);
    }
}
