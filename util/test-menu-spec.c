/*
 * Copyright (C) 2004 Red Hat, Inc.
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

#include "gmenu-tree.h"

#include <string.h>

/* This is only a test program, so we don't need translations. Still keep the
 * infrastructure in place in case we suddenly decide we want to localize this
 * program. Don't forget to reenable the call to bindtextdomain() if going back
 * to real localization. */
#define _(x) x
#define N_(x) x

static char     *menu_file = NULL;
static gboolean  monitor = FALSE;
static gboolean  include_excluded = FALSE;
static gboolean  include_nodisplay = FALSE;
static gboolean  include_unallocated = FALSE;

static GOptionEntry options[] = {
  { "file",                'f', 0, G_OPTION_ARG_STRING, &menu_file,           N_("Menu file"),                      N_("MENU_FILE") },
  { "monitor",             'm', 0, G_OPTION_ARG_NONE,   &monitor,             N_("Monitor for menu changes"),       NULL },
  { "include-excluded",    'i', 0, G_OPTION_ARG_NONE,   &include_excluded,    N_("Include <Exclude>d entries"),     NULL },
  { "include-nodisplay",   'n', 0, G_OPTION_ARG_NONE,   &include_nodisplay,   N_("Include NoDisplay=true entries"), NULL },
  { "include-unallocated", 'u', 0, G_OPTION_ARG_NONE,   &include_unallocated, N_("Include unallocated entries"), NULL },
  { NULL }
};

static void
append_directory_path (GMenuTreeDirectory *directory,
		       GString            *path)
{
  GMenuTreeDirectory *parent;

  parent = gmenu_tree_directory_get_parent (directory);

  if (!parent)
    {
      g_string_append_c (path, '/');
      return;
    }

  append_directory_path (parent, path);

  g_string_append (path, gmenu_tree_directory_get_name (directory));
  g_string_append_c (path, '/');

  gmenu_tree_item_unref (parent);
}

static char *
make_path (GMenuTreeDirectory *directory)
{
  GString *path;

  g_return_val_if_fail (directory != NULL, NULL);

  path = g_string_new (NULL);

  append_directory_path (directory, path);

  return g_string_free (path, FALSE);
}

static void
print_entry (GMenuTreeEntry *entry,
	     const char     *path)
{
  char *utf8_path;
  char *utf8_file_id;

  utf8_path = g_filename_to_utf8 (gmenu_tree_entry_get_desktop_file_path (entry),
				  -1, NULL, NULL, NULL);

  utf8_file_id = g_filename_to_utf8 (gmenu_tree_entry_get_desktop_file_id (entry),
				     -1, NULL, NULL, NULL);

  g_print ("%s\t%s\t%s%s\n",
	   path,
	   utf8_file_id ? utf8_file_id : _("Invalid desktop file ID"),
	   utf8_path ? utf8_path : _("[Invalid Filename]"),
	   gmenu_tree_entry_get_is_excluded (entry) ? _(" <excluded>") : "");

  g_free (utf8_file_id);
  g_free (utf8_path);
}

static void
print_directory (GMenuTreeDirectory *directory)
{
  GMenuTreeIter *iter;
  const char *path;
  char       *freeme;

  freeme = make_path (directory);
  if (!strcmp (freeme, "/"))
    path = freeme;
  else
    path = freeme + 1;

  iter = gmenu_tree_directory_iter (directory);

  while (TRUE)
    {
      gpointer item;

      switch (gmenu_tree_iter_next (iter))
	{
	case GMENU_TREE_ITEM_INVALID:
	  goto done;

	case GMENU_TREE_ITEM_ENTRY:
	  item = gmenu_tree_iter_get_entry (iter);
	  print_entry ((GMenuTreeEntry*)item, path);
	  break;

	case GMENU_TREE_ITEM_DIRECTORY:
	  item = gmenu_tree_iter_get_directory (iter);
	  print_directory ((GMenuTreeDirectory*)item);
	  break;

	case GMENU_TREE_ITEM_HEADER:
	case GMENU_TREE_ITEM_SEPARATOR:
	  item = NULL;
	  break;

	case GMENU_TREE_ITEM_ALIAS:
	  {
	    item = gmenu_tree_iter_get_alias (iter);

	    if (gmenu_tree_alias_get_aliased_item_type (item) == GMENU_TREE_ITEM_ENTRY)
	      {
		GMenuTreeEntry *entry = gmenu_tree_alias_get_aliased_entry (item);
		print_entry (entry, path);
		gmenu_tree_item_unref (entry);
	      }
	  }
	  break;

	default:
	  g_assert_not_reached ();
	  break;
	}

      gmenu_tree_item_unref (item);
      continue;
    done:
      break;
    }

  gmenu_tree_iter_unref (iter);

  g_free (freeme);
}

static void
handle_tree_changed (GMenuTree *tree)
{
  GMenuTreeDirectory *root;
  GError *error = NULL;

  g_print (_("\n\n\n==== Menu changed, reloading ====\n\n\n"));

  if (!gmenu_tree_load_sync (tree, &error))
    {
      g_printerr ("Failed to load tree: %s\n", error->message);
      g_clear_error (&error);
      return;
    }

  root = gmenu_tree_get_root_directory (tree);
  if (root == NULL)
    {
      g_warning (_("Menu tree is empty"));
      return;
    }

  print_directory (root);
  gmenu_tree_item_unref (root);
}

int
main (int argc, char **argv)
{
  GOptionContext    *options_context;
  GMenuTree          *tree;
  GMenuTreeDirectory *root;
  GMenuTreeFlags      flags;
  GError             *error = NULL;

  g_type_init ();

#if 0
  /* See comment when defining _() at the top of this file. */
  bindtextdomain (GETTEXT_PACKAGE, GNOMELOCALEDIR);
  bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
  textdomain (GETTEXT_PACKAGE);
#endif

  options_context = g_option_context_new (_("- test GNOME's implementation of the Desktop Menu Specification"));
  g_option_context_add_main_entries (options_context, options, GETTEXT_PACKAGE);
  g_option_context_parse (options_context, &argc, &argv, NULL);
  g_option_context_free (options_context);

  flags = GMENU_TREE_FLAGS_NONE;
  if (include_excluded)
    flags |= GMENU_TREE_FLAGS_INCLUDE_EXCLUDED;
  if (include_nodisplay)
    flags |= GMENU_TREE_FLAGS_INCLUDE_NODISPLAY;
  if (include_unallocated)
    flags |= GMENU_TREE_FLAGS_INCLUDE_UNALLOCATED;

  tree = gmenu_tree_new (menu_file ? menu_file : "applications.menu", flags);
  g_assert (tree != NULL);

  if (!gmenu_tree_load_sync (tree, &error))
    {
      g_printerr ("Failed to load tree: %s\n", error->message);
      return 1;
    }

  g_print ("Loaded menu from %s\n", gmenu_tree_get_canonical_menu_path (tree));

  root = gmenu_tree_get_root_directory (tree);
  if (root != NULL)
    {
      print_directory (root);
      gmenu_tree_item_unref (root);
    }
  else
    {
      g_warning (_("Menu tree is empty"));
    }

  if (monitor)
    {
      GMainLoop *main_loop;

      g_signal_connect (tree, "changed", G_CALLBACK (handle_tree_changed), NULL);

      main_loop = g_main_loop_new (NULL, FALSE);
      g_main_loop_run (main_loop);
      g_main_loop_unref (main_loop);
    }

  g_object_unref (tree);

  return 0;
}
