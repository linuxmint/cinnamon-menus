/*
 * Copyright (C) 2002 - 2004 Red Hat, Inc.
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

#include "desktop-entries.h"
#include <gio/gdesktopappinfo.h>

#include <string.h>

#include "menu-util.h"

#define DESKTOP_ENTRY_GROUP     "Desktop Entry"

struct DesktopEntry
{
  guint       refcount;

  char       *path;
  const char *basename;

  guint       type      : 2;
  guint       reserved  : 30;
};

typedef struct
{
  DesktopEntry     base;

  GDesktopAppInfo *appinfo;
  GQuark          *categories;
  guint            showin : 1;
} DesktopEntryDesktop;

typedef struct
{
  DesktopEntry base;

  char     *name;
  char     *generic_name;
  char     *comment;
  GIcon    *icon;

  guint     nodisplay   : 1;
  guint     hidden      : 1;
  guint     showin      : 1;
} DesktopEntryDirectory;

struct DesktopEntrySet
{
  int         refcount;
  GHashTable *hash;
};

/*
 * Desktop entries
 */

/**
 * unix_basename_from_path:
 * @path: Path string
 *
 * Returns: A constant pointer into the basename of @path
 */
static const char *
unix_basename_from_path (const char *path)
{
  const char *basename = g_strrstr (path, "/");
  if (basename)
    return basename + 1;
  else
    return path;
}

static const char *
get_current_desktop (void)
{
  static char *current_desktop = NULL;

  /* Support XDG_CURRENT_DESKTOP environment variable; this can be used
   * to abuse gnome-menus in non-GNOME desktops. */
  if (!current_desktop)
    {
      const char *desktop;

      desktop = g_getenv ("XDG_CURRENT_DESKTOP");

      /* Note: if XDG_CURRENT_DESKTOP is set but empty, do as if it
       * was not set */
      if (!desktop || desktop[0] == '\0')
        current_desktop = g_strdup ("GNOME");
      else
        current_desktop = g_strdup (desktop);
    }

  /* Using "*" means skipping desktop-related checks */
  if (g_strcmp0 (current_desktop, "*") == 0)
    return NULL;

  return current_desktop;
}

static GIcon *
key_file_get_icon (GKeyFile *key_file)
{
  GIcon *icon = NULL;
  gchar *icon_name;

  icon_name = g_key_file_get_locale_string (key_file, DESKTOP_ENTRY_GROUP,
                                            "Icon", NULL, NULL);
  if (!icon_name)
    return NULL;

  if (g_path_is_absolute (icon_name)) {
    GFile *file;

    file = g_file_new_for_path (icon_name);
    icon = g_file_icon_new (file);
    g_object_unref (file);
  } else {
    char *p;

    /* Work around a common mistake in desktop files */
    if ((p = strrchr (icon_name, '.')) != NULL &&
        (strcmp (p, ".png") == 0 ||
         strcmp (p, ".xpm") == 0 ||
         strcmp (p, ".svg") == 0))
      *p = 0;

    icon = g_themed_icon_new (icon_name);
  }

  g_free (icon_name);

  return icon;
}

static gboolean
key_file_get_show_in (GKeyFile *key_file)
{
  const gchar *current_desktop;
  gchar **strv;
  gboolean show_in = TRUE;
  int i;
  gchar *exec;

  current_desktop = get_current_desktop ();
  if (!current_desktop)
    return TRUE;

  exec = g_key_file_get_string (key_file,
                                DESKTOP_ENTRY_GROUP,
                                "Exec",
                                NULL);

  if (exec) {
      if (g_str_has_prefix (exec, "gnome-control-center")) {
        g_free (exec);
        return FALSE;
      }
      g_free (exec);
  }

  strv = g_key_file_get_string_list (key_file,
                                     DESKTOP_ENTRY_GROUP,
                                     "OnlyShowIn",
                                     NULL,
                                     NULL);

  if (strv)
    {
      show_in = FALSE;
      for (i = 0; strv[i]; i++)
        {
          if (!strcmp (strv[i], "GNOME") || !strcmp (strv[i], "X-Cinnamon"))
            {
              show_in = TRUE;
              break;
            }
        }
    }
  else
    {
      strv = g_key_file_get_string_list (key_file,
                                         DESKTOP_ENTRY_GROUP,
                                         "NotShowIn",
                                         NULL,
                                         NULL);
      if (strv)
        {
          show_in = TRUE;
          for (i = 0; strv[i]; i++)
            {
              if (!strcmp (strv[i], current_desktop))
                {
                  show_in = FALSE;
                }
            }
        }
    }
  g_strfreev (strv);

  return show_in;
}

static gboolean
desktop_entry_load_directory (DesktopEntry  *entry,
                              GKeyFile      *key_file,
                              GError       **error)
{
  DesktopEntryDirectory *entry_directory = (DesktopEntryDirectory*)entry;
  char *type_str;

  type_str = g_key_file_get_string (key_file, DESKTOP_ENTRY_GROUP, "Type", error);
  if (!type_str)
    return FALSE;

  if (strcmp (type_str, "Directory") != 0)
    {
      g_set_error (error,
                   G_KEY_FILE_ERROR,
                   G_KEY_FILE_ERROR_INVALID_VALUE,
                   "\"%s\" does not contain the correct \"Type\" value\n", entry->path);
      g_free (type_str);
      return FALSE;
    }

  g_free (type_str);

  entry_directory->name = g_key_file_get_locale_string (key_file, DESKTOP_ENTRY_GROUP, "Name", NULL, error);
  if (entry_directory->name == NULL)
    return FALSE;

  entry_directory->generic_name = g_key_file_get_locale_string (key_file, DESKTOP_ENTRY_GROUP, "GenericName", NULL, NULL);
  entry_directory->comment      = g_key_file_get_locale_string (key_file, DESKTOP_ENTRY_GROUP, "Comment", NULL, NULL);
  entry_directory->icon         = key_file_get_icon (key_file);
  entry_directory->nodisplay    = g_key_file_get_boolean (key_file,
                                                          DESKTOP_ENTRY_GROUP,
                                                          "NoDisplay",
                                                          NULL);
  entry_directory->hidden       = g_key_file_get_boolean (key_file,
                                                          DESKTOP_ENTRY_GROUP,
                                                          "Hidden",
                                                          NULL);
  entry_directory->showin       = key_file_get_show_in (key_file);

  return TRUE;
}

static DesktopEntryResultCode
desktop_entry_load (DesktopEntry *entry)
{
  if (strstr (entry->path, "/menu-xdg/"))
    return DESKTOP_ENTRY_LOAD_FAIL_OTHER;
  if (entry->type == DESKTOP_ENTRY_DESKTOP)
    {
      GKeyFile *key_file = NULL;
      DesktopEntryDesktop *entry_desktop = (DesktopEntryDesktop*)entry;
      const char *categories_str;

      entry_desktop->appinfo = g_desktop_app_info_new_from_filename (entry->path);
      if (!G_IS_DESKTOP_APP_INFO (((DesktopEntryDesktop*)entry)->appinfo))
        {
          menu_verbose ("Failed to load \"%s\"\n", entry->path);
          return DESKTOP_ENTRY_LOAD_FAIL_APPINFO;
        }

      categories_str = g_desktop_app_info_get_categories (entry_desktop->appinfo);
      if (categories_str)
        {
          char **categories;
          int i;

          categories = g_strsplit (categories_str, ";", -1);
          entry_desktop->categories = g_new0 (GQuark, g_strv_length (categories) + 1);

          for (i = 0; categories[i]; i++)
            entry_desktop->categories[i] = g_quark_from_string (categories[i]);

          g_strfreev (categories);
        }

      key_file = g_key_file_new ();

      if (!g_key_file_load_from_file (key_file, entry->path, 0, NULL))
        entry_desktop->showin = TRUE;
      else
        entry_desktop->showin = key_file_get_show_in (key_file);

      g_key_file_free (key_file);

      return DESKTOP_ENTRY_LOAD_SUCCESS;
    }
  else if (entry->type == DESKTOP_ENTRY_DIRECTORY)
    {
      GKeyFile *key_file = NULL;
      GError   *error = NULL;
      DesktopEntryResultCode rescode = DESKTOP_ENTRY_LOAD_SUCCESS;

      key_file = g_key_file_new ();

      if (!g_key_file_load_from_file (key_file, entry->path, 0, &error))
        {
          rescode = DESKTOP_ENTRY_LOAD_FAIL_OTHER;
          goto out;
        }

      if (!desktop_entry_load_directory (entry, key_file, &error))
        {
          rescode = DESKTOP_ENTRY_LOAD_FAIL_OTHER;
          goto out;
        }

      rescode = DESKTOP_ENTRY_LOAD_SUCCESS;

    out:
      g_key_file_free (key_file);

      if (rescode == DESKTOP_ENTRY_LOAD_FAIL_OTHER)
        {
          if (error)
            {
              menu_verbose ("Failed to load \"%s\": %s\n", entry->path, error->message);
              g_error_free (error);
            }
          else
            menu_verbose ("Failed to load \"%s\"\n", entry->path);
        }

      return rescode;
    }
  else
    g_assert_not_reached ();

  return DESKTOP_ENTRY_LOAD_FAIL_OTHER;
}

DesktopEntry *
desktop_entry_new (const char             *path,
                   DesktopEntryResultCode *res_code)
{
  DesktopEntryType  type;
  DesktopEntry     *retval;
  DesktopEntryResultCode code;

  menu_verbose ("Loading desktop entry \"%s\"\n", path);

  if (g_str_has_suffix (path, ".desktop"))
    {
      type = DESKTOP_ENTRY_DESKTOP;
      retval = (DesktopEntry*)g_new0 (DesktopEntryDesktop, 1);
    }
  else if (g_str_has_suffix (path, ".directory"))
    {
      type = DESKTOP_ENTRY_DIRECTORY;
      retval = (DesktopEntry*)g_new0 (DesktopEntryDirectory, 1);
    }
  else
    {
      menu_verbose ("Unknown desktop entry suffix in \"%s\"\n",
                    path);
      *res_code = DESKTOP_ENTRY_LOAD_FAIL_OTHER;
      return NULL;
    }

  retval->refcount = 1;
  retval->type     = type;
  retval->path     = g_strdup (path);
  retval->basename = unix_basename_from_path (retval->path);

  code = desktop_entry_load (retval);
  *res_code = code;

  if (code < DESKTOP_ENTRY_LOAD_SUCCESS)
    {
      desktop_entry_unref (retval);
      return NULL;
    }

  return retval;
}

DesktopEntry *
desktop_entry_reload (DesktopEntry *entry)
{
  g_return_val_if_fail (entry != NULL, NULL);

  menu_verbose ("Re-loading desktop entry \"%s\"\n", entry->path);

  if (entry->type == DESKTOP_ENTRY_DESKTOP)
    {
      DesktopEntryDesktop *entry_desktop = (DesktopEntryDesktop *) entry;

      g_object_unref (entry_desktop->appinfo);
      entry_desktop->appinfo = NULL;

      g_free (entry_desktop->categories);
      entry_desktop->categories = NULL;
    }
  else if (entry->type == DESKTOP_ENTRY_DIRECTORY)
    {
      DesktopEntryDirectory *entry_directory = (DesktopEntryDirectory*) entry;

      g_free (entry_directory->name);
      entry_directory->name = NULL;

      g_free (entry_directory->comment);
      entry_directory->comment = NULL;

      g_object_unref (entry_directory->icon);
      entry_directory->icon = NULL;
    }
  else
    g_assert_not_reached ();

  if (desktop_entry_load (entry) < DESKTOP_ENTRY_LOAD_SUCCESS)
    {
      desktop_entry_unref (entry);
      return NULL;
    }

  return entry;
}

DesktopEntry *
desktop_entry_ref (DesktopEntry *entry)
{
  g_return_val_if_fail (entry != NULL, NULL);
  g_return_val_if_fail (entry->refcount > 0, NULL);

  entry->refcount += 1;

  return entry;
}

DesktopEntry *
desktop_entry_copy (DesktopEntry *entry)
{
  DesktopEntry *retval;

  menu_verbose ("Copying desktop entry \"%s\"\n",
                entry->basename);

  if (entry->type == DESKTOP_ENTRY_DESKTOP)
    retval = (DesktopEntry*)g_new0 (DesktopEntryDesktop, 1);
  else if (entry->type == DESKTOP_ENTRY_DIRECTORY)
    retval = (DesktopEntry*)g_new0 (DesktopEntryDirectory, 1);
  else
    g_assert_not_reached ();

  retval->refcount     = 1;
  retval->type         = entry->type;
  retval->path         = g_strdup (entry->path);
  retval->basename     = unix_basename_from_path (retval->path);

  if (retval->type == DESKTOP_ENTRY_DESKTOP)
    {
      DesktopEntryDesktop *desktop_entry = (DesktopEntryDesktop*) entry;
      DesktopEntryDesktop *retval_desktop_entry = (DesktopEntryDesktop*) retval;
      int i;

      retval_desktop_entry->appinfo = g_object_ref (desktop_entry->appinfo);

      if (desktop_entry->categories != NULL)
        {
          i = 0;
          for (; desktop_entry->categories[i]; i++);

          retval_desktop_entry->categories = g_new0 (GQuark, i + 1);

          i = 0;
          for (; desktop_entry->categories[i]; i++)
            retval_desktop_entry->categories[i] = desktop_entry->categories[i];
        }
      else
        retval_desktop_entry->categories = NULL;
    }
  else if (entry->type == DESKTOP_ENTRY_DIRECTORY)
    {
      DesktopEntryDirectory *entry_directory = (DesktopEntryDirectory*)entry;
      DesktopEntryDirectory *retval_directory = (DesktopEntryDirectory*)retval;

      retval_directory->name         = g_strdup (entry_directory->name);
      retval_directory->comment      = g_strdup (entry_directory->comment);
      retval_directory->icon         = g_object_ref (entry_directory->icon);
      retval_directory->nodisplay    = entry_directory->nodisplay;
      retval_directory->hidden       = entry_directory->hidden;
      retval_directory->showin       = entry_directory->showin;
    }

  return retval;
}

void
desktop_entry_unref (DesktopEntry *entry)
{
  g_return_if_fail (entry != NULL);
  g_return_if_fail (entry->refcount > 0);

  entry->refcount -= 1;
  if (entry->refcount != 0)
    return;

  g_free (entry->path);
  entry->path = NULL;

  if (entry->type == DESKTOP_ENTRY_DESKTOP)
    {
      DesktopEntryDesktop *desktop_entry = (DesktopEntryDesktop*) entry;
      g_free (desktop_entry->categories);
      if (desktop_entry->appinfo)
        g_object_unref (desktop_entry->appinfo);
    }
  else if (entry->type == DESKTOP_ENTRY_DIRECTORY)
    {
      DesktopEntryDirectory *entry_directory = (DesktopEntryDirectory*) entry;

      g_free (entry_directory->name);
      entry_directory->name = NULL;

      g_free (entry_directory->comment);
      entry_directory->comment = NULL;

      if (entry_directory->icon != NULL)
        {
          g_object_unref (entry_directory->icon);
          entry_directory->icon = NULL;
        }
    }
  else
    g_assert_not_reached ();

  g_free (entry);
}

DesktopEntryType
desktop_entry_get_type (DesktopEntry *entry)
{
  return entry->type;
}

const char *
desktop_entry_get_path (DesktopEntry *entry)
{
  return entry->path;
}

const char *
desktop_entry_get_basename (DesktopEntry *entry)
{
  return entry->basename;
}

const char *
desktop_entry_get_name (DesktopEntry *entry)
{
  if (entry->type == DESKTOP_ENTRY_DESKTOP)
    {
      g_return_val_if_fail (G_IS_DESKTOP_APP_INFO (((DesktopEntryDesktop*)entry)->appinfo), NULL);
      return g_app_info_get_name (G_APP_INFO (((DesktopEntryDesktop*)entry)->appinfo));
    }

  return ((DesktopEntryDirectory*)entry)->name;
}

const char *
desktop_entry_get_generic_name (DesktopEntry *entry)
{
  if (entry->type == DESKTOP_ENTRY_DESKTOP)
    {
      g_return_val_if_fail (G_IS_DESKTOP_APP_INFO (((DesktopEntryDesktop*)entry)->appinfo), NULL);
      return g_desktop_app_info_get_generic_name (((DesktopEntryDesktop*)entry)->appinfo);
    }

  return ((DesktopEntryDirectory*)entry)->generic_name;
}

const char *
desktop_entry_get_comment (DesktopEntry *entry)
{
  if (entry->type == DESKTOP_ENTRY_DESKTOP)
    {
      g_return_val_if_fail (G_IS_DESKTOP_APP_INFO (((DesktopEntryDesktop*)entry)->appinfo), NULL);
      return g_app_info_get_description (G_APP_INFO (((DesktopEntryDesktop*)entry)->appinfo));
    }

  return ((DesktopEntryDirectory*)entry)->comment;
}

GIcon *
desktop_entry_get_icon (DesktopEntry *entry)
{
  if (entry->type == DESKTOP_ENTRY_DESKTOP)
    {
      g_return_val_if_fail (G_IS_DESKTOP_APP_INFO (((DesktopEntryDesktop*)entry)->appinfo), NULL);
      return g_app_info_get_icon (G_APP_INFO (((DesktopEntryDesktop*)entry)->appinfo));
    }

  return ((DesktopEntryDirectory*)entry)->icon;
}

gboolean
desktop_entry_get_no_display (DesktopEntry *entry)
{
  if (entry->type == DESKTOP_ENTRY_DESKTOP)
    {
      g_return_val_if_fail (G_IS_DESKTOP_APP_INFO (((DesktopEntryDesktop*)entry)->appinfo), FALSE);
      return g_desktop_app_info_get_nodisplay (((DesktopEntryDesktop*)entry)->appinfo);
    }

  return ((DesktopEntryDirectory*)entry)->nodisplay;
}

gboolean
desktop_entry_get_hidden (DesktopEntry *entry)
{
  if (entry->type == DESKTOP_ENTRY_DESKTOP)
    {
      g_return_val_if_fail (G_IS_DESKTOP_APP_INFO (((DesktopEntryDesktop*)entry)->appinfo), FALSE);
      return g_desktop_app_info_get_is_hidden (((DesktopEntryDesktop*)entry)->appinfo);
    }

  return ((DesktopEntryDirectory*)entry)->hidden;
}

gboolean
desktop_entry_get_show_in (DesktopEntry *entry)
{
  if (entry->type == DESKTOP_ENTRY_DESKTOP)
    {
      const char *current_desktop = get_current_desktop ();

      if (current_desktop == NULL)
        return TRUE;
      else {
        return ((DesktopEntryDesktop *)entry)->showin;
      }
    }
  return ((DesktopEntryDirectory*)entry)->showin;
}


GDesktopAppInfo  *
desktop_entry_get_app_info (DesktopEntry *entry)
{
  g_return_val_if_fail (entry->type == DESKTOP_ENTRY_DESKTOP, NULL);
  return ((DesktopEntryDesktop*)entry)->appinfo;
}

gboolean
desktop_entry_has_categories (DesktopEntry *entry)
{
  DesktopEntryDesktop *desktop_entry;
  if (entry->type != DESKTOP_ENTRY_DESKTOP)
    return FALSE;

  desktop_entry = (DesktopEntryDesktop*) entry;
  return (desktop_entry->categories != NULL && desktop_entry->categories[0] != 0);
}

gboolean
desktop_entry_has_category (DesktopEntry *entry,
                            const char   *category)
{
  GQuark quark;
  int    i;
  DesktopEntryDesktop *desktop_entry;

  if (entry->type != DESKTOP_ENTRY_DESKTOP)
    return FALSE;

  desktop_entry = (DesktopEntryDesktop*) entry;

  if (desktop_entry->categories == NULL)
    return FALSE;

  if (!(quark = g_quark_try_string (category)))
    return FALSE;

  for (i = 0; desktop_entry->categories[i]; i++)
    {
      if (quark == desktop_entry->categories[i])
        return TRUE;
    }

  return FALSE;
}

/*
 * Entry sets
 */

DesktopEntrySet *
desktop_entry_set_new (void)
{
  DesktopEntrySet *set;

  set = g_new0 (DesktopEntrySet, 1);
  set->refcount = 1;

  menu_verbose (" New entry set %p\n", set);

  return set;
}

DesktopEntrySet *
desktop_entry_set_ref (DesktopEntrySet *set)
{
  g_return_val_if_fail (set != NULL, NULL);
  g_return_val_if_fail (set->refcount > 0, NULL);

  set->refcount += 1;

  return set;
}

void
desktop_entry_set_unref (DesktopEntrySet *set)
{
  g_return_if_fail (set != NULL);
  g_return_if_fail (set->refcount > 0);

  set->refcount -= 1;
  if (set->refcount == 0)
    {
      menu_verbose (" Deleting entry set %p\n", set);

      if (set->hash)
        g_hash_table_destroy (set->hash);
      set->hash = NULL;

      g_free (set);
    }
}

void
desktop_entry_set_add_entry (DesktopEntrySet *set,
                             DesktopEntry    *entry,
                             const char      *file_id)
{
  menu_verbose (" Adding to set %p entry %s\n", set, file_id);

  if (set->hash == NULL)
    {
      set->hash = g_hash_table_new_full (g_str_hash,
                                         g_str_equal,
                                         g_free,
                                         (GDestroyNotify) desktop_entry_unref);
    }

  g_hash_table_replace (set->hash,
                        g_strdup (file_id),
                        desktop_entry_ref (entry));
}

DesktopEntry *
desktop_entry_set_lookup (DesktopEntrySet *set,
                          const char      *file_id)
{
  if (set->hash == NULL)
    return NULL;

  return g_hash_table_lookup (set->hash, file_id);
}

typedef struct
{
  DesktopEntrySetForeachFunc func;
  gpointer                   user_data;
} EntryHashForeachData;

static void
entry_hash_foreach (const char           *file_id,
                    DesktopEntry         *entry,
                    EntryHashForeachData *fd)
{
  fd->func (file_id, entry, fd->user_data);
}

void
desktop_entry_set_foreach (DesktopEntrySet            *set,
                           DesktopEntrySetForeachFunc  func,
                           gpointer                    user_data)
{
  g_return_if_fail (set != NULL);
  g_return_if_fail (func != NULL);

  if (set->hash != NULL)
    {
      EntryHashForeachData fd;

      fd.func      = func;
      fd.user_data = user_data;

      g_hash_table_foreach (set->hash,
                            (GHFunc) entry_hash_foreach,
                            &fd);
    }
}

static void
desktop_entry_set_clear (DesktopEntrySet *set)
{
  menu_verbose (" Clearing set %p\n", set);

  if (set->hash != NULL)
    {
      g_hash_table_destroy (set->hash);
      set->hash = NULL;
    }
}

int
desktop_entry_set_get_count (DesktopEntrySet *set)
{
  if (set->hash == NULL)
    return 0;

  return g_hash_table_size (set->hash);
}

static void
union_foreach (const char      *file_id,
               DesktopEntry    *entry,
               DesktopEntrySet *set)
{
  /* we are iterating over "with" adding anything not
   * already in "set". We unconditionally overwrite
   * the stuff in "set" because we can assume
   * two entries with the same name are equivalent.
   */
  desktop_entry_set_add_entry (set, entry, file_id);
}

void
desktop_entry_set_union (DesktopEntrySet *set,
                         DesktopEntrySet *with)
{
  menu_verbose (" Union of %p and %p\n", set, with);

  if (desktop_entry_set_get_count (with) == 0)
    return; /* A fast simple case */

  g_hash_table_foreach (with->hash,
                        (GHFunc) union_foreach,
                        set);
}

typedef struct
{
  DesktopEntrySet *set;
  DesktopEntrySet *with;
} IntersectData;

static gboolean
intersect_foreach_remove (const char    *file_id,
                          DesktopEntry  *entry,
                          IntersectData *id)
{
  /* Remove everything in "set" which is not in "with" */

  if (g_hash_table_lookup (id->with->hash, file_id) != NULL)
    return FALSE;

  menu_verbose (" Removing from %p entry %s\n", id->set, file_id);

  return TRUE; /* return TRUE to remove */
}

void
desktop_entry_set_intersection (DesktopEntrySet *set,
                                DesktopEntrySet *with)
{
  IntersectData id;

  menu_verbose (" Intersection of %p and %p\n", set, with);

  if (desktop_entry_set_get_count (set) == 0 ||
      desktop_entry_set_get_count (with) == 0)
    {
      /* A fast simple case */
      desktop_entry_set_clear (set);
      return;
    }

  id.set  = set;
  id.with = with;

  g_hash_table_foreach_remove (set->hash,
                               (GHRFunc) intersect_foreach_remove,
                               &id);
}

typedef struct
{
  DesktopEntrySet *set;
  DesktopEntrySet *other;
} SubtractData;

static gboolean
subtract_foreach_remove (const char   *file_id,
                         DesktopEntry *entry,
                         SubtractData *sd)
{
  /* Remove everything in "set" which is not in "other" */

  if (g_hash_table_lookup (sd->other->hash, file_id) == NULL)
    return FALSE;

  menu_verbose (" Removing from %p entry %s\n", sd->set, file_id);

  return TRUE; /* return TRUE to remove */
}

void
desktop_entry_set_subtract (DesktopEntrySet *set,
                            DesktopEntrySet *other)
{
  SubtractData sd;

  menu_verbose (" Subtract from %p set %p\n", set, other);

  if (desktop_entry_set_get_count (set) == 0 ||
      desktop_entry_set_get_count (other) == 0)
    return; /* A fast simple case */

  sd.set   = set;
  sd.other = other;

  g_hash_table_foreach_remove (set->hash,
                               (GHRFunc) subtract_foreach_remove,
                               &sd);
}

void
desktop_entry_set_swap_contents (DesktopEntrySet *a,
                                 DesktopEntrySet *b)
{
  GHashTable *tmp;

  menu_verbose (" Swap contents of %p and %p\n", a, b);

  tmp = a->hash;
  a->hash = b->hash;
  b->hash = tmp;
}
