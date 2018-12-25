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

#include <config.h>

#include "entry-directories.h"

#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <dirent.h>
#include <stdlib.h>

#include "menu-util.h"
#include "menu-monitor.h"

typedef struct CachedDir CachedDir;
typedef struct CachedDirMonitor CachedDirMonitor;

struct EntryDirectory
{
  CachedDir *dir;

  guint entry_type : 2;
  guint refcount : 24;
};

struct EntryDirectoryList
{
  int    refcount;
  int    length;
  GList *dirs;
};

struct CachedDir
{
  CachedDir *parent;
  char      *name;

  GSList *entries;
  GSList *subdirs;
  GSList *retry_later_desktop_entries;

  MenuMonitor *dir_monitor;
  GSList      *monitors;

  guint have_read_entries : 1;
  guint deleted : 1;

  guint references;

  GFunc    notify;
  gpointer notify_data;
};

struct CachedDirMonitor
{
  EntryDirectory            *ed;
  EntryDirectoryChangedFunc  callback;
  gpointer                   user_data;
};

static void     cached_dir_add_reference          (CachedDir *dir);
static void     cached_dir_remove_reference       (CachedDir *dir);
static void     cached_dir_free                   (CachedDir  *dir);
static gboolean cached_dir_load_entries_recursive (CachedDir  *dir,
                                                   const char *dirname);
static void     cached_dir_unref                  (CachedDir *dir);
static void     cached_dir_unref_noparent         (CachedDir *dir);
static CachedDir * cached_dir_add_subdir          (CachedDir  *dir,
                                                   const char *basename,
                                                   const char *path);
static gboolean cached_dir_remove_subdir          (CachedDir  *dir,
                                                   const char *basename);

static void handle_cached_dir_changed (MenuMonitor      *monitor,
				       MenuMonitorEvent  event,
				       const char       *path,
				       CachedDir        *dir);

/*
 * Entry directory cache
 */

static CachedDir *dir_cache = NULL;

static void
clear_cache (CachedDir *dir,
             gpointer  *cache)
{
  *cache = NULL;
}

static CachedDir *
cached_dir_new (const char *name)
{
  CachedDir *dir;

  dir = g_new0 (CachedDir, 1);
  dir->name = g_strdup (name);

  return dir;
}

static CachedDir *
cached_dir_new_full (const char *name,
                     GFunc       notify,
                     gpointer    notify_data)
{
  CachedDir *dir;

  dir = cached_dir_new (name);

  dir->notify = notify;
  dir->notify_data = notify_data;

  return dir;
}

static void
cached_dir_free (CachedDir *dir)
{
  if (dir->dir_monitor)
    {
      menu_monitor_remove_notify (dir->dir_monitor,
				  (MenuMonitorNotifyFunc) handle_cached_dir_changed,
				  dir);
      menu_monitor_unref (dir->dir_monitor);
      dir->dir_monitor = NULL;
    }

  g_slist_foreach (dir->monitors, (GFunc) g_free, NULL);
  g_slist_free (dir->monitors);
  dir->monitors = NULL;

  g_slist_foreach (dir->entries,
                   (GFunc) desktop_entry_unref,
                   NULL);
  g_slist_free (dir->entries);
  dir->entries = NULL;

  g_slist_foreach (dir->subdirs,
                   (GFunc) cached_dir_unref_noparent,
                   NULL);
  g_slist_free (dir->subdirs);
  dir->subdirs = NULL;

  g_slist_free_full (dir->retry_later_desktop_entries, g_free);
  dir->retry_later_desktop_entries = NULL;

  g_free (dir->name);
  g_free (dir);
}

static CachedDir *
cached_dir_ref (CachedDir *dir)
{
  dir->references++;
  return dir;
}

static void
cached_dir_unref (CachedDir *dir)
{
  if (--dir->references == 0)
    {
      CachedDir *parent;

      parent = dir->parent;

      if (parent != NULL)
        cached_dir_remove_subdir (parent, dir->name);

      if (dir->notify)
        dir->notify (dir, dir->notify_data);

      cached_dir_free (dir);
    }
}

static void
cached_dir_unref_noparent (CachedDir *dir)
{
  if (--dir->references == 0)
    {
      if (dir->notify)
        dir->notify (dir, dir->notify_data);

      cached_dir_free (dir);
    }
}

static inline CachedDir *
find_subdir (CachedDir  *dir,
             const char *subdir)
{
  GSList *tmp;

  tmp = dir->subdirs;
  while (tmp != NULL)
    {
      CachedDir *sub = tmp->data;

      if (strcmp (sub->name, subdir) == 0)
        return sub;

      tmp = tmp->next;
    }

  return NULL;
}

static DesktopEntry *
find_entry (CachedDir  *dir,
            const char *basename)
{
  GSList *tmp;

  tmp = dir->entries;
  while (tmp != NULL)
    {
      if (strcmp (desktop_entry_get_basename (tmp->data), basename) == 0)
        return tmp->data;

      tmp = tmp->next;
    }

  return NULL;
}

static DesktopEntry *
cached_dir_find_relative_path (CachedDir  *dir,
                               const char *relative_path)
{
  DesktopEntry  *retval = NULL;
  char         **split;
  int            i;

  split = g_strsplit (relative_path, "/", -1);

  i = 0;
  while (split[i] != NULL)
    {
      if (split[i + 1] != NULL)
        {
          if ((dir = find_subdir (dir, split[i])) == NULL)
            break;
        }
      else
        {
          retval = find_entry (dir, split[i]);
          break;
        }

      ++i;
    }

  g_strfreev (split);

  return retval;
}

static CachedDir *
cached_dir_lookup (const char *canonical)
{
  CachedDir  *dir;
  char      **split;
  int         i;

  if (dir_cache == NULL)
    dir_cache = cached_dir_new_full ("/",
                                     (GFunc) clear_cache,
                                     &dir_cache);
  dir = dir_cache;

  g_assert (canonical != NULL && canonical[0] == G_DIR_SEPARATOR);

  menu_verbose ("Looking up cached dir \"%s\"\n", canonical);

  split = g_strsplit (canonical + 1, "/", -1);

  i = 0;
  while (split[i] != NULL)
    {
      CachedDir *subdir;

      subdir = cached_dir_add_subdir (dir, split[i], NULL);

      dir = subdir;

      ++i;
    }

  g_strfreev (split);

  g_assert (dir != NULL);

  return dir;
}

static gboolean
cached_dir_add_entry (CachedDir  *dir,
                      const char *basename,
                      const char *path)
{
  DesktopEntry *entry;
  DesktopEntryResultCode code;

  entry = desktop_entry_new (path, &code);
  if (entry == NULL)
    {
      if (code == DESKTOP_ENTRY_LOAD_FAIL_APPINFO)
        {
          menu_verbose ("Adding %s to the retry list (mimeinfo.cache maybe isn't done getting updated yet\n", path);

          dir->retry_later_desktop_entries = g_slist_prepend (dir->retry_later_desktop_entries, g_strdup (path));
        }

      return FALSE;
    }

  dir->entries = g_slist_prepend (dir->entries, entry);

  return TRUE;
}

static gboolean
cached_dir_update_entry (CachedDir  *dir,
                         const char *basename,
                         const char *path)
{
  GSList *tmp;

  tmp = dir->entries;
  while (tmp != NULL)
    {
      if (strcmp (desktop_entry_get_basename (tmp->data), basename) == 0)
        {
          if (!desktop_entry_reload (tmp->data))
            {
              dir->entries = g_slist_delete_link (dir->entries, tmp);
            }

          return TRUE;
        }

      tmp = tmp->next;
    }

  return cached_dir_add_entry (dir, basename, path);
}

static gboolean
cached_dir_remove_entry (CachedDir  *dir,
                         const char *basename)
{
  GSList *tmp;

  tmp = dir->entries;
  while (tmp != NULL)
    {
      if (strcmp (desktop_entry_get_basename (tmp->data), basename) == 0)
        {
          desktop_entry_unref (tmp->data);
          dir->entries = g_slist_delete_link (dir->entries, tmp);
          return TRUE;
        }

      tmp = tmp->next;
    }

  return FALSE;
}

static CachedDir *
cached_dir_add_subdir (CachedDir  *dir,
                       const char *basename,
                       const char *path)
{
  CachedDir *subdir;

  subdir = find_subdir (dir, basename);

  if (subdir != NULL)
    {
      subdir->deleted = FALSE;
      return subdir;
    }

  subdir = cached_dir_new (basename);

  if (path != NULL && !cached_dir_load_entries_recursive (subdir, path))
    {
      cached_dir_free (subdir);
      return NULL;
    }

  menu_verbose ("Caching dir \"%s\"\n", basename);

  subdir->parent = dir;
  dir->subdirs = g_slist_prepend (dir->subdirs, cached_dir_ref (subdir));

  return subdir;
}

static gboolean
cached_dir_remove_subdir (CachedDir  *dir,
                          const char *basename)
{
  CachedDir *subdir;

  subdir = find_subdir (dir, basename);

  if (subdir != NULL)
    {
      subdir->deleted = TRUE;

      cached_dir_unref (subdir);
      dir->subdirs = g_slist_remove (dir->subdirs, subdir);

      return TRUE;
    }

  return FALSE;
}

static guint   monitors_idle_handler = 0;
static GSList *pending_monitors_dirs = NULL;

static void
cached_dir_invoke_monitors (CachedDir *dir)
{
  GSList *tmp;

  tmp = dir->monitors;
  while (tmp != NULL)
    {
      CachedDirMonitor *monitor = tmp->data;
      GSList           *next    = tmp->next;

      monitor->callback (monitor->ed, monitor->user_data);

      tmp = next;
    }

  /* we explicitly don't invoke monitors of the parent since an
   * event has been queued for it too */
}

static gboolean
emit_monitors_in_idle (void)
{
  GSList *monitors_to_emit;
  GSList *tmp;

  monitors_to_emit = pending_monitors_dirs;

  pending_monitors_dirs = NULL;
  monitors_idle_handler = 0;

  tmp = monitors_to_emit;
  while (tmp != NULL)
    {
      CachedDir *dir = tmp->data;

      cached_dir_invoke_monitors (dir);
      cached_dir_remove_reference (dir);

      tmp = tmp->next;
    }

  g_slist_free (monitors_to_emit);

  return FALSE;
}

static void
cached_dir_queue_monitor_event (CachedDir *dir)
{
  GSList *tmp;

  tmp = pending_monitors_dirs;
  while (tmp != NULL)
    {
      CachedDir *d    = tmp->data;
      GSList    *next = tmp->next;

      if (dir->parent == d->parent &&
          g_strcmp0 (dir->name, d->name) == 0)
        break;

      tmp = next;
    }

  /* not found, so let's queue it */
  if (tmp == NULL)
    {
      cached_dir_add_reference (dir);
      pending_monitors_dirs = g_slist_append (pending_monitors_dirs, dir);
    }

  if (dir->parent)
    {
      cached_dir_queue_monitor_event (dir->parent);
    }

  if (monitors_idle_handler > 0)
    {
      g_source_remove (monitors_idle_handler);
    }

    monitors_idle_handler = g_timeout_add (100, (GSourceFunc) emit_monitors_in_idle, NULL);
}

static void
handle_cached_dir_changed (MenuMonitor      *monitor,
			   MenuMonitorEvent  event,
			   const char       *path,
                           CachedDir        *dir)
{
  gboolean  handled = FALSE;
  gboolean  retry_changes = FALSE;

  char     *basename;
  char     *dirname;

  dirname  = g_path_get_dirname  (path);
  basename = g_path_get_basename (path);

  dir = cached_dir_lookup (dirname);
  cached_dir_add_reference (dir);

  if (g_str_has_suffix (basename, ".desktop") ||
      g_str_has_suffix (basename, ".directory"))
    {
      switch (event)
        {
        case MENU_MONITOR_EVENT_CREATED:
        case MENU_MONITOR_EVENT_CHANGED:
          handled = cached_dir_update_entry (dir, basename, path);
          break;

        case MENU_MONITOR_EVENT_DELETED:
          handled = cached_dir_remove_entry (dir, basename);
          break;

        default:
          g_assert_not_reached ();
          break;
        }
    }
  else if (g_strcmp0 (basename, "mimeinfo.cache") == 0)
    {
        /* The observed file notifies when a new desktop file is added
         * (but fails to load) go something like:
         *
         * NOTIFY: foo.desktop
         * NOTIFY: mimeinfo.cache.tempfile
         * NOTIFY: mimeinfo.cache.tempfile
         * NOTIFY: mimeinfo.cache
         *
         * Additionally, the failure is not upon trying to read the file,
         * but attempting to get its GAppInfo (g_desktop_app_info_new_from_filename()
         * in desktop-entries.c ln 277).  If you jigger desktop_entry_load() around 
         * and read the file as a keyfile *first*, it succeeds.  If you then try
         * to run g_desktop_app_info_new_from_keyfile(), *then* it fails.
         *
         * The theory here is there is a race condition where app info (which includes
         * mimetype stuff) is unavailable because mimeinfo.cache is updated immediately
         * after the app is installed.
         *
         * What we do here is, when a desktop fails to load, we add it to a temporary
         * list.  We wait until mimeinfo.cache changes, then retry that desktop file,
         * which succeeds this second time.
         *
         * Note: An alternative fix (presented more as a proof than a suggestion) is to
         * change line 151 in menu-monitor.c to use g_timeout_add_seconds, and delay
         * for one second.  This also avoids the issue (but it remains a race condition).
         */

        GSList *iter;

        menu_verbose ("mimeinfo changed, checking for failed entries\n");

        for (iter = dir->retry_later_desktop_entries; iter != NULL; iter = iter->next)
          {
            const gchar *retry_path = iter->data;

            menu_verbose ("retrying %s\n", retry_path);

            char *retry_basename = g_path_get_basename (retry_path);

            if (cached_dir_update_entry (dir, retry_basename, retry_path))
              retry_changes = TRUE;

            g_free (retry_basename);
          }

        g_slist_free_full (dir->retry_later_desktop_entries, g_free);
        dir->retry_later_desktop_entries = NULL;

        handled = retry_changes;
    }
  else /* Try recursing */
    {
      switch (event)
        {
        case MENU_MONITOR_EVENT_CREATED:
          handled = cached_dir_add_subdir (dir, basename, path) != NULL;
          break;

        case MENU_MONITOR_EVENT_CHANGED:
          break;

        case MENU_MONITOR_EVENT_DELETED:
          handled = cached_dir_remove_subdir (dir, basename);
          break;

        default:
          g_assert_not_reached ();
          break;
        }
    }

  g_free (basename);
  g_free (dirname);

  if (handled)
    {
      menu_verbose ("'%s' notified of '%s' %s - invalidating cache\n",
                    dir->name,
                    path,
                    event == MENU_MONITOR_EVENT_CREATED ? ("created") :
                    event == MENU_MONITOR_EVENT_DELETED ? ("deleted") : ("changed"));

      /* CHANGED events don't change the set of desktop entries, unless it's the mimeinfo.cache file changing */
      if (retry_changes || (event == MENU_MONITOR_EVENT_CREATED || event == MENU_MONITOR_EVENT_DELETED))
        {
          _entry_directory_list_empty_desktop_cache ();
        }

      cached_dir_queue_monitor_event (dir);
    }

  cached_dir_remove_reference (dir);
}

static void
cached_dir_ensure_monitor (CachedDir  *dir,
                           const char *dirname)
{
  if (dir->dir_monitor == NULL)
    {
      dir->dir_monitor = menu_get_directory_monitor (dirname);
      menu_monitor_add_notify (dir->dir_monitor,
			       (MenuMonitorNotifyFunc) handle_cached_dir_changed,
			       dir);
    }
}

static gboolean
cached_dir_load_entries_recursive (CachedDir  *dir,
                                   const char *dirname)
{
  DIR           *dp;
  struct dirent *dent;
  GString       *fullpath;
  gsize          fullpath_len;

  g_assert (dir != NULL);

  if (dir->have_read_entries)
    return TRUE;

  menu_verbose ("Attempting to read entries from %s (full path %s)\n",
                dir->name, dirname);

  dp = opendir (dirname);
  if (dp == NULL)
    {
      menu_verbose ("Unable to list directory \"%s\"\n",
                    dirname);
      return FALSE;
    }

  cached_dir_ensure_monitor (dir, dirname);

  fullpath = g_string_new (dirname);
  if (fullpath->str[fullpath->len - 1] != G_DIR_SEPARATOR)
    g_string_append_c (fullpath, G_DIR_SEPARATOR);

  fullpath_len = fullpath->len;

  while ((dent = readdir (dp)) != NULL)
    {
      /* ignore . and .. */
      if (dent->d_name[0] == '.' &&
          (dent->d_name[1] == '\0' ||
           (dent->d_name[1] == '.' &&
            dent->d_name[2] == '\0')))
        continue;

      g_string_append (fullpath, dent->d_name);

      if (g_str_has_suffix (dent->d_name, ".desktop") ||
          g_str_has_suffix (dent->d_name, ".directory"))
        {
          cached_dir_add_entry (dir, dent->d_name, fullpath->str);
        }
      else /* Try recursing */
        {
          cached_dir_add_subdir (dir, dent->d_name, fullpath->str);
        }

      g_string_truncate (fullpath, fullpath_len);
    }

  closedir (dp);

  g_string_free (fullpath, TRUE);

  dir->have_read_entries = TRUE;

  return TRUE;
}

static void
cached_dir_add_monitor (CachedDir                 *dir,
                        EntryDirectory            *ed,
                        EntryDirectoryChangedFunc  callback,
                        gpointer                   user_data)
{
  CachedDirMonitor *monitor;
  GSList           *tmp;

  tmp = dir->monitors;
  while (tmp != NULL)
    {
      monitor = tmp->data;

      if (monitor->ed == ed &&
          monitor->callback == callback &&
          monitor->user_data == user_data)
        break;

      tmp = tmp->next;
    }

  if (tmp == NULL)
    {
      monitor            = g_new0 (CachedDirMonitor, 1);
      monitor->ed        = ed;
      monitor->callback  = callback;
      monitor->user_data = user_data;

      dir->monitors = g_slist_append (dir->monitors, monitor);
    }
}

static void
cached_dir_remove_monitor (CachedDir                 *dir,
                           EntryDirectory            *ed,
                           EntryDirectoryChangedFunc  callback,
                           gpointer                   user_data)
{
  GSList *tmp;

  tmp = dir->monitors;
  while (tmp != NULL)
    {
      CachedDirMonitor *monitor = tmp->data;
      GSList           *next = tmp->next;

      if (monitor->ed == ed &&
          monitor->callback == callback &&
          monitor->user_data == user_data)
        {
          dir->monitors = g_slist_delete_link (dir->monitors, tmp);
          g_free (monitor);
        }

      tmp = next;
    }
}

static void
cached_dir_add_reference (CachedDir *dir)
{
  cached_dir_ref (dir);

  if (dir->parent != NULL)
    {
      cached_dir_add_reference (dir->parent);
    }
}

static void
cached_dir_remove_reference (CachedDir *dir)
{
  CachedDir *parent;

  parent = dir->parent;

  cached_dir_unref (dir);

  if (parent != NULL)
    {
      cached_dir_remove_reference (parent);
    }
}

/*
 * Entry directories
 */

EntryDirectory *
entry_directory_new (DesktopEntryType  entry_type,
                     const char       *path)
{
  EntryDirectory *ed;
  char           *canonical;

  menu_verbose ("Loading entry directory \"%s\"\n", path);

  canonical = realpath (path, NULL);
  if (canonical == NULL)
    {
      menu_verbose ("Failed to canonicalize \"%s\": %s\n",
                    path, g_strerror (errno));
      return NULL;
    }

  ed = g_new0 (EntryDirectory, 1);

  ed->dir = cached_dir_lookup (canonical);
  g_assert (ed->dir != NULL);

  cached_dir_add_reference (ed->dir);
  cached_dir_load_entries_recursive (ed->dir, canonical);

  ed->entry_type    = entry_type;
  ed->refcount      = 1;

  g_free (canonical);

  return ed;
}

EntryDirectory *
entry_directory_ref (EntryDirectory *ed)
{
  g_return_val_if_fail (ed != NULL, NULL);
  g_return_val_if_fail (ed->refcount > 0, NULL);

  ed->refcount++;

  return ed;
}

void
entry_directory_unref (EntryDirectory *ed)
{
  g_return_if_fail (ed != NULL);
  g_return_if_fail (ed->refcount > 0);

  if (--ed->refcount == 0)
    {
      cached_dir_remove_reference (ed->dir);

      ed->dir        = NULL;
      ed->entry_type = DESKTOP_ENTRY_INVALID;

      g_free (ed);
    }
}

static void
entry_directory_add_monitor (EntryDirectory            *ed,
                             EntryDirectoryChangedFunc  callback,
                             gpointer                   user_data)
{
  cached_dir_add_monitor (ed->dir, ed, callback, user_data);
}

static void
entry_directory_remove_monitor (EntryDirectory            *ed,
                                EntryDirectoryChangedFunc  callback,
                                gpointer                   user_data)
{
  cached_dir_remove_monitor (ed->dir, ed, callback, user_data);
}

static DesktopEntry *
entry_directory_get_directory (EntryDirectory *ed,
                               const char     *relative_path)
{
  DesktopEntry *entry;

  if (ed->entry_type != DESKTOP_ENTRY_DIRECTORY)
    return NULL;

  entry = cached_dir_find_relative_path (ed->dir, relative_path);
  if (entry == NULL || desktop_entry_get_type (entry) != DESKTOP_ENTRY_DIRECTORY)
    return NULL;

  return desktop_entry_ref (entry);
}

static char *
get_desktop_file_id_from_path (EntryDirectory   *ed,
			       DesktopEntryType  entry_type,
			       const char       *relative_path)
{
  char *retval;
  
  retval = NULL;

  if (entry_type == DESKTOP_ENTRY_DESKTOP)
    {
      retval = g_strdelimit (g_strdup (relative_path), "/", '-');
    }
  else
    {
      retval = g_strdup (relative_path);
    }

  return retval;
}

typedef gboolean (* EntryDirectoryForeachFunc) (EntryDirectory  *ed,
                                                DesktopEntry    *entry,
                                                const char      *file_id,
                                                DesktopEntrySet *set,
                                                gpointer         user_data);

static gboolean
entry_directory_foreach_recursive (EntryDirectory            *ed,
                                   CachedDir                 *cd,
                                   GString                   *relative_path,
                                   EntryDirectoryForeachFunc  func,
                                   DesktopEntrySet           *set,
                                   gpointer                   user_data)
{
  GSList *tmp;
  int     relative_path_len;

  if (cd->deleted)
    return TRUE;

  relative_path_len = relative_path->len;

  tmp = cd->entries;
  while (tmp != NULL)
    {
      DesktopEntry *entry = tmp->data;

      if (desktop_entry_get_type (entry) == ed->entry_type)
        {
          gboolean  ret;
          char     *file_id;

          g_string_append (relative_path,
                           desktop_entry_get_basename (entry));

	  file_id = get_desktop_file_id_from_path (ed,
						   ed->entry_type,
						   relative_path->str);

          ret = func (ed, entry, file_id, set, user_data);

          g_free (file_id);

          g_string_truncate (relative_path, relative_path_len);

          if (!ret)
            return FALSE;
        }

      tmp = tmp->next;
    }

  tmp = cd->subdirs;
  while (tmp != NULL)
    {
      CachedDir *subdir = tmp->data;

      g_string_append (relative_path, subdir->name);
      g_string_append_c (relative_path, G_DIR_SEPARATOR);

      if (!entry_directory_foreach_recursive (ed,
                                              subdir,
                                              relative_path,
                                              func,
                                              set,
                                              user_data))
        return FALSE;

      g_string_truncate (relative_path, relative_path_len);

      tmp = tmp->next;
    }

  return TRUE;
}

static void
entry_directory_foreach (EntryDirectory            *ed,
                         EntryDirectoryForeachFunc  func,
                         DesktopEntrySet           *set,
                         gpointer                   user_data)
{
  GString *path;

  path = g_string_new (NULL);

  entry_directory_foreach_recursive (ed,
                                     ed->dir,
                                     path,
                                     func,
                                     set,
                                     user_data);

  g_string_free (path, TRUE);
}

void
entry_directory_get_flat_contents (EntryDirectory   *ed,
                                   DesktopEntrySet  *desktop_entries,
                                   DesktopEntrySet  *directory_entries,
                                   GSList          **subdirs)
{
  GSList *tmp;

  if (subdirs)
    *subdirs = NULL;

  tmp = ed->dir->entries;
  while (tmp != NULL)
    {
      DesktopEntry *entry = tmp->data;
      const char   *basename;

      basename = desktop_entry_get_basename (entry);

      if (desktop_entries &&
          desktop_entry_get_type (entry) == DESKTOP_ENTRY_DESKTOP)
        {
          char *file_id;

          file_id = get_desktop_file_id_from_path (ed,
						   DESKTOP_ENTRY_DESKTOP,
						   basename);

          desktop_entry_set_add_entry (desktop_entries,
                                       entry,
                                       file_id);

          g_free (file_id);
        }

      if (directory_entries &&
          desktop_entry_get_type (entry) == DESKTOP_ENTRY_DIRECTORY)
        {
          desktop_entry_set_add_entry (directory_entries,
				       entry,
				       basename);
        }

      tmp = tmp->next;
    }

  if (subdirs)
    {
      tmp = ed->dir->subdirs;
      while (tmp != NULL)
        {
          CachedDir *cd = tmp->data;

	  if (!cd->deleted)
	    {
	      *subdirs = g_slist_prepend (*subdirs, g_strdup (cd->name));
	    }

          tmp = tmp->next;
        }
    }

  if (subdirs)
    *subdirs = g_slist_reverse (*subdirs);
}

/*
 * Entry directory lists
 */

EntryDirectoryList *
entry_directory_list_new (void)
{
  EntryDirectoryList *list;

  list = g_new0 (EntryDirectoryList, 1);

  list->refcount = 1;
  list->dirs = NULL;
  list->length = 0;

  return list;
}

EntryDirectoryList *
entry_directory_list_ref (EntryDirectoryList *list)
{
  g_return_val_if_fail (list != NULL, NULL);
  g_return_val_if_fail (list->refcount > 0, NULL);

  list->refcount += 1;

  return list;
}

void
entry_directory_list_unref (EntryDirectoryList *list)
{
  g_return_if_fail (list != NULL);
  g_return_if_fail (list->refcount > 0);

  list->refcount -= 1;
  if (list->refcount == 0)
    {
      g_list_foreach (list->dirs, (GFunc) entry_directory_unref, NULL);
      g_list_free (list->dirs);
      list->dirs = NULL;
      list->length = 0;
      g_free (list);
    }
}

void
entry_directory_list_prepend  (EntryDirectoryList *list,
                               EntryDirectory     *ed)
{
  list->length += 1;
  list->dirs = g_list_prepend (list->dirs,
                               entry_directory_ref (ed));
}

int
entry_directory_list_get_length (EntryDirectoryList *list)
{
  return list->length;
}

void
entry_directory_list_append_list (EntryDirectoryList *list,
                                  EntryDirectoryList *to_append)
{
  GList *tmp;
  GList *new_dirs = NULL;

  if (to_append->length == 0)
    return;

  tmp = to_append->dirs;
  while (tmp != NULL)
    {
      list->length += 1;
      new_dirs = g_list_prepend (new_dirs,
                                 entry_directory_ref (tmp->data));

      tmp = tmp->next;
    }

  new_dirs   = g_list_reverse (new_dirs);
  list->dirs = g_list_concat (list->dirs, new_dirs);
}

DesktopEntry *
entry_directory_list_get_directory (EntryDirectoryList *list,
                                    const char         *relative_path)
{
  DesktopEntry *retval = NULL;
  GList        *tmp;

  tmp = list->dirs;
  while (tmp != NULL)
    {
      if ((retval = entry_directory_get_directory (tmp->data, relative_path)) != NULL)
        break;

      tmp = tmp->next;
    }

  return retval;
}

gboolean
_entry_directory_list_compare (const EntryDirectoryList *a,
                               const EntryDirectoryList *b)
{
  GList *al, *bl;

  if (a == NULL && b == NULL)
    return TRUE;

  if ((a == NULL || b == NULL))
    return FALSE;

  if (a->length != b->length)
    return FALSE;

  al = a->dirs; bl = b->dirs;
  while (al && bl && al->data == bl->data)
    {
      al = al->next;
      bl = bl->next;
    }

  return (al == NULL && bl == NULL);
}

static gboolean
get_all_func (EntryDirectory   *ed,
              DesktopEntry     *entry,
              const char       *file_id,
              DesktopEntrySet  *set,
              gpointer          user_data)
{
  entry = desktop_entry_ref (entry);

  desktop_entry_set_add_entry (set, entry, file_id);
  desktop_entry_unref (entry);

  return TRUE;
}

static DesktopEntrySet    *entry_directory_last_set = NULL;
static EntryDirectoryList *entry_directory_last_list = NULL;

void
_entry_directory_list_empty_desktop_cache (void)
{
  if (entry_directory_last_set != NULL)
    desktop_entry_set_unref (entry_directory_last_set);
  entry_directory_last_set = NULL;

  if (entry_directory_last_list != NULL)
    entry_directory_list_unref (entry_directory_last_list);
  entry_directory_last_list = NULL;
}

DesktopEntrySet *
_entry_directory_list_get_all_desktops (EntryDirectoryList *list)
{
  GList *tmp;
  DesktopEntrySet *set;

  /* The only tricky thing here is that desktop files later
   * in the search list with the same relative path
   * are "hidden" by desktop files earlier in the path,
   * so we have to do the earlier files first causing
   * the later files to replace the earlier files
   * in the DesktopEntrySet
   *
   * We go from the end of the list so we can just
   * g_hash_table_replace and not have to do two
   * hash lookups (check for existing entry, then insert new
   * entry)
   */

  /* This method is -extremely- slow, so we have a simple
     one-entry cache here */
  if (_entry_directory_list_compare (list, entry_directory_last_list))
    {
      menu_verbose (" Hit desktop list (%p) cache\n", list);
      return desktop_entry_set_ref (entry_directory_last_set);
    }

  if (entry_directory_last_set != NULL)
    desktop_entry_set_unref (entry_directory_last_set);
  if (entry_directory_last_list != NULL)
    entry_directory_list_unref (entry_directory_last_list);

  set = desktop_entry_set_new ();
  menu_verbose (" Storing all of list %p in set %p\n",
                list, set);

  tmp = g_list_last (list->dirs);
  while (tmp != NULL)
    {
      entry_directory_foreach (tmp->data, get_all_func, set, NULL);

      tmp = tmp->prev;
    }

  entry_directory_last_list = entry_directory_list_ref (list);
  entry_directory_last_set = desktop_entry_set_ref (set);

  return set;
}

void
entry_directory_list_add_monitors (EntryDirectoryList        *list,
                                   EntryDirectoryChangedFunc  callback,
                                   gpointer                   user_data)
{
  GList *tmp;

  tmp = list->dirs;
  while (tmp != NULL)
    {
      entry_directory_add_monitor (tmp->data, callback, user_data);
      tmp = tmp->next;
    }
}

void
entry_directory_list_remove_monitors (EntryDirectoryList        *list,
                                      EntryDirectoryChangedFunc  callback,
                                      gpointer                   user_data)
{
  GList *tmp;

  tmp = list->dirs;
  while (tmp != NULL)
    {
      entry_directory_remove_monitor (tmp->data, callback, user_data);
      tmp = tmp->next;
    }
}
