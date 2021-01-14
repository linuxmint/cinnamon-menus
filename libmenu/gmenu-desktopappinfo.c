/*
 * gmenu-desktopappinfo.c
 * Copyright (C) 2020 Lars Mueller <cobinja@yahoo.de>
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
 * Free Software Foundation, 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110, USA.
 */

#include "gmenu-desktopappinfo.h"
#include <gio/gdesktopappinfo.h>

struct _GMenuDesktopAppInfo
{
  GObject parent_instance;
  
  GDesktopAppInfo *super_appinfo;
  gchar *desktop_id;
  gboolean is_flatpak;
  gchar *startup_wm_class;
  gchar *flatpak_app_id;
};

// This function sets desktop id and startup wm class and adds ":flatpak" for flatpak apps
static void
update_app_data (GMenuDesktopAppInfo *info)
{
  gchar *exec = NULL;
  gchar *id = NULL;
  gchar *startup_wm_class = NULL;

  g_free (info->desktop_id);
  info->desktop_id = NULL;

  g_free (info->startup_wm_class);
  info->startup_wm_class = NULL;

  g_free (info->flatpak_app_id);
  info->flatpak_app_id = NULL;

  if (info->super_appinfo != NULL)
  {
    exec = (gchar *) g_app_info_get_executable (G_APP_INFO (info->super_appinfo));
    id = (gchar *) g_app_info_get_id (G_APP_INFO (info->super_appinfo));
    startup_wm_class = (gchar *) g_desktop_app_info_get_startup_wm_class (info->super_appinfo);

    if (exec && (strstr (exec, "flatpak") || strstr (exec, "bwrap")))
    {
      info->desktop_id = g_strconcat (id, GMENU_DESKTOPAPPINFO_FLATPAK_SUFFIX, NULL);
      if (startup_wm_class)
      {
        info->startup_wm_class = g_strconcat (startup_wm_class, GMENU_DESKTOPAPPINFO_FLATPAK_SUFFIX, NULL);
      }

      // if (g_desktop_app_info_has_key (info->super_appinfo, "X-Flatpak"))
      // {
        info->flatpak_app_id = g_desktop_app_info_get_string (info->super_appinfo, "X-Flatpak");
      // }

      info->is_flatpak = TRUE;
    } else {
      info->desktop_id = g_strdup (id);
      info->is_flatpak = FALSE;
      info->startup_wm_class = g_strdup (startup_wm_class);
    }
  }
}

static GAppInfo *
gmenu_desktopappinfo_dup (GAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  GMenuDesktopAppInfo *new_info;

  new_info = g_object_new (GMENU_TYPE_DESKTOPAPPINFO, NULL);
  
  new_info->super_appinfo = G_DESKTOP_APP_INFO (g_app_info_dup(G_APP_INFO(info->super_appinfo)));
  
  update_app_data (new_info);
  
  return G_APP_INFO (new_info);
}

static gboolean
gmenu_desktopappinfo_equal (GAppInfo *appinfo1,
                              GAppInfo *appinfo2)
{
  GMenuDesktopAppInfo *info1 = GMENU_DESKTOPAPPINFO (appinfo1);
  GMenuDesktopAppInfo *info2 = GMENU_DESKTOPAPPINFO (appinfo2);

  if (info1->desktop_id == NULL ||
      info2->desktop_id == NULL)
    return info1 == info2;

  return strcmp (info1->desktop_id, info2->desktop_id) == 0;
}

static const char *
gmenu_desktopappinfo_get_id (GAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);

  return info->desktop_id;
}

static const char *
gmenu_desktopappinfo_get_name (GAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_get_name (G_APP_INFO(info->super_appinfo));
}

static const char *
gmenu_desktopappinfo_get_description (GAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_get_description (G_APP_INFO(info->super_appinfo));
}

static const char *
gmenu_desktopappinfo_get_executable (GAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_get_executable (G_APP_INFO(info->super_appinfo));
}

static GIcon *
gmenu_desktopappinfo_get_icon (GAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_get_icon (G_APP_INFO(info->super_appinfo));
}

static gboolean
gmenu_desktopappinfo_launch (GAppInfo *appinfo, GList *files, GAppLaunchContext *launch_context, GError **error)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_launch (G_APP_INFO(info->super_appinfo), files, launch_context, error);
}

static gboolean
gmenu_desktopappinfo_supports_uris (GAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_supports_uris (G_APP_INFO(info->super_appinfo));
}

static gboolean
gmenu_desktopappinfo_supports_files (GAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_supports_files (G_APP_INFO(info->super_appinfo));
}

static gboolean
gmenu_desktopappinfo_launch_uris (GAppInfo           *appinfo,
                                GList              *uris,
                                GAppLaunchContext  *launch_context,
                                GError            **error)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_launch_uris (G_APP_INFO(info->super_appinfo), uris, launch_context, error);
}

static gboolean
gmenu_desktopappinfo_should_show (GAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_should_show (G_APP_INFO(info->super_appinfo));
}

static gboolean
gmenu_desktopappinfo_set_as_default_for_type (GAppInfo    *appinfo,
                                                const char  *content_type,
                                                GError     **error)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_set_as_default_for_type (G_APP_INFO(info->super_appinfo), content_type, error);
}

static gboolean
gmenu_desktopappinfo_set_as_default_for_extension (GAppInfo    *appinfo,
                                                 const char  *extension,
                                                 GError     **error)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_set_as_default_for_extension (G_APP_INFO(info->super_appinfo), extension, error);
}

static gboolean
gmenu_desktopappinfo_add_supports_type (GAppInfo    *appinfo,
                                      const char  *content_type,
                                      GError     **error)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_add_supports_type (G_APP_INFO(info->super_appinfo), content_type, error);
}

static gboolean
gmenu_desktopappinfo_can_remove_supports_type (GAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_can_remove_supports_type (G_APP_INFO(info->super_appinfo));
}

static gboolean
gmenu_desktopappinfo_remove_supports_type (GAppInfo    *appinfo,
                                         const char  *content_type,
                                         GError     **error)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_remove_supports_type (G_APP_INFO(info->super_appinfo), content_type, error);
}

static gboolean
gmenu_desktopappinfo_can_delete (GAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_can_delete (G_APP_INFO(info->super_appinfo));
}

static gboolean
gmenu_desktopappinfo_delete (GAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_delete (G_APP_INFO(info->super_appinfo));
}

static const char *
gmenu_desktopappinfo_get_commandline (GAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_get_commandline (G_APP_INFO(info->super_appinfo));
}

static const char *
gmenu_desktopappinfo_get_display_name (GAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_get_display_name (G_APP_INFO(info->super_appinfo));
}

static gboolean
gmenu_desktopappinfo_set_as_last_used_for_type (GAppInfo    *appinfo,
                                         const char  *content_type,
                                         GError     **error)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_set_as_last_used_for_type (G_APP_INFO(info->super_appinfo), content_type, error);
}

static const char **
gmenu_desktopappinfo_get_supported_types (GAppInfo    *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);

  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (appinfo);
  return g_app_info_get_supported_types (G_APP_INFO(info->super_appinfo));
}

static void
gmenu_desktopappinfo_interface_init (GAppInfoIface *iface)
{
  iface->dup = gmenu_desktopappinfo_dup;
  iface->equal = gmenu_desktopappinfo_equal;
  iface->get_id = gmenu_desktopappinfo_get_id;
  iface->get_name = gmenu_desktopappinfo_get_name;
  iface->get_description = gmenu_desktopappinfo_get_description;
  iface->get_executable = gmenu_desktopappinfo_get_executable;
  iface->get_icon = gmenu_desktopappinfo_get_icon;
  iface->launch = gmenu_desktopappinfo_launch;
  iface->supports_uris = gmenu_desktopappinfo_supports_uris;
  iface->supports_files = gmenu_desktopappinfo_supports_files;
  iface->launch_uris = gmenu_desktopappinfo_launch_uris;
  iface->should_show = gmenu_desktopappinfo_should_show;
  iface->set_as_default_for_type = gmenu_desktopappinfo_set_as_default_for_type;
  iface->set_as_default_for_extension = gmenu_desktopappinfo_set_as_default_for_extension;
  iface->add_supports_type = gmenu_desktopappinfo_add_supports_type;
  iface->can_remove_supports_type = gmenu_desktopappinfo_can_remove_supports_type;
  iface->remove_supports_type = gmenu_desktopappinfo_remove_supports_type;
  iface->can_delete = gmenu_desktopappinfo_can_delete;
  iface->do_delete = gmenu_desktopappinfo_delete;
  iface->get_commandline = gmenu_desktopappinfo_get_commandline;
  iface->get_display_name = gmenu_desktopappinfo_get_display_name;
  iface->set_as_last_used_for_type = gmenu_desktopappinfo_set_as_last_used_for_type;
  iface->get_supported_types = gmenu_desktopappinfo_get_supported_types;
}

G_DEFINE_TYPE_WITH_CODE (GMenuDesktopAppInfo, gmenu_desktopappinfo, G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (G_TYPE_APP_INFO, gmenu_desktopappinfo_interface_init))

static void
gmenu_desktopappinfo_init (GMenuDesktopAppInfo *info)
{
  info->super_appinfo = NULL;
  info->desktop_id = NULL;
  info->flatpak_app_id = NULL;
  info->startup_wm_class = NULL;
}

static void
gmenu_desktopappinfo_finalize (GObject *object)
{
  /* TODO: Add deinitalization code here */
  
  GMenuDesktopAppInfo *info = GMENU_DESKTOPAPPINFO (object);
  
  g_free (info->desktop_id);
  g_free (info->startup_wm_class);
  g_free (info->flatpak_app_id);

  if (info->super_appinfo)
    g_object_unref (info->super_appinfo);
}

static void
gmenu_desktopappinfo_class_init (GMenuDesktopAppInfoClass *klass)
{
  GObjectClass* object_class = G_OBJECT_CLASS (klass);

  object_class->finalize = gmenu_desktopappinfo_finalize;
}

/**
 * gmenu_desktopappinfo_new:
 * @desktop_id: the desktop file id
 *
 * This is currently unused in Cinnamon and does not make sense here
 * because the desktop id as used here is not necessarily unique
 *
 * Returns: (nullable): %NULL
 */
GMenuDesktopAppInfo*
gmenu_desktopappinfo_new (const char *desktop_id)
{
  return NULL;
}

/**
 * gmenu_desktopappinfo_new_from_filename:
 * @filename: (type filename): the path of a desktop file, in the GLib
 *      filename encoding
 *
 * Creates a new #GMenuDesktopAppInfo.
 *
 * Returns: (nullable): a new #GMenuDesktopAppInfo or %NULL on error.
 **/
GMenuDesktopAppInfo*
gmenu_desktopappinfo_new_from_filename (gchar *filename)
{
  GMenuDesktopAppInfo *info = NULL;

  info = g_object_new (GMENU_TYPE_DESKTOPAPPINFO, NULL);
  info->super_appinfo = g_desktop_app_info_new_from_filename (filename);
  
  if (info->super_appinfo)
  {
    update_app_data (info);
    return info;
  } else {
      g_object_unref (info);
      return NULL;
  }
}

/**
 * gmenu_desktopappinfo_new_from_keyfile:
 * @key_file: an opened #GKeyFile
 *
 * Creates a new #GMenuDesktopAppInfo.
 *
 * Returns: (nullable): a new #GMenuDesktopAppInfo or %NULL on error.
 **/
GMenuDesktopAppInfo*
gmenu_desktopappinfo_new_from_keyfile (GKeyFile *keyfile)
{
  GMenuDesktopAppInfo *info = NULL;

  info = g_object_new (GMENU_TYPE_DESKTOPAPPINFO, NULL);
  info->super_appinfo = g_desktop_app_info_new_from_keyfile (keyfile);
  
  if (info->super_appinfo)
  {
    update_app_data (info);
    return info;
  } else {
      g_object_unref (info);
      return NULL;
  }
}

/**
 * gmenu_desktopappinfo_get_filename:
 * @appinfo: a #MenuGDesktopAppInfo
 *
 * When @info was created from a known filename, return it.  In some
 * situations such as the #GMenuDesktopAppInfo returned from
 * gmenu_desktopappinfo_new_from_keyfile(), this function will return %NULL.
 *
 * Returns: (type filename): The full path to the file for @info,
 *     or %NULL if not known.
 * Since: 2.24
 */
const char * gmenu_desktopappinfo_get_filename (GMenuDesktopAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);
  return g_desktop_app_info_get_filename (appinfo->super_appinfo);
}

/**
 * gmenu_desktopappinfo_get_generic_name:
 * @appinfo: a #MenuGDesktopAppInfo
 *
 * Gets the generic name from the destkop file.
 *
 * Returns: The value of the GenericName key
 */
const char * gmenu_desktopappinfo_get_generic_name (GMenuDesktopAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);
  return g_desktop_app_info_get_generic_name (appinfo->super_appinfo);
}

/**
 * gmenu_desktopappinfo_get_categories:
 * @appinfo: a #GMenuDesktopAppInfo
 *
 * Gets the categories from the desktop file.
 *
 * Returns: The unparsed Categories key from the desktop file;
 *     i.e. no attempt is made to split it by ';' or validate it.
 */
const char * gmenu_desktopappinfo_get_categories (GMenuDesktopAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);
  return g_desktop_app_info_get_categories (appinfo->super_appinfo);
}

/**
 * gmenu_desktopappinfo_get_keywords:
 * @appinfo: a #GMenuDesktopAppInfo
 *
 * Gets the keywords from the desktop file.
 *
 * Returns: (transfer none): The value of the Keywords key
 */
const char * const *gmenu_desktopappinfo_get_keywords (GMenuDesktopAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);
  return g_desktop_app_info_get_keywords (appinfo->super_appinfo);
}

/**
 * gmenu_desktopappinfo_get_nodisplay:
 * @appinfo: a #GMenuDesktopAppInfo
 *
 * Gets the value of the NoDisplay key, which helps determine if the
 * application info should be shown in menus. See
 * #G_KEY_FILE_DESKTOP_KEY_NO_DISPLAY and g_app_info_should_show().
 *
 * Returns: The value of the NoDisplay key
 */
gboolean gmenu_desktopappinfo_get_nodisplay (GMenuDesktopAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);
  return g_desktop_app_info_get_nodisplay (appinfo->super_appinfo);
}

/**
 * gmenu_desktopappinfo_get_show_in:
 * @appinfo: a #GMenuDesktopAppInfo
 * @desktop_env: (nullable): a string specifying a desktop name
 *
 * Checks if the application info should be shown in menus that list available
 * applications for a specific name of the desktop, based on the
 * `OnlyShowIn` and `NotShowIn` keys.
 *
 * @desktop_env should typically be given as %NULL, in which case the
 * `XDG_CURRENT_DESKTOP` environment variable is consulted.  If you want
 * to override the default mechanism then you may specify @desktop_env,
 * but this is not recommended.
 *
 * Note that g_app_info_should_show() for @info will include this check (with
 * %NULL for @desktop_env) as well as additional checks.
 *
 * Returns: %TRUE if the @info should be shown in @desktop_env according to the
 * `OnlyShowIn` and `NotShowIn` keys, %FALSE
 * otherwise.
 */
gboolean gmenu_desktopappinfo_get_show_in (GMenuDesktopAppInfo *appinfo, const gchar *desktop_env)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);
  return g_desktop_app_info_get_show_in (appinfo->super_appinfo, desktop_env);
}

/**
 * gmenu_desktopappinfo_get_startup_wm_class:
 * @appinfo: a #GMenuDesktopAppInfo that supports startup notify
 *
 * Retrieves the StartupWMClass field from @info. This represents the
 * WM_CLASS property of the main window of the application, if launched
 * through @info.
 * 
 * Note: The returned value contain the suffix ":flatpak" if @info specifies a flatpak app
 * and if the desktop file has a StartupWMClass
 *
 * Returns: (transfer none): the startup WM class, or %NULL if none is set
 * in the desktop file.
 */
const char * gmenu_desktopappinfo_get_startup_wm_class (GMenuDesktopAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);

  return appinfo->startup_wm_class;
}

/**
 * gmenu_desktopappinfo_get_is_hidden:
 * @appinfo: a #GMenuDesktopAppInfo.
 *
 * A desktop file is hidden if the Hidden key in it is
 * set to True.
 *
 * Returns: %TRUE if hidden, %FALSE otherwise.
 **/
gboolean gmenu_desktopappinfo_get_is_hidden (GMenuDesktopAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);
  return g_desktop_app_info_get_is_hidden (appinfo->super_appinfo);
}

/**
 * gmenu_desktopappinfo_has_key:
 * @appinfo: a #GMenuDesktopAppInfo
 * @key: the key to look up
 *
 * Returns whether @key exists in the "Desktop Entry" group
 * of the keyfile backing @info.
 *
 * Returns: %TRUE if the @key exists
 */
gboolean gmenu_desktopappinfo_has_key (GMenuDesktopAppInfo *appinfo, const char *key)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);
  return g_desktop_app_info_has_key (appinfo->super_appinfo, key);
}

/**
 * gmenu_desktopappinfo_get_string:
 * @appinfo: a #GMenuDesktopAppInfo
 * @key: the key to look up
 *
 * Looks up a string value in the keyfile backing @info.
 *
 * The @key is looked up in the "Desktop Entry" group.
 *
 * Returns: a newly allocated string, or %NULL if the key
 *     is not found
 */
char * gmenu_desktopappinfo_get_string (GMenuDesktopAppInfo *appinfo, const char *key)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);
  return g_desktop_app_info_get_string (appinfo->super_appinfo, key);
}

/**
 * gmenu_desktopappinfo_get_locale_string:
 * @appinfo: a #GMenuDesktopAppInfo
 * @key: the key to look up
 *
 * Looks up a localized string value in the keyfile backing @info
 * translated to the current locale.
 *
 * The @key is looked up in the "Desktop Entry" group.
 *
 * Returns: (nullable): a newly allocated string, or %NULL if the key
 *     is not found
 */
char * gmenu_desktopappinfo_get_locale_string (GMenuDesktopAppInfo *appinfo, const char *key)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);
  return g_desktop_app_info_get_locale_string (appinfo->super_appinfo, key);
}

/**
 * gmenu_desktopappinfo_get_boolean:
 * @appinfo: a #GMenuDesktopAppInfo
 * @key: the key to look up
 *
 * Looks up a boolean value in the keyfile backing @info.
 *
 * The @key is looked up in the "Desktop Entry" group.
 *
 * Returns: the boolean value, or %FALSE if the key
 *     is not found
 */
gboolean gmenu_desktopappinfo_get_boolean (GMenuDesktopAppInfo *appinfo, const char *key)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);
  return g_desktop_app_info_get_boolean (appinfo->super_appinfo, key);
}

/**
 * gmenu_desktopappinfo_list_actions:
 * @appinfo: a #GMenuDesktopAppInfo
 *
 * Returns the list of "additional application actions" supported on the
 * desktop file, as per the desktop file specification.
 *
 * As per the specification, this is the list of actions that are
 * explicitly listed in the "Actions" key of the [Desktop Entry] group.
 *
 * Returns: (array zero-terminated=1) (element-type utf8) (transfer none): a list of strings, always non-%NULL
 **/
const gchar * const * gmenu_desktopappinfo_list_actions (GMenuDesktopAppInfo *appinfo)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);
  return g_desktop_app_info_list_actions (appinfo->super_appinfo);
}

/**
 * gmenu_desktopappinfo_launch_action:
 * @appinfo: a #GMenuDesktopAppInfo
 * @action_name: the name of the action as from
 *   g_desktop_app_info_list_actions()
 * @launch_context: (nullable): a #GAppLaunchContext
 *
 * Activates the named application action.
 *
 * You may only call this function on action names that were
 * returned from g_desktop_app_info_list_actions().
 *
 * Note that if the main entry of the desktop file indicates that the
 * application supports startup notification, and @launch_context is
 * non-%NULL, then startup notification will be used when activating the
 * action (and as such, invocation of the action on the receiving side
 * must signal the end of startup notification when it is completed).
 * This is the expected behaviour of applications declaring additional
 * actions, as per the desktop file specification.
 *
 * As with g_app_info_launch() there is no way to detect failures that
 * occur while using this function.
 */
void gmenu_desktopappinfo_launch_action (GMenuDesktopAppInfo *appinfo, const gchar *action_name, GAppLaunchContext *launch_context)
{
  g_return_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo));
  g_desktop_app_info_launch_action (appinfo->super_appinfo, action_name, launch_context);
}

/**
 * gmenu_desktopappinfo_get_action_name:
 * @appinfo: a #GMenuDesktopAppInfo
 * @action_name: the name of the action as from
 *   gmenu_desktopappinfo_list_actions()
 *
 * Gets the user-visible display name of the "additional application
 * action" specified by @action_name.
 *
 * This corresponds to the "Name" key within the keyfile group for the
 * action.
 *
 * Returns: (transfer full): the locale-specific action name
 *
 * Since: 2.38
 */
gchar * gmenu_desktopappinfo_get_action_name (GMenuDesktopAppInfo *appinfo, const gchar *action_name)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), NULL);
  return g_desktop_app_info_get_action_name (appinfo->super_appinfo, action_name);
}

/**
 * gmenu_desktopappinfo_launch_uris_as_manager:
 * @appinfo: a #GMenuDesktopAppInfo
 * @uris: (element-type utf8): List of URIs
 * @launch_context: (nullable): a #GAppLaunchContext
 * @spawn_flags: #GSpawnFlags, used for each process
 * @user_setup: (scope async) (nullable): a #GSpawnChildSetupFunc, used once
 *     for each process.
 * @user_setup_data: (closure user_setup) (nullable): User data for @user_setup
 * @pid_callback: (scope call) (nullable): Callback for child processes
 * @pid_callback_data: (closure pid_callback) (nullable): User data for @callback
 * @error: return location for a #GError, or %NULL
 *
 * This function performs the equivalent of g_app_info_launch_uris(),
 * but is intended primarily for operating system components that
 * launch applications.  Ordinary applications should use
 * g_app_info_launch_uris().
 *
 * If the application is launched via GSpawn, then @spawn_flags, @user_setup
 * and @user_setup_data are used for the call to g_spawn_async().
 * Additionally, @pid_callback (with @pid_callback_data) will be called to
 * inform about the PID of the created process. See g_spawn_async_with_pipes()
 * for information on certain parameter conditions that can enable an
 * optimized posix_spawn() codepath to be used.
 *
 * If application launching occurs via some other mechanism (eg: D-Bus
 * activation) then @spawn_flags, @user_setup, @user_setup_data,
 * @pid_callback and @pid_callback_data are ignored.
 *
 * Returns: %TRUE on successful launch, %FALSE otherwise.
 */
gboolean gmenu_desktopappinfo_launch_uris_as_manager (GMenuDesktopAppInfo            *appinfo,
                                           GList                      *uris,
                                           GAppLaunchContext          *launch_context,
                                           GSpawnFlags                 spawn_flags,
                                           GSpawnChildSetupFunc        user_setup,
                                           gpointer                    user_setup_data,
                                           GDesktopAppLaunchCallback   pid_callback,
                                           gpointer                    pid_callback_data,
                                           GError                    **error)
{
  g_return_val_if_fail (GMENU_IS_DESKTOPAPPINFO (appinfo), FALSE);
  return g_desktop_app_info_launch_uris_as_manager (appinfo->super_appinfo,
                                                    uris,
                                                    launch_context,
                                                    spawn_flags,
                                                    user_setup,
                                                    user_setup_data,
                                                    pid_callback,
                                                    pid_callback_data,
                                                    error);
}

/**
 * gmenu_desktopappinfo_get_is_flatpak:
 * @appinfo: a #GMenuMenuDesktopAppInfo
 * 
 * Returns: %TRUE if @info specifies a flatpak app, %FALSE otherwise
 */
gboolean gmenu_desktopappinfo_get_is_flatpak (GMenuDesktopAppInfo *appinfo)
{
  return appinfo->is_flatpak;
}

/**
 * gmenu_desktopappinfo_get_flatpak_app_id:
 * @appinfo: a #GMenuMenuDesktopAppInfo
 * 
 * This function looks up the "X-Flatpak" key of the [Desktop Entry] group,
 * which contains the Flatpak App ID
 * 
 * Returns: (nullable): the flatpak app id or %NULL
 */
const char *
gmenu_desktopappinfo_get_flatpak_app_id (GMenuDesktopAppInfo *appinfo)
{
  return appinfo->flatpak_app_id;
}
