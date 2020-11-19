/*
 * gmenu-desktopappinfo.h
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


#ifndef _GMENU_DESKTOPAPPINFO_H_
#define _GMENU_DESKTOPAPPINFO_H_

#ifndef GMENU_I_KNOW_THIS_IS_UNSTABLE
#error "libgnome-menu should only be used if you understand that it's subject to frequent change, and is not supported as a fixed API/ABI or as part of the platform"
#endif

#include <gio/gdesktopappinfo.h>

G_BEGIN_DECLS

#define GMENU_TYPE_DESKTOPAPPINFO             (gmenu_desktopappinfo_get_type ())
#define GMENU_DESKTOPAPPINFO(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), GMENU_TYPE_DESKTOPAPPINFO, GMenuDesktopAppInfo))
#define GMENU_DESKTOPAPPINFO_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), GMENU_TYPE_DESKTOPAPPINFO, GMenuDesktopAppInfoClass))
#define GMENU_IS_DESKTOPAPPINFO(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GMENU_TYPE_DESKTOPAPPINFO))
#define GMENU_IS_DESKTOPAPPINFO_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), GMENU_TYPE_DESKTOPAPPINFO))
#define GMENU_DESKTOPAPPINFO_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), GMENU_TYPE_DESKTOPAPPINFO, GMenuDesktopAppInfoClass))

typedef struct _GMenuDesktopAppInfoClass GMenuDesktopAppInfoClass;
typedef struct _GMenuDesktopAppInfo GMenuDesktopAppInfo;

struct _GMenuDesktopAppInfoClass
{
  GObjectClass parent_class;
};

#define GMENU_DESKTOPAPPINFO_FLATPAK_SUFFIX ":flatpak"

GType gmenu_desktopappinfo_get_type (void) G_GNUC_CONST;

GMenuDesktopAppInfo* gmenu_desktopappinfo_new_from_filename (gchar *filename);
GMenuDesktopAppInfo *gmenu_desktopappinfo_new_from_keyfile  (GKeyFile *key_file);

const char * gmenu_desktopappinfo_get_filename (GMenuDesktopAppInfo *appinfo);
const char * gmenu_desktopappinfo_get_generic_name (GMenuDesktopAppInfo *appinfo);
const char * gmenu_desktopappinfo_get_categories (GMenuDesktopAppInfo *appinfo);
const char * const *gmenu_desktopappinfo_get_keywords (GMenuDesktopAppInfo *appinfo);
gboolean gmenu_desktopappinfo_get_nodisplay (GMenuDesktopAppInfo *appinfo);
gboolean gmenu_desktopappinfo_get_show_in (GMenuDesktopAppInfo *appinfo, const gchar *desktop_env);
const char * gmenu_desktopappinfo_get_startup_wm_class (GMenuDesktopAppInfo *appinfo);
GMenuDesktopAppInfo *gmenu_desktopappinfo_new (const char *desktop_id);
gboolean gmenu_desktopappinfo_get_is_hidden (GMenuDesktopAppInfo *appinfo);
gboolean gmenu_desktopappinfo_has_key (GMenuDesktopAppInfo *appinfo, const char *key);
char * gmenu_desktopappinfo_get_string (GMenuDesktopAppInfo *appinfo, const char *key);
char * gmenu_desktopappinfo_get_locale_string (GMenuDesktopAppInfo *appinfo, const char *key);
gboolean gmenu_desktopappinfo_get_boolean (GMenuDesktopAppInfo *appinfo, const char *key);
const gchar * const * gmenu_desktopappinfo_list_actions (GMenuDesktopAppInfo *appinfo);
void gmenu_desktopappinfo_launch_action (GMenuDesktopAppInfo *appinfo, const gchar *action_name, GAppLaunchContext *launch_context);
gchar * gmenu_desktopappinfo_get_action_name (GMenuDesktopAppInfo *appinfo, const gchar *action_name);
gboolean gmenu_desktopappinfo_launch_uris_as_manager (GMenuDesktopAppInfo            *appinfo,
                                           GList                      *uris,
                                           GAppLaunchContext          *launch_context,
                                           GSpawnFlags                 spawn_flags,
                                           GSpawnChildSetupFunc        user_setup,
                                           gpointer                    user_setup_data,
                                           GDesktopAppLaunchCallback   pid_callback,
                                           gpointer                    pid_callback_data,
                                           GError                    **error);

gboolean gmenu_desktopappinfo_get_is_flatpak (GMenuDesktopAppInfo *appinfo);
const char * gmenu_desktopappinfo_get_flatpak_app_id (GMenuDesktopAppInfo *appinfo);

G_END_DECLS

#endif /* _GMENU_DESKTOPAPPINFO_H_ */

