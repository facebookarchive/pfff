/**************************************************************************/
/*                Lablgtk                                                 */
/*                                                                        */
/*    This program is free software; you can redistribute it              */
/*    and/or modify it under the terms of the GNU Library General         */
/*    Public License as published by the Free Software Foundation         */
/*    version 2, with the exception described in file COPYING which       */
/*    comes with the library.                                             */
/*                                                                        */
/*    This program is distributed in the hope that it will be useful,     */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of      */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       */
/*    GNU Library General Public License for more details.                */
/*                                                                        */
/*    You should have received a copy of the GNU Library General          */
/*    Public License along with this program; if not, write to the        */
/*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         */
/*    Boston, MA 02111-1307  USA                                          */
/*                                                                        */
/*                                                                        */
/**************************************************************************/

/* $Id: ml_gtkaction.c 1347 2007-06-20 07:40:34Z guesdon $*/

#include <gtk/gtk.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include "wrappers.h"
#include "ml_glib.h"
#include "ml_gobject.h"
#include "ml_gtk.h"
#include "gtk_tags.h"

CAMLprim value ml_gtkaction_init(value unit)
{
  GType t =
#ifdef HASGTK24
    gtk_action_get_type () +
    gtk_toggle_action_get_type () +
    gtk_radio_action_get_type () +
    gtk_toggle_action_get_type () +
    gtk_action_group_get_type () +
    gtk_ui_manager_get_type () ;
#else
    0;
#endif
  return Val_GType(t);
}

#ifdef HASGTK24

#define gobject_list_of_GSList(l) Val_GSList(l, (value_in) Val_GObject)
#define gobject_list_of_GSList_free(l)  Val_GSList_free (l, (value_in) Val_GObject)
#define gobject_list_of_GList(l)  Val_GList (l, (value_in) Val_GObject)
#define gobject_list_of_GList_free(l)  Val_GList_free (l, (value_in) Val_GObject)


/* gtkaction.h */
#define GtkAction_val(val) check_cast(GTK_ACTION, val)
ML_1 (gtk_action_activate, GtkAction_val, Unit)
ML_2 (gtk_action_connect_proxy, GtkAction_val, GtkWidget_val, Unit)
ML_2 (gtk_action_disconnect_proxy, GtkAction_val, GtkWidget_val, Unit)
ML_1 (gtk_action_get_proxies, GtkAction_val, gobject_list_of_GSList)
ML_1 (gtk_action_connect_accelerator, GtkAction_val, Unit)
ML_1 (gtk_action_disconnect_accelerator, GtkAction_val, Unit)
ML_2 (gtk_action_set_accel_path, GtkAction_val, String_val, Unit)
ML_2 (gtk_action_set_accel_group, GtkAction_val, GtkAccelGroup_val, Unit)
ML_1 (gtk_action_is_sensitive, GtkAction_val, Val_bool)
ML_1 (gtk_action_is_visible, GtkAction_val, Val_bool)
ML_2 (gtk_action_block_activate_from, GtkAction_val, GtkWidget_val, Unit)
ML_2 (gtk_action_unblock_activate_from, GtkAction_val, GtkWidget_val, Unit)

/* gtktoggleaction.h */
#define GtkToggleAction_val(val) check_cast(GTK_TOGGLE_ACTION, val)
ML_1 (gtk_toggle_action_toggled, GtkToggleAction_val, Unit)
ML_2 (gtk_toggle_action_set_active, GtkToggleAction_val, Bool_val, Unit)
ML_1 (gtk_toggle_action_get_active, GtkToggleAction_val, Val_bool)


/* gtkradioaction.h */
#define GtkRadioAction_val(val) check_cast(GTK_RADIO_ACTION, val)
CAMLprim value ml_gtk_radio_action_set_group(value ac, value grp)
{
  GtkRadioAction *grp_ac = Option_val(grp, GtkRadioAction_val, NULL);
  GSList *slist = grp_ac ? gtk_radio_action_get_group(grp_ac) : NULL;
  gtk_radio_action_set_group(GtkRadioAction_val(ac), slist);
  return Val_unit;
}
ML_1 (gtk_radio_action_get_current_value, GtkRadioAction_val, Val_int)


/* gtkactiongroup.h */
#define GtkActionGroup_val(val) check_cast(GTK_ACTION_GROUP, val)
ML_2 (gtk_action_group_get_action, GtkActionGroup_val, String_val, Val_GAnyObject)
ML_1 (gtk_action_group_list_actions, GtkActionGroup_val, gobject_list_of_GList_free)
ML_2 (gtk_action_group_add_action, GtkActionGroup_val, GtkAction_val, Unit)
ML_3 (gtk_action_group_add_action_with_accel, GtkActionGroup_val, GtkAction_val, String_option_val, Unit)
ML_2 (gtk_action_group_remove_action, GtkActionGroup_val, GtkAction_val, Unit)


/* gtkuimanager.h */
#define GtkUIManager_val(val) check_cast(GTK_UI_MANAGER, val)
ML_3 (gtk_ui_manager_insert_action_group, GtkUIManager_val, GtkActionGroup_val, Int_val, Unit)
ML_2 (gtk_ui_manager_remove_action_group, GtkUIManager_val, GtkActionGroup_val, Unit)
ML_1 (gtk_ui_manager_get_action_groups, GtkUIManager_val, gobject_list_of_GList)
ML_1 (gtk_ui_manager_get_accel_group, GtkUIManager_val, Val_GtkAccelGroup)
CAMLprim value ml_gtk_ui_manager_get_widget (value m, value n)
{
  GtkWidget *w = gtk_ui_manager_get_widget (GtkUIManager_val(m), String_val(n));
  if (w == NULL) raise_not_found();
  return Val_GAnyObject(w);
}
CAMLprim value ml_gtk_ui_manager_get_action (value m, value n)
{
  GtkAction *a = gtk_ui_manager_get_action (GtkUIManager_val(m), String_val(n));
  if (a == NULL) raise_not_found();
  return Val_GAnyObject(a);
}
CAMLprim value ml_gtk_ui_manager_add_ui_from_string(value uim, value s)
{
  GError *error = NULL;
  guint id;
  id = gtk_ui_manager_add_ui_from_string(GtkUIManager_val(uim),
					 String_val(s), string_length(s), 
					 &error);
  if (error != NULL) ml_raise_gerror (error);
  return Val_int(id);
}
CAMLprim value ml_gtk_ui_manager_add_ui_from_file(value uim, value s)
{
  GError *error = NULL;
  guint id;
  id = gtk_ui_manager_add_ui_from_file(GtkUIManager_val(uim),
				       String_val(s), &error);
  if (error != NULL) ml_raise_gerror (error);
  return Val_int(id);
}
ML_2 (gtk_ui_manager_remove_ui, GtkUIManager_val, Int_val, Unit)
ML_1 (gtk_ui_manager_ensure_update, GtkUIManager_val, Unit)
ML_1 (gtk_ui_manager_new_merge_id, GtkUIManager_val, Val_int)
ML_7 (gtk_ui_manager_add_ui, GtkUIManager_val, Int_val, String_val, String_val, String_option_val, Ui_manager_item_type_val, Bool_val, Unit)
ML_bc7(ml_gtk_ui_manager_add_ui)
Make_Flags_val(Ui_manager_item_type_val)
ML_2 (gtk_ui_manager_get_toplevels, GtkUIManager_val, Flags_Ui_manager_item_type_val, gobject_list_of_GSList_free)

#else /* HASGTK24 */

Unsupported_24(gtk_action_activate)
Unsupported_24(gtk_action_connect_proxy)
Unsupported_24(gtk_action_disconnect_proxy)
Unsupported_24(gtk_action_get_proxies)
Unsupported_24(gtk_action_connect_accelerator)
Unsupported_24(gtk_action_disconnect_accelerator)
Unsupported_24(gtk_action_set_accel_path)
Unsupported_24(gtk_action_set_accel_group)
Unsupported_24(gtk_action_is_sensitive)
Unsupported_24(gtk_action_is_visible)
Unsupported_24(gtk_action_block_activate_from)
Unsupported_24(gtk_action_unblock_activate_from)
Unsupported_24(gtk_toggle_action_toggled)
Unsupported_24(gtk_toggle_action_set_active)
Unsupported_24(gtk_toggle_action_get_active)
Unsupported_24(gtk_radio_action_set_group)
Unsupported_24(gtk_radio_action_get_current_value)
Unsupported_24(gtk_action_group_get_action)
Unsupported_24(gtk_action_group_list_actions)
Unsupported_24(gtk_action_group_add_action)
Unsupported_24(gtk_action_group_add_action_with_accel)
Unsupported_24(gtk_action_group_remove_action)
Unsupported_24(gtk_ui_manager_insert_action_group)
Unsupported_24(gtk_ui_manager_remove_action_group)
Unsupported_24(gtk_ui_manager_get_action_groups)
Unsupported_24(gtk_ui_manager_get_accel_group)
Unsupported_24(gtk_ui_manager_get_widget)
Unsupported_24(gtk_ui_manager_get_toplevels)
Unsupported_24(gtk_ui_manager_get_action)
Unsupported_24(gtk_ui_manager_add_ui_from_string)
Unsupported_24(gtk_ui_manager_add_ui_from_file)
Unsupported_24(gtk_ui_manager_remove_ui)
Unsupported_24(gtk_ui_manager_ensure_update)
Unsupported_24(gtk_ui_manager_add_ui)
Unsupported_24(gtk_ui_manager_add_ui_bc)
Unsupported_24(gtk_ui_manager_new_merge_id)
Unsupported_24(gtk_ui_manager_new_merge_id_bc)

#endif /* HASGTK24 */
