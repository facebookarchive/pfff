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

/* $Id: ml_gtkbutton.c 1355 2007-08-08 13:17:06Z ben_99_9 $ */

#include <string.h>
#include <gtk/gtk.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include "wrappers.h"
#include "ml_glib.h"
#include "ml_gobject.h"
#include "ml_gdk.h"
#include "ml_gtk.h"
#include "gtk_tags.h"

/* Init all */

CAMLprim value ml_gtkbutton_init(value unit)
{
    /* Since these are declared const, must force gcc to call them! */
    GType t =
        gtk_button_get_type() +
        gtk_check_button_get_type() +
        gtk_toggle_button_get_type() +
        gtk_radio_button_get_type() +
        gtk_toolbar_get_type() +
#ifdef HASGTK24
        gtk_color_button_get_type() +
        gtk_font_button_get_type() +
        gtk_tool_item_get_type() +
        gtk_separator_tool_item_get_type() +
        gtk_tool_button_get_type() +
        gtk_toggle_tool_button_get_type() +
        gtk_radio_tool_button_get_type() +
#endif
#ifdef HASGTK26
        gtk_menu_tool_button_get_type() +
#endif
#ifdef HASGTK210
        gtk_link_button_get_type () +
#endif
#ifdef HASGTK212
        gtk_scale_button_get_type () +
#endif
        0;
    return Val_GType(t);
}
/* gtkbutton.h */

#define GtkButton_val(val) check_cast(GTK_BUTTON,val)
/*
ML_0 (gtk_button_new, Val_GtkWidget_sink)
ML_1 (gtk_butTon_new_with_label, String_val, Val_GtkWidget_sink)
ML_1 (gtk_button_new_with_mnemonic, String_val, Val_GtkWidget_sink)
ML_1 (gtk_button_new_from_stock, String_val, Val_GtkWidget_sink)
*/
ML_1 (gtk_button_pressed, GtkButton_val, Unit)
ML_1 (gtk_button_released, GtkButton_val, Unit)
ML_1 (gtk_button_clicked, GtkButton_val, Unit)
ML_1 (gtk_button_enter, GtkButton_val, Unit)
ML_1 (gtk_button_leave, GtkButton_val, Unit)
/* properties
ML_2 (gtk_button_set_relief, GtkButton_val, Relief_style_val, Unit)
ML_1 (gtk_button_get_relief, GtkButton_val, Val_relief_style)
ML_2 (gtk_button_set_label, GtkButton_val, String_val, Unit)
ML_1 (gtk_button_get_label, GtkButton_val, Val_optstring)
*/

/* gtktogglebutton.h */

#define GtkToggleButton_val(val) check_cast(GTK_TOGGLE_BUTTON,val)
/*
ML_0 (gtk_toggle_button_new, Val_GtkWidget_sink)
ML_1 (gtk_toggle_button_new_with_label, String_val, Val_GtkWidget_sink)
ML_1 (gtk_toggle_button_new_with_mnemonic, String_val, Val_GtkWidget_sink)
ML_2 (gtk_toggle_button_set_mode, GtkToggleButton_val, Bool_val, Unit)
ML_2 (gtk_toggle_button_set_active, GtkToggleButton_val, Bool_val, Unit)
*/
ML_1 (gtk_toggle_button_toggled, GtkToggleButton_val, Unit)

/* gtkcheckbutton.h */
/*
#define GtkCheckButton_val(val) check_cast(GTK_CHECK_BUTTON,val)
ML_0 (gtk_check_button_new, Val_GtkWidget_sink)
ML_1 (gtk_check_button_new_with_label, String_val, Val_GtkWidget_sink)
ML_1 (gtk_check_button_new_with_mnemonic, String_val, Val_GtkWidget_sink)
*/

/* gtkradiobutton.h */
/*
#define GtkRadioButton_val(val) check_cast(GTK_RADIO_BUTTON,val)
static GSList* button_group_val(value val)
{
    return (val == Val_unit ? NULL :
            gtk_radio_button_group(GtkRadioButton_val(Field(val,0))));
}
ML_1 (gtk_radio_button_new, button_group_val,
      Val_GtkWidget_sink)
ML_2 (gtk_radio_button_new_with_label, button_group_val,
      String_val, Val_GtkWidget_sink)
ML_2 (gtk_radio_button_new_with_mnemonic, button_group_val,
      String_val, Val_GtkWidget_sink)
ML_2 (gtk_radio_button_set_group, GtkRadioButton_val, button_group_val, Unit)
*/

/* gtktoolbar.h */

#define GtkToolbar_val(val) check_cast(GTK_TOOLBAR,val)
/* ML_0 (gtk_toolbar_new, Val_GtkWidget_sink) */
ML_2 (gtk_toolbar_insert_space, GtkToolbar_val, Int_val, Unit)
ML_7 (gtk_toolbar_insert_element, GtkToolbar_val, Toolbar_child_val,
      Insert(NULL) Optstring_val, Optstring_val, Optstring_val, GtkWidget_val,
      Insert(NULL) Insert(NULL) Int_val, Val_GtkWidget)
ML_bc7 (ml_gtk_toolbar_insert_element)
ML_5 (gtk_toolbar_insert_widget, GtkToolbar_val, GtkWidget_val,
      Optstring_val, Optstring_val, Int_val, Unit)
/*
ML_2 (gtk_toolbar_set_orientation, GtkToolbar_val, Orientation_val, Unit)
ML_2 (gtk_toolbar_set_style, GtkToolbar_val, Toolbar_style_val, Unit)
ML_2 (gtk_toolbar_set_space_size, GtkToolbar_val, Int_val, Unit)
ML_2 (gtk_toolbar_set_space_style, GtkToolbar_val, Toolbar_space_style_val, Unit)
*/
ML_1 (gtk_toolbar_unset_style, GtkToolbar_val, Unit)
ML_2 (gtk_toolbar_set_tooltips, GtkToolbar_val, Bool_val, Unit)
ML_1 (gtk_toolbar_get_icon_size, GtkToolbar_val, Val_icon_size)
ML_2 (gtk_toolbar_set_icon_size, GtkToolbar_val, Icon_size_val, Unit)
ML_1 (gtk_toolbar_unset_icon_size, GtkToolbar_val, Unit)

/* extended API in GTK 2.4 */
#ifdef HASGTK24
#define GtkToolItem_val(val) check_cast(GTK_TOOL_ITEM,val)
ML_2 (gtk_tool_item_set_homogeneous, GtkToolItem_val, Bool_val, Unit)
ML_1 (gtk_tool_item_get_homogeneous, GtkToolItem_val, Val_bool)
ML_2 (gtk_tool_item_set_expand, GtkToolItem_val, Bool_val, Unit)
ML_1 (gtk_tool_item_get_expand, GtkToolItem_val, Val_bool)
ML_2 (gtk_tool_item_set_use_drag_window, GtkToolItem_val, Bool_val, Unit)
ML_1 (gtk_tool_item_get_use_drag_window, GtkToolItem_val, Val_bool)
ML_4 (gtk_tool_item_set_tooltip, GtkToolItem_val, GtkTooltips_val, String_val, String_val, Unit)

#define GtkToggleToolButton_val(val) check_cast(GTK_TOGGLE_TOOL_BUTTON,val)
ML_2 (gtk_toggle_tool_button_set_active, GtkToggleToolButton_val, Bool_val, Unit)
ML_1 (gtk_toggle_tool_button_get_active, GtkToggleToolButton_val, Val_bool)

ML_3 (gtk_toolbar_insert, GtkToolbar_val, GtkToolItem_val, Int_val, Unit)
ML_2 (gtk_toolbar_get_item_index, GtkToolbar_val, GtkToolItem_val, Val_int)
ML_1 (gtk_toolbar_get_n_items, GtkToolbar_val, Val_int)
ML_2 (gtk_toolbar_get_nth_item, GtkToolbar_val, Int_val, Val_GtkWidget)
ML_3 (gtk_toolbar_get_drop_index, GtkToolbar_val, Int_val, Int_val, Val_int)
#define OptGtkToolItem_val(i) Option_val(i,GtkToolItem_val,NULL)
ML_3 (gtk_toolbar_set_drop_highlight_item, GtkToolbar_val, OptGtkToolItem_val, Int_val, Unit)
ML_1 (gtk_toolbar_get_tooltips, GtkToolbar_val, Val_bool)
ML_1 (gtk_toolbar_get_relief_style, GtkToolbar_val, Val_relief_style)

#else
Unsupported_24(gtk_tool_item_set_homogeneous)
Unsupported_24(gtk_tool_item_get_homogeneous)
Unsupported_24(gtk_tool_item_set_expand)
Unsupported_24(gtk_tool_item_get_expand)
Unsupported_24(gtk_tool_item_set_use_drag_window)
Unsupported_24(gtk_tool_item_get_use_drag_window)
Unsupported_24(gtk_tool_item_set_tooltip)
Unsupported_24(gtk_toggle_tool_button_set_active)
Unsupported_24(gtk_toggle_tool_button_get_active)
Unsupported_24(gtk_toolbar_insert)
Unsupported_24(gtk_toolbar_get_item_index)
Unsupported_24(gtk_toolbar_get_n_items)
Unsupported_24(gtk_toolbar_get_nth_item)
Unsupported_24(gtk_toolbar_get_drop_index)
Unsupported_24(gtk_toolbar_set_drop_highlight_item)
Unsupported_24(gtk_toolbar_get_tooltips)
Unsupported_24(gtk_toolbar_get_relief_style)
#endif /* HASGTK24 */

#ifdef HASGTK26
#define GtkMenuToolButton_val(val) check_cast(GTK_MENU_TOOL_BUTTON,val)
ML_4 (gtk_menu_tool_button_set_arrow_tooltip, GtkMenuToolButton_val, GtkTooltips_val, String_val, String_val, Unit)
#else
Unsupported_26(gtk_menu_tool_button_set_arrow_tooltip)
#endif /* HASGTK26 */

/* gtklinkbutton.h */ 
#ifdef HASGTK210
ML_1(gtk_link_button_new, String_val, Val_GtkWidget_sink)
ML_2(gtk_link_button_new_with_label, String_val, String_val, Val_GtkWidget_sink)
static void ml_g_link_button_func(GtkLinkButton *button,
                                  const gchar *link,
                                  gpointer user_data) {
  value *clos = user_data;
  CAMLparam0();
  CAMLlocal2(ml_link,ret);
  ml_link = Val_string(link);
  ret = callback2_exn(*clos, Val_GtkWidget(button),ml_link);
  if (Is_exception_result(ret)) {
    CAML_EXN_LOG("gtk_link_button_func");
  }
  CAMLreturn0;
}

CAMLprim value ml_gtk_link_button_set_uri_hook (value clos) {
  value *clos_p = ml_global_root_new (clos);
  
  gtk_link_button_set_uri_hook
    (ml_g_link_button_func, 
     clos_p,
     ml_global_root_destroy);

  return Val_unit;
}
#else
Unsupported_210(gtk_link_button_set_uri_hook)
Unsupported_210(gtk_link_button_new)
Unsupported_210(gtk_link_button_new_with_label)
#endif
