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

/* $Id: ml_gtkmenu.c 1402 2008-03-25 08:55:03Z garrigue $ */

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

CAMLprim value ml_gtkmenu_init(value unit)
{
    /* Since these are declared const, must force gcc to call them! */
    GType t =
        gtk_menu_item_get_type() +
        gtk_image_menu_item_get_type() +
        gtk_check_menu_item_get_type() +
        gtk_radio_menu_item_get_type() +
        gtk_option_menu_get_type() +
        gtk_menu_bar_get_type() +
        gtk_menu_get_type();
    return Val_GType(t);
}

/* gtkmenuitem.h */

#define GtkMenuItem_val(val) check_cast(GTK_MENU_ITEM,val)
ML_0 (gtk_menu_item_new, Val_GtkWidget_sink)
ML_0 (gtk_separator_menu_item_new, Val_GtkWidget_sink)
ML_0 (gtk_tearoff_menu_item_new, Val_GtkWidget_sink)
ML_1 (gtk_menu_item_new_with_label, String_val, Val_GtkWidget_sink)
ML_1 (gtk_menu_item_new_with_mnemonic, String_val, Val_GtkWidget_sink)
ML_2 (gtk_menu_item_set_submenu, GtkMenuItem_val, GtkWidget_val, Unit)

CAMLprim value ml_gtk_menu_item_get_submenu(value sm)
{
  CAMLparam1(sm);
  CAMLlocal1(res);
  res = Val_option(gtk_menu_item_get_submenu(GtkMenuItem_val(sm)),
		   Val_GtkWidget);
  CAMLreturn(res);
}

ML_1 (gtk_menu_item_remove_submenu, GtkMenuItem_val, Unit)
ML_2 (gtk_menu_item_set_accel_path, GtkMenuItem_val, String_val, Unit)
ML_1 (gtk_menu_item_activate, GtkMenuItem_val, Unit)
ML_1 (gtk_menu_item_select, GtkMenuItem_val, Unit)
ML_1 (gtk_menu_item_deselect, GtkMenuItem_val, Unit)
ML_2 (gtk_menu_item_set_right_justified, GtkMenuItem_val, Bool_val, Unit)
ML_1 (gtk_menu_item_get_right_justified, GtkMenuItem_val, Val_bool)

CAMLprim value ml_gtk_menu_item_toggle_size_request(value sm,value i)
{
  CAMLparam2(sm,i);
  CAMLlocal1(res);
  int j;
  j = Int_val(i);
  gtk_menu_item_toggle_size_request(GtkMenuItem_val(sm),&j);
  CAMLreturn( Val_unit );
}

ML_2 (gtk_menu_item_toggle_size_allocate, GtkMenuItem_val, Int_val, Unit)

/* gtkimagemenuitem.h */
#define GtkImageMenuItem_val(val) check_cast(GTK_IMAGE_MENU_ITEM,val)
ML_0 (gtk_image_menu_item_new, Val_GtkWidget_sink)
ML_1 (gtk_image_menu_item_new_with_label, String_val, Val_GtkWidget_sink)
ML_1 (gtk_image_menu_item_new_with_mnemonic, String_val, Val_GtkWidget_sink)

ML_2 (gtk_image_menu_item_new_from_stock, String_val, Option_val(arg2,GtkAccelGroup_val,NULL) Ignore, Val_GtkWidget_sink)
ML_2 (gtk_image_menu_item_set_image, GtkImageMenuItem_val, GtkWidget_val, Unit)
ML_1 (gtk_image_menu_item_get_image, GtkImageMenuItem_val, Val_GtkWidget)


/* gtkcheckmenuitem.h */

#define GtkCheckMenuItem_val(val) check_cast(GTK_CHECK_MENU_ITEM,val)
ML_0 (gtk_check_menu_item_new, Val_GtkWidget_sink)
ML_1 (gtk_check_menu_item_new_with_label, String_val, Val_GtkWidget_sink)
ML_1 (gtk_check_menu_item_new_with_mnemonic, String_val, Val_GtkWidget_sink)

ML_2 (gtk_check_menu_item_set_active, GtkCheckMenuItem_val, Bool_val, Unit)
ML_2 (gtk_check_menu_item_set_inconsistent, GtkCheckMenuItem_val, Bool_val, Unit)
ML_1 (gtk_check_menu_item_get_inconsistent, GtkCheckMenuItem_val, Val_bool)
ML_2 (gtk_check_menu_item_set_show_toggle, GtkCheckMenuItem_val,
      Bool_val, Unit)
ML_1 (gtk_check_menu_item_toggled, GtkCheckMenuItem_val, Unit)
Make_Extractor (gtk_check_menu_item_get, GtkCheckMenuItem_val,
		active, Val_bool)

/* gtkradiomenuitem.h */

#define GtkRadioMenuItem_val(val) check_cast(GTK_RADIO_MENU_ITEM,val)
static GSList* item_group_val(value val)
{
    return (val == Val_unit ? NULL :
            gtk_radio_menu_item_group(GtkRadioMenuItem_val(Field(val,0))));
}
ML_1 (gtk_radio_menu_item_new, item_group_val, Val_GtkWidget_sink)
ML_2 (gtk_radio_menu_item_new_with_label, item_group_val,
      String_val, Val_GtkWidget_sink)
ML_2 (gtk_radio_menu_item_new_with_mnemonic, item_group_val,
      String_val, Val_GtkWidget_sink)
ML_2 (gtk_radio_menu_item_set_group, GtkRadioMenuItem_val,
      item_group_val, Unit)

/* gtkoptionmenu.h */

#define GtkOptionMenu_val(val) check_cast(GTK_OPTION_MENU,val)
/*
ML_0 (gtk_option_menu_new, Val_GtkWidget_sink)
ML_1 (gtk_option_menu_get_menu, GtkOptionMenu_val, Val_GtkWidget_sink)
ML_2 (gtk_option_menu_set_menu, GtkOptionMenu_val, GtkWidget_val, Unit)
*/
ML_1 (gtk_option_menu_remove_menu, GtkOptionMenu_val, Unit)
ML_2 (gtk_option_menu_set_history, GtkOptionMenu_val, Int_val, Unit)

/* gtkmenushell.h */

#define GtkMenuShell_val(val) check_cast(GTK_MENU_SHELL,val)
ML_2 (gtk_menu_shell_append, GtkMenuShell_val, GtkWidget_val, Unit)
ML_2 (gtk_menu_shell_prepend, GtkMenuShell_val, GtkWidget_val, Unit)
ML_3 (gtk_menu_shell_insert, GtkMenuShell_val, GtkWidget_val, Int_val, Unit)
ML_1 (gtk_menu_shell_deactivate, GtkMenuShell_val, Unit)

/* gtkmenu.h */

#define GtkMenu_val(val) check_cast(GTK_MENU,val)
ML_0 (gtk_menu_new, Val_GtkWidget_sink)
ML_5 (gtk_menu_popup, GtkMenu_val, GtkWidget_val, GtkWidget_val,
      Insert(NULL) Insert(NULL) Int_val, Int32_val, Unit)
static void menu_popup_cb(GtkMenu *menu, gint *x, gint *y,
                          gboolean *push_in, gpointer clos)
{
    value res =
        caml_callback3(*(value*)clos, Val_int(*x), Val_int(*y),
                       Val_bool(*push_in));
    *x = Int_val(Field(res,0));
    *y = Int_val(Field(res,1));
    *push_in = Int_val(Field(res,2));
    caml_remove_global_root(clos);
    stat_free(clos);
}
CAMLprim value ml_gtk_menu_popup_at (value menu, value button,
                                     value time, value func)
{
    value *clos = stat_alloc(sizeof(value));
    *clos = func;
    caml_register_global_root(clos);
    gtk_menu_popup(GtkMenu_val(menu), NULL, NULL, &menu_popup_cb, clos,
                   Option_val(button,Int_val,0), Option_val(time,Int32_val,0));
    return Val_unit;
}   
ML_1 (gtk_menu_popdown, GtkMenu_val, Unit)
ML_1 (gtk_menu_get_active, GtkMenu_val, Val_GtkWidget)
ML_2 (gtk_menu_set_active, GtkMenu_val, Int_val, Unit)
ML_2 (gtk_menu_set_accel_group, GtkMenu_val, GtkAccelGroup_val, Unit)
ML_1 (gtk_menu_get_accel_group, GtkMenu_val, Val_GtkAccelGroup)
ML_2 (gtk_menu_set_accel_path, GtkMenu_val, String_val, Unit)
CAMLprim value ml_gtk_menu_attach_to_widget (value menu, value widget)
{
    gtk_menu_attach_to_widget (GtkMenu_val(menu), GtkWidget_val(widget), NULL);
    return Val_unit;
}
ML_1 (gtk_menu_get_attach_widget, GtkMenu_val, Val_GtkWidget)
ML_1 (gtk_menu_detach, GtkMenu_val, Unit)

/* gtkmenubar.h */
/*
#define GtkMenuBar_val(val) check_cast(GTK_MENU_BAR,val)
ML_0 (gtk_menu_bar_new, Val_GtkWidget_sink)
*/
