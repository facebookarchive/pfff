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

/* $Id: ml_gtkbutton.c 1347 2007-06-20 07:40:34Z guesdon $ */

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
#include "ml_gdkpixbuf.h"
#include "gtk_tags.h"

/* Init all */

CAMLprim value ml_gtkassistant_init(value unit)
{
    /* Since these are declared const, must force gcc to call them! */
    GType t =
#ifdef HASGTK210
        gtk_assistant_get_type () +
#endif
        0;
    return Val_GType(t);
}
/* gtkassistant.h */

#ifdef HASGTK210
#define GtkAssistant_val(val) check_cast(GTK_ASSISTANT,val)
ML_1(gtk_assistant_update_buttons_state, GtkAssistant_val,Unit)
ML_2(gtk_assistant_remove_action_widget, GtkAssistant_val, GtkWidget_val, Unit)
ML_2(gtk_assistant_add_action_widget, GtkAssistant_val, GtkWidget_val, Unit)
ML_2(gtk_assistant_get_page_complete,GtkAssistant_val, GtkWidget_val, Val_bool)
ML_3(gtk_assistant_set_page_complete,GtkAssistant_val, GtkWidget_val, Bool_val, Unit)
ML_2(gtk_assistant_get_page_side_image, GtkAssistant_val, GtkWidget_val, Val_GdkPixbuf)
ML_3(gtk_assistant_set_page_side_image, GtkAssistant_val, GtkWidget_val, GdkPixbuf_val, Unit)
ML_2(gtk_assistant_get_page_header_image, GtkAssistant_val, GtkWidget_val, Val_GdkPixbuf)
ML_3(gtk_assistant_set_page_header_image, GtkAssistant_val, GtkWidget_val, GdkPixbuf_val, Unit)
ML_2(gtk_assistant_get_page_title, GtkAssistant_val, GtkWidget_val, Val_string)
ML_3(gtk_assistant_set_page_title, GtkAssistant_val, GtkWidget_val, String_val, Unit)

ML_2(gtk_assistant_get_page_type, GtkAssistant_val, GtkWidget_val, Val_assistant_page_type)
ML_3(gtk_assistant_set_page_type, GtkAssistant_val, GtkWidget_val, Assistant_page_type_val, Unit)
ML_3(gtk_assistant_insert_page, GtkAssistant_val, GtkWidget_val, Int_val, Val_int)
ML_2(gtk_assistant_get_nth_page, GtkAssistant_val, Int_val, Val_GtkWidget)
ML_1(gtk_assistant_get_n_pages, GtkAssistant_val, Val_int)
ML_1(gtk_assistant_get_current_page, GtkAssistant_val, Val_int)
ML_2(gtk_assistant_set_current_page, GtkAssistant_val, Int_val,Unit)

// Untested code:
static gint ml_g_assistant_page_func(gint current_page,
                                     gpointer user_data) {
  value *clos = user_data;
  CAMLparam0();
  CAMLlocal1(ret);
  ret = callback_exn(*clos, Val_int(current_page));
  if (Is_exception_result(ret)) {
    CAML_EXN_LOG("gtk_assistant_page_function");
  }
  CAMLreturn(ret);

}
CAMLprim value ml_gtk_assistant_set_forward_page_func (value assistant, 
                                                       value clos) {
  value *clos_p = ml_global_root_new (clos);
  
  gtk_assistant_set_forward_page_func((GtkAssistant*)Val_GtkAny(assistant),
                                      ml_g_assistant_page_func, 
                                      clos_p,
                                      ml_global_root_destroy);
  return Val_unit;
}

#else
Unsupported_210(gtk_assistant_update_buttons_state)
Unsupported_210(gtk_assistant_remove_action_widget)
Unsupported_210(gtk_assistant_add_action_widget)
Unsupported_210(gtk_assistant_get_page_complete)
Unsupported_210(gtk_assistant_set_page_complete)
Unsupported_210(gtk_assistant_get_page_side_image)
Unsupported_210(gtk_assistant_set_page_side_image)
Unsupported_210(gtk_assistant_get_page_header_image)
Unsupported_210(gtk_assistant_set_page_header_image)
Unsupported_210(gtk_assistant_get_page_title)
Unsupported_210(gtk_assistant_set_page_title)
Unsupported_210(gtk_assistant_get_page_type)
Unsupported_210(gtk_assistant_set_page_type)
Unsupported_210(gtk_assistant_insert_page)
Unsupported_210(gtk_assistant_get_nth_page)
Unsupported_210(gtk_assistant_get_n_pages)
Unsupported_210(gtk_assistant_get_current_page)
Unsupported_210(gtk_assistant_set_current_page)
Unsupported_210(gtk_assistant_set_forward_page_func)
#endif
