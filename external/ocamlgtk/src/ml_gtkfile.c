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

/* $Id: ml_gtkfile.c 1356 2007-08-08 18:05:11Z ben_99_9 $ */

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


CAMLprim value ml_gtkfile_init(value unit)
{
#ifdef HASGTK24
  GType t =
#ifdef HASGTK26
    gtk_file_chooser_button_get_type () +
#endif
    gtk_file_chooser_dialog_get_type () +
    gtk_file_chooser_widget_get_type ();

  ml_register_exn_map (GTK_FILE_CHOOSER_ERROR, 
		       "gtk_file_chooser_error");
  return Val_GType(t);
#else
  return Val_unit;
#endif
}

#ifdef HASGTK24
#define GtkFileChooser_val(val) check_cast(GTK_FILE_CHOOSER,val)

static value some_string_and_free(gchar *s)
{
  value v = s ? ml_some(copy_string(s)) : Val_unit;
  g_free(s);
  return v;
}
#define string_list_of_GSList(l) Val_GSList_free(l, (value_in) copy_string_g_free)
#define widget_list_of_GSList(l) Val_GSList_free(l, (value_in) Val_GObject)

ML_2 (gtk_file_chooser_set_current_name, GtkFileChooser_val, String_val, Unit)
ML_1 (gtk_file_chooser_get_filename, GtkFileChooser_val, some_string_and_free)
ML_2 (gtk_file_chooser_set_filename, GtkFileChooser_val, String_val, Val_bool)
ML_2 (gtk_file_chooser_select_filename, GtkFileChooser_val, String_val, Val_bool)
ML_2 (gtk_file_chooser_unselect_filename, GtkFileChooser_val, String_val, Unit)
ML_1 (gtk_file_chooser_select_all, GtkFileChooser_val, Unit)
ML_1 (gtk_file_chooser_unselect_all, GtkFileChooser_val, Unit)
ML_1 (gtk_file_chooser_get_filenames, GtkFileChooser_val, string_list_of_GSList)
ML_2 (gtk_file_chooser_set_current_folder, GtkFileChooser_val, String_val, Val_bool)
ML_1 (gtk_file_chooser_get_current_folder, GtkFileChooser_val, some_string_and_free)

ML_1 (gtk_file_chooser_get_uri, GtkFileChooser_val, some_string_and_free)
ML_2 (gtk_file_chooser_set_uri, GtkFileChooser_val, String_val, Val_bool)
ML_2 (gtk_file_chooser_select_uri, GtkFileChooser_val, String_val, Val_bool)
ML_2 (gtk_file_chooser_unselect_uri, GtkFileChooser_val, String_val, Unit)
ML_1 (gtk_file_chooser_get_uris, GtkFileChooser_val, string_list_of_GSList)
ML_2 (gtk_file_chooser_set_current_folder_uri, GtkFileChooser_val, String_val, Val_bool)
ML_1 (gtk_file_chooser_get_current_folder_uri, GtkFileChooser_val, copy_string_g_free)

ML_1 (gtk_file_chooser_get_preview_filename, GtkFileChooser_val, some_string_and_free)
ML_1 (gtk_file_chooser_get_preview_uri, GtkFileChooser_val, some_string_and_free)

#define GtkFileFilter_val(val)  check_cast(GTK_FILE_FILTER, val)
ML_0 (gtk_file_filter_new, Val_GtkAny_sink)
ML_2 (gtk_file_filter_set_name, GtkFileFilter_val, String_val, Unit)
ML_1 (gtk_file_filter_get_name, GtkFileFilter_val, copy_string_or_null);
ML_2 (gtk_file_filter_add_mime_type, GtkFileFilter_val, String_val, Unit)
ML_2 (gtk_file_filter_add_pattern, GtkFileFilter_val, String_val, Unit)
static gboolean ml_gtk_file_filter_func (const GtkFileFilterInfo *filter_info, 
					 gpointer data)
{
  value *cb = data;
  CAMLparam0();
  CAMLlocal5(r, l, v, t, s);
  l = Val_emptylist;
#define CONS_MEMBER(memb, flag) \
  if (filter_info->contains & GTK_FILE_FILTER_##flag) {	\
    s = copy_string (filter_info->memb);	\
    v = alloc_small(2, 0);			\
    Field(v, 0) = MLTAG_##flag;			\
    Field(v, 1) = s;				\
    l = ml_cons (v, l);				\
  }
  CONS_MEMBER (mime_type, MIME_TYPE)
  CONS_MEMBER (display_name, DISPLAY_NAME)
  CONS_MEMBER (uri, URI)
  CONS_MEMBER (filename, FILENAME)
#undef CONS_MEMBER
  r = callback_exn (*cb, l);
  if (Is_exception_result (r)) CAMLreturn(TRUE);
  CAMLreturn (Bool_val(r));
}
Make_Flags_val(File_filter_flags_val)
CAMLprim value ml_gtk_file_filter_add_custom(value obj, value needed, value cb)
{
  value *clos = ml_global_root_new(cb);
  gtk_file_filter_add_custom (GtkFileFilter_val(obj), Flags_File_filter_flags_val(needed),
			      ml_gtk_file_filter_func, clos, ml_global_root_destroy);
  return Val_unit;
}


ML_2 (gtk_file_chooser_add_filter, GtkFileChooser_val, GtkFileFilter_val, Unit)
ML_2 (gtk_file_chooser_remove_filter, GtkFileChooser_val, GtkFileFilter_val, Unit)
ML_1 (gtk_file_chooser_list_filters, GtkFileChooser_val, widget_list_of_GSList)

CAMLprim value ml_gtk_file_chooser_add_shortcut_folder(value w, value f)
{
  GError *err = NULL;
  gtk_file_chooser_add_shortcut_folder(GtkFileChooser_val(w), 
				       String_val(f), &err);
  if (err) ml_raise_gerror(err);
  return Val_unit;
}
CAMLprim value ml_gtk_file_chooser_remove_shortcut_folder(value w, value f)
{
  GError *err = NULL;
  gtk_file_chooser_remove_shortcut_folder(GtkFileChooser_val(w), 
				       String_val(f), &err);
  if (err) ml_raise_gerror(err);
  return Val_unit;
}
ML_1 (gtk_file_chooser_list_shortcut_folders, GtkFileChooser_val, string_list_of_GSList)
CAMLprim value ml_gtk_file_chooser_add_shortcut_folder_uri(value w, value f)
{
  GError *err = NULL;
  gtk_file_chooser_add_shortcut_folder_uri(GtkFileChooser_val(w), 
					   String_val(f), &err);
  if (err) ml_raise_gerror(err);
  return Val_unit;
}
CAMLprim value ml_gtk_file_chooser_remove_shortcut_folder_uri(value w, value f)
{
  GError *err = NULL;
  gtk_file_chooser_remove_shortcut_folder_uri(GtkFileChooser_val(w), 
					      String_val(f), &err);
  if (err) ml_raise_gerror(err);
  return Val_unit;
}
ML_1 (gtk_file_chooser_list_shortcut_folder_uris, GtkFileChooser_val, string_list_of_GSList)

#else /* HASGTK24 */

Unsupported_24(gtk_file_chooser_set_current_name)
Unsupported_24(gtk_file_chooser_get_filename)
Unsupported_24(gtk_file_chooser_set_filename)
Unsupported_24(gtk_file_chooser_select_filename)
Unsupported_24(gtk_file_chooser_unselect_filename)
Unsupported_24(gtk_file_chooser_select_all)
Unsupported_24(gtk_file_chooser_unselect_all)
Unsupported_24(gtk_file_chooser_get_filenames)
Unsupported_24(gtk_file_chooser_set_current_folder)
Unsupported_24(gtk_file_chooser_get_current_folder)
Unsupported_24(gtk_file_chooser_get_uri)
Unsupported_24(gtk_file_chooser_set_uri)
Unsupported_24(gtk_file_chooser_select_uri)
Unsupported_24(gtk_file_chooser_unselect_uri)
Unsupported_24(gtk_file_chooser_get_uris)
Unsupported_24(gtk_file_chooser_set_current_folder_uri)
Unsupported_24(gtk_file_chooser_get_current_folder_uri)
Unsupported_24(gtk_file_chooser_get_preview_filename)
Unsupported_24(gtk_file_chooser_get_preview_uri)
Unsupported_24(gtk_file_filter_new)
Unsupported_24(gtk_file_filter_set_name)
Unsupported_24(gtk_file_filter_get_name)
Unsupported_24(gtk_file_filter_add_mime_type)
Unsupported_24(gtk_file_filter_add_pattern)
Unsupported_24(gtk_file_filter_add_custom)
Unsupported_24(gtk_file_chooser_add_filter)
Unsupported_24(gtk_file_chooser_remove_filter)
Unsupported_24(gtk_file_chooser_list_filters)
Unsupported_24(gtk_file_chooser_list_shortcut_folders)
Unsupported_24(gtk_file_chooser_add_shortcut_folder)
Unsupported_24(gtk_file_chooser_remove_shortcut_folder)
Unsupported_24(gtk_file_chooser_add_shortcut_folder_uri)
Unsupported_24(gtk_file_chooser_remove_shortcut_folder_uri)
Unsupported_24(gtk_file_chooser_list_shortcut_folder_uris)

#endif /* HASGTK24 */

