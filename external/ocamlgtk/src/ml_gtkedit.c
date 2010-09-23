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

/* $Id: ml_gtkedit.c 1454 2009-05-12 10:19:38Z garrigue $ */

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
#include "ml_gtktree.h"

/* Init all */

CAMLprim value ml_gtkedit_init(value unit)
{
    /* Since these are declared const, must force gcc to call them! */
    GType t =
        gtk_spin_button_get_type() +
        gtk_combo_get_type() +
#ifdef HASGTK24
        gtk_combo_box_get_type() +
        gtk_combo_box_entry_get_type() +
        gtk_entry_completion_get_type();
#else
        0;
#endif
    return Val_GType(t);
}

/* gtkeditable.h */

#define GtkEditable_val(val) check_cast(GTK_EDITABLE,val)
ML_3 (gtk_editable_select_region, GtkEditable_val, Int_val, Int_val, Unit)
CAMLprim value ml_gtk_editable_get_selection_bounds(value w)
{
    int start, end;
    CAMLparam1(w);
    CAMLlocal1(tmp);
    value res = Val_unit;

    if (gtk_editable_get_selection_bounds(GtkEditable_val(w), &start, &end)) {
        tmp = alloc_small(2,0);
        Field(tmp,0) = Val_int(start);
        Field(tmp,1) = Val_int(end);
        res = alloc_small(1,0);
        Field(res,0) = tmp;
    }
    CAMLreturn(res);
}
CAMLprim value ml_gtk_editable_insert_text (value w, value s, value pos)
{
    int position = Int_val(pos);
    gtk_editable_insert_text (GtkEditable_val(w), String_val(s),
			      string_length(s), &position);
    return Val_int(position);
}
ML_3 (gtk_editable_delete_text, GtkEditable_val, Int_val, Int_val, Unit)
ML_3 (gtk_editable_get_chars, GtkEditable_val, Int_val, Int_val,
      copy_string_g_free)
ML_1 (gtk_editable_cut_clipboard, GtkEditable_val, Unit)
ML_1 (gtk_editable_copy_clipboard, GtkEditable_val, Unit)
ML_1 (gtk_editable_paste_clipboard, GtkEditable_val, Unit)
ML_1 (gtk_editable_delete_selection, GtkEditable_val, Unit)
ML_2 (gtk_editable_set_position, GtkEditable_val, Int_val, Unit)
ML_1 (gtk_editable_get_position, GtkEditable_val, Val_int)
ML_2 (gtk_editable_set_editable, GtkEditable_val, Bool_val, Unit)
ML_1 (gtk_editable_get_editable, GtkEditable_val, Val_bool)

/* gtkentry.h */

#define GtkEntry_val(val) check_cast(GTK_ENTRY,val)
ML_2 (gtk_entry_append_text, GtkEntry_val, String_val, Unit)
ML_2 (gtk_entry_prepend_text, GtkEntry_val, String_val, Unit)
Make_Extractor (gtk_entry, GtkEntry_val, text_length, Val_int)
/*
ML_0 (gtk_entry_new, Val_GtkWidget_sink)
ML_1 (gtk_entry_new_with_max_length, (gint16)Long_val, Val_GtkWidget_sink)
ML_2 (gtk_entry_set_text, GtkEntry_val, String_val, Unit)
ML_1 (gtk_entry_get_text, GtkEntry_val, Val_string)
ML_3 (gtk_entry_select_region, GtkEntry_val, Int_val, Int_val, Unit)
ML_2 (gtk_entry_set_visibility, GtkEntry_val, Bool_val, Unit)
ML_2 (gtk_entry_set_max_length, GtkEntry_val, (gint16)Long_val, Unit)
*/

/* gtkspinbutton.h */

#define GtkSpinButton_val(val) check_cast(GTK_SPIN_BUTTON,val)
/*
ML_3 (gtk_spin_button_new, GtkAdjustment_val,
      Float_val, Int_val, Val_GtkWidget_sink)
ML_2 (gtk_spin_button_set_adjustment, GtkSpinButton_val, GtkAdjustment_val,
      Unit)
ML_1 (gtk_spin_button_get_adjustment, GtkSpinButton_val, Val_GtkAny)
ML_2 (gtk_spin_button_set_digits, GtkSpinButton_val, Int_val, Unit)
ML_1 (gtk_spin_button_get_value_as_float, GtkSpinButton_val, copy_double)
ML_2 (gtk_spin_button_set_value, GtkSpinButton_val, Float_val, Unit)
ML_2 (gtk_spin_button_set_update_policy, GtkSpinButton_val,
      Spin_button_update_policy_val, Unit)
ML_2 (gtk_spin_button_set_numeric, GtkSpinButton_val, Bool_val, Unit)
ML_2 (gtk_spin_button_set_wrap, GtkSpinButton_val, Bool_val, Unit)
ML_2 (gtk_spin_button_set_snap_to_ticks, GtkSpinButton_val, Bool_val, Unit)
ML_4 (gtk_spin_button_configure, GtkSpinButton_val, GtkAdjustment_val,
      Float_val, Int_val, Unit)
*/
ML_2 (gtk_spin_button_spin, GtkSpinButton_val,
      Insert (Is_long(arg2) ? Spin_type_val(arg2) : GTK_SPIN_USER_DEFINED)
      (Is_long(arg2) ? 0.0 : Float_val(Field(arg2,1))) Ignore, Unit)
ML_1 (gtk_spin_button_update, GtkSpinButton_val, Unit)

/* gtktext.h */
/*
#define GtkText_val(val) check_cast(GTK_TEXT,val)
ML_2 (gtk_text_new, GtkAdjustment_val, GtkAdjustment_val, Val_GtkWidget_sink)
ML_2 (gtk_text_set_word_wrap, GtkText_val, Bool_val, Unit)
ML_2 (gtk_text_set_line_wrap, GtkText_val, Bool_val, Unit)
ML_3 (gtk_text_set_adjustments, GtkText_val,
      Option_val(arg2,GtkAdjustment_val,GtkText_val(arg1)->hadj) Ignore,
      Option_val(arg3,GtkAdjustment_val,GtkText_val(arg1)->vadj) Ignore,
      Unit)
Make_Extractor (gtk_text_get, GtkText_val, hadj, Val_GtkWidget)
Make_Extractor (gtk_text_get, GtkText_val, vadj, Val_GtkWidget)
ML_2 (gtk_text_set_point, GtkText_val, Int_val, Unit)
ML_1 (gtk_text_get_point, GtkText_val, Val_int)
ML_1 (gtk_text_get_length, GtkText_val, Val_int)
ML_1 (gtk_text_freeze, GtkText_val, Unit)
ML_1 (gtk_text_thaw, GtkText_val, Unit)
CAMLprim value ml_gtk_text_insert (value text, value font, value fore,
                                   value back, value str)
{
    gtk_text_insert (GtkText_val(text),
		     Option_val(font,GdkFont_val,NULL),
		     Option_val(fore,GdkColor_val,NULL),
		     Option_val(back,GdkColor_val,NULL),
		     String_val(str), string_length(str));
    return Val_unit;
}
ML_2 (gtk_text_forward_delete, GtkText_val, Int_val, Val_int)
ML_2 (gtk_text_backward_delete, GtkText_val, Int_val, Val_int)
*/

/* gtkcombo.h */

#define GtkCombo_val(val) check_cast(GTK_COMBO,val)
/*
ML_0 (gtk_combo_new, Val_GtkWidget_sink)
ML_3 (gtk_combo_set_value_in_list, GtkCombo_val,
      Option_val(arg2, Bool_val, GtkCombo_val(arg1)->value_in_list) Ignore,
      Option_val(arg3, Bool_val, GtkCombo_val(arg1)->ok_if_empty) Ignore,
      Unit)
ML_2 (gtk_combo_set_use_arrows, GtkCombo_val, Bool_val, Unit)
ML_2 (gtk_combo_set_use_arrows_always, GtkCombo_val, Bool_val, Unit)
ML_2 (gtk_combo_set_case_sensitive, GtkCombo_val, Bool_val, Unit)
*/
ML_3 (gtk_combo_set_item_string, GtkCombo_val, GtkItem_val, String_val, Unit)
ML_1 (gtk_combo_disable_activate, GtkCombo_val, Unit)
Make_Extractor (gtk_combo, GtkCombo_val, entry, Val_GtkWidget)
Make_Extractor (gtk_combo, GtkCombo_val, list, Val_GtkWidget)

#ifdef HASGTK24
/* gtkcombobox.h */
#define GtkComboBox_val(val) check_cast(GTK_COMBO_BOX,val)

CAMLprim value 
ml_gtk_combo_box_get_active_iter(value combo)
{
  GtkTreeIter it;
  if (! gtk_combo_box_get_active_iter(GtkComboBox_val(combo), &it))
    return Val_unit;
  return ml_some(Val_GtkTreeIter(&it));
}
ML_2(gtk_combo_box_set_active_iter, GtkComboBox_val, GtkTreeIter_optval, Unit)

/* gtkentrycompletion.h */
#define GtkEntryCompletion_val(val) check_cast(GTK_ENTRY_COMPLETION,val)
ML_1(gtk_entry_completion_get_entry,GtkEntryCompletion_val,Val_GtkWidget)
ML_1(gtk_entry_completion_complete,GtkEntryCompletion_val,Unit)
ML_3(gtk_entry_completion_insert_action_text,GtkEntryCompletion_val,Int_val,String_val,Unit)
ML_3(gtk_entry_completion_insert_action_markup,GtkEntryCompletion_val,Int_val,String_val,Unit)
ML_2(gtk_entry_completion_delete_action,GtkEntryCompletion_val,Int_val,Unit)
ML_2(gtk_entry_completion_set_text_column,GtkEntryCompletion_val,Int_val,Unit)

static gboolean ml_gtk_entry_completion_match_func (GtkEntryCompletion *completion,
						    const gchar *key,
						    GtkTreeIter *iter,
						    gpointer user_data)
{
  value *closure = user_data;
  CAMLparam0();
  CAMLlocal3(vkey, viter, vret);
  vkey = copy_string(key);
  viter = Val_GtkTreeIter(iter);
  vret = callback2_exn(*closure, vkey, viter);
  if (Is_exception_result(vret))
    CAMLreturn(FALSE);
  CAMLreturn(Bool_val(vret));
}
CAMLprim value ml_gtk_entry_completion_set_match_func(value compl, value cb)
{
  value *closure = ml_global_root_new(cb);
  gtk_entry_completion_set_match_func(GtkEntryCompletion_val(compl),
				      ml_gtk_entry_completion_match_func,
				      closure,
				      ml_global_root_destroy);
  return Val_unit;
}

ML_2 (gtk_entry_set_completion, GtkEntry_val, GtkEntryCompletion_val, Unit)
CAMLprim value ml_gtk_entry_get_completion(value entry)
{
  GtkEntryCompletion *c = gtk_entry_get_completion(GtkEntry_val(entry));
  return c ? ml_some(Val_GAnyObject(c)) : Val_unit;
}
#else
Unsupported_24(gtk_combo_box_get_active_iter)
Unsupported_24(gtk_combo_box_set_active_iter)

Unsupported_24(gtk_entry_completion_get_entry)
Unsupported_24(gtk_entry_completion_complete)
Unsupported_24(gtk_entry_completion_insert_action_text)
Unsupported_24(gtk_entry_completion_insert_action_markup)
Unsupported_24(gtk_entry_completion_delete_action)
Unsupported_24(gtk_entry_completion_set_text_column)
Unsupported_24(gtk_entry_completion_set_match_func)

Unsupported_24(gtk_entry_get_completion)
Unsupported_24(gtk_entry_set_completion)
#endif /* HASGTK24 */

#ifdef HASGTK26
CAMLprim value
ml_gtk_combo_box_set_row_separator_func (value cb, value fun_o)
{
  gpointer data;
  GtkDestroyNotify dnotify;
  GtkTreeViewRowSeparatorFunc func;
  if (Is_long (fun_o))
    {
      data = NULL;
      dnotify = NULL;
      func = NULL;
    }
  else
    {
      data = ml_global_root_new (Field (fun_o, 0));
      dnotify = ml_global_root_destroy;
      func = ml_gtk_row_separator_func;
    }
  gtk_combo_box_set_row_separator_func (GtkComboBox_val (cb), func, data, dnotify);
  return Val_unit;
}
#else
Unsupported_26 (gtk_combo_box_set_row_separator_func)
#endif /* HASGTK26 */
