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

/* $Id: ml_gtklist.c 1452 2009-05-08 10:15:38Z garrigue $ */

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

Make_Flags_val (Button_action_val)

/* Init all */

CAMLprim value ml_gtklist_init(value unit)
{
    /* Since these are declared const, must force gcc to call them! */
    GType t =
        gtk_list_item_get_type() +
        gtk_list_get_type() +
        gtk_clist_get_type();
    return Val_GType(t);
}

/* gtklistitem.h */

ML_0 (gtk_list_item_new, Val_GtkWidget_sink)
ML_1 (gtk_list_item_new_with_label, String_val, Val_GtkWidget_sink)

/* gtklist.h */

#define GtkList_val(val) check_cast(GTK_LIST,val)
ML_0 (gtk_list_new, Val_GtkWidget_sink)
CAMLprim value ml_gtk_list_insert_item (value list, value item, value pos)
{
    GList *tmp_list = g_list_alloc ();
    tmp_list->data = GtkWidget_val(item);
    tmp_list->next = NULL;
    tmp_list->prev = NULL;
    gtk_list_insert_items (GtkList_val(list), tmp_list, Int_val(pos));
    return Val_unit;
}
ML_3 (gtk_list_clear_items, GtkList_val, Int_val, Int_val, Unit)
ML_2 (gtk_list_select_item, GtkList_val, Int_val, Unit)
ML_2 (gtk_list_unselect_item, GtkList_val, Int_val, Unit)
ML_2 (gtk_list_select_child, GtkList_val, GtkWidget_val, Unit)
ML_2 (gtk_list_unselect_child, GtkList_val, GtkWidget_val, Unit)
ML_2 (gtk_list_child_position, GtkList_val, GtkWidget_val, Val_int)

/* gtkclist.h */

#define GtkCList_val(val) check_cast(GTK_CLIST,val)
ML_1 (gtk_clist_new, Int_val, Val_GtkWidget_sink)
ML_1 (gtk_clist_new_with_titles, Insert(Wosize_val(arg1)) (char **),
      Val_GtkWidget_sink)
Make_Extractor (gtk_clist_get, GtkCList_val, rows, Val_int)
Make_Extractor (gtk_clist_get, GtkCList_val, columns, Val_int)
Make_Extractor (gtk_clist_get, GtkCList_val, focus_row, Val_int)
ML_2 (gtk_clist_set_hadjustment, GtkCList_val, GtkAdjustment_val, Unit)
ML_2 (gtk_clist_set_vadjustment, GtkCList_val, GtkAdjustment_val, Unit)
ML_1 (gtk_clist_get_hadjustment, GtkCList_val, Val_GtkAny)
ML_1 (gtk_clist_get_vadjustment, GtkCList_val, Val_GtkAny)
ML_2 (gtk_clist_set_shadow_type, GtkCList_val, Shadow_type_val, Unit)
ML_2 (gtk_clist_set_selection_mode, GtkCList_val, Selection_mode_val, Unit)
ML_2 (gtk_clist_set_reorderable, GtkCList_val, Bool_val, Unit)
ML_2 (gtk_clist_set_use_drag_icons, GtkCList_val, Bool_val, Unit)
ML_3 (gtk_clist_set_button_actions, GtkCList_val, Int_val,
      (guint8)Flags_Button_action_val, Unit)
ML_1 (gtk_clist_freeze, GtkCList_val, Unit)
ML_1 (gtk_clist_thaw, GtkCList_val, Unit)
ML_1 (gtk_clist_column_titles_show, GtkCList_val, Unit)
ML_1 (gtk_clist_column_titles_hide, GtkCList_val, Unit)
ML_2 (gtk_clist_column_title_active, GtkCList_val, Int_val, Unit)
ML_2 (gtk_clist_column_title_passive, GtkCList_val, Int_val, Unit)
ML_1 (gtk_clist_column_titles_active, GtkCList_val, Unit)
ML_1 (gtk_clist_column_titles_passive, GtkCList_val, Unit)
ML_3 (gtk_clist_set_column_title, GtkCList_val, Int_val, String_val, Unit)
ML_2 (gtk_clist_get_column_title, GtkCList_val, Int_val, Val_string)
ML_3 (gtk_clist_set_column_widget, GtkCList_val, Int_val, GtkWidget_val, Unit)
ML_2 (gtk_clist_get_column_widget, GtkCList_val, Int_val, Val_GtkWidget)
ML_3 (gtk_clist_set_column_justification, GtkCList_val, Int_val,
      Justification_val, Unit)
ML_3 (gtk_clist_set_column_visibility, GtkCList_val, Int_val, Bool_val, Unit)
ML_3 (gtk_clist_set_column_resizeable, GtkCList_val, Int_val, Bool_val, Unit)
ML_3 (gtk_clist_set_column_auto_resize, GtkCList_val, Int_val, Bool_val, Unit)
ML_1 (gtk_clist_columns_autosize, GtkCList_val, Unit)
ML_2 (gtk_clist_optimal_column_width, GtkCList_val, Int_val, Val_int)
ML_3 (gtk_clist_set_column_width, GtkCList_val, Int_val, Int_val, Unit)
ML_3 (gtk_clist_set_column_min_width, GtkCList_val, Int_val, Int_val, Unit)
ML_3 (gtk_clist_set_column_max_width, GtkCList_val, Int_val, Int_val, Unit)
ML_2 (gtk_clist_set_row_height, GtkCList_val, Int_val, Unit)
ML_5 (gtk_clist_moveto, GtkCList_val, Int_val, Int_val,
      Double_val, Double_val, Unit)
ML_2 (gtk_clist_row_is_visible, GtkCList_val, Int_val, Val_visibility)
ML_3 (gtk_clist_get_cell_type, GtkCList_val, Int_val, Int_val, Val_cell_type)
ML_4 (gtk_clist_set_text, GtkCList_val, Int_val, Int_val, Optstring_val, Unit)
CAMLprim value ml_gtk_clist_get_text (value clist, value row, value column)
{
    char *text;
    if (!gtk_clist_get_text (GtkCList_val(clist), Int_val(row),
			     Int_val(column), &text))
	invalid_argument ("Gtk.Clist.get_text");
    return Val_optstring(text);
}
ML_5 (gtk_clist_set_pixmap, GtkCList_val, Int_val, Int_val, GdkPixmap_val,
      (GdkBitmap*)Pointer_val, Unit)
CAMLprim value ml_gtk_clist_get_pixmap (value clist, value row, value column)
{
    CAMLparam0 ();
    GdkPixmap *pixmap;
    GdkBitmap *bitmap;
    CAMLlocal2 (vpixmap,vbitmap);
    value ret;

    if (!gtk_clist_get_pixmap (GtkCList_val(clist), Int_val(row),
			       Int_val(column), &pixmap, &bitmap))
	invalid_argument ("Gtk.Clist.get_pixmap");
    vpixmap = Val_option (pixmap, Val_GdkPixmap);
    vbitmap = Val_option (bitmap, Val_GdkBitmap);

    ret = alloc_small (2,0);
    Field(ret,0) = vpixmap;
    Field(ret,1) = vbitmap;
    CAMLreturn(ret);
}
ML_7 (gtk_clist_set_pixtext, GtkCList_val, Int_val, Int_val, String_val,
      (guint8)Long_val, GdkPixmap_val, (GdkBitmap*)Pointer_val, Unit)
ML_bc7 (ml_gtk_clist_set_pixtext)
ML_3 (gtk_clist_set_foreground, GtkCList_val, Int_val, GdkColor_val, Unit)
ML_3 (gtk_clist_set_background, GtkCList_val, Int_val, GdkColor_val, Unit)
ML_3 (gtk_clist_get_cell_style, GtkCList_val, Int_val, Int_val, Val_GtkStyle)
ML_4 (gtk_clist_set_cell_style, GtkCList_val, Int_val, Int_val, GtkStyle_val,
      Unit)
ML_2 (gtk_clist_get_row_style, GtkCList_val, Int_val, Val_GtkStyle)
ML_3 (gtk_clist_set_row_style, GtkCList_val, Int_val, GtkStyle_val, Unit)
ML_3 (gtk_clist_set_selectable, GtkCList_val, Int_val, Bool_val, Unit)
ML_2 (gtk_clist_get_selectable, GtkCList_val, Int_val, Val_bool)
ML_5 (gtk_clist_set_shift, GtkCList_val, Int_val, Int_val, Int_val, Int_val,
      Unit)
/* ML_2 (gtk_clist_append, GtkCList_val, (char **), Val_int) */
ML_3 (gtk_clist_insert, GtkCList_val, Int_val, (char **), Val_int)
ML_2 (gtk_clist_remove, GtkCList_val, Int_val, Unit)
CAMLprim value ml_gtk_clist_set_row_data (value w, value row, value data)
{
     value *data_p = ml_global_root_new (data);
     gtk_clist_set_row_data_full (GtkCList_val(w), Int_val(row),
				  data_p, ml_global_root_destroy);
     return Val_unit;
}
ML_2 (gtk_clist_get_row_data, GtkCList_val, Int_val, *(value*)Check_null)
ML_3 (gtk_clist_select_row, GtkCList_val, Int_val, Int_val, Unit)
ML_3 (gtk_clist_unselect_row, GtkCList_val, Int_val, Int_val, Unit)
ML_1 (gtk_clist_clear, GtkCList_val, Unit)
CAMLprim value ml_gtk_clist_get_selection_info (value clist, value x, value y)
{
    int row, column;
    value ret;
    if (!gtk_clist_get_selection_info (GtkCList_val(clist), Int_val(x),
			     Int_val(y), &row, &column))
	invalid_argument ("Gtk.Clist.get_row_column");
    ret = alloc_small (2,0);
    Field(ret,0) = Val_int(row);
    Field(ret,1) = Val_int(column);
    return ret;
}
ML_1 (gtk_clist_select_all, GtkCList_val, Unit)
ML_1 (gtk_clist_unselect_all, GtkCList_val, Unit)
ML_3 (gtk_clist_swap_rows, GtkCList_val, Int_val, Int_val, Unit)
ML_3 (gtk_clist_row_move, GtkCList_val, Int_val, Int_val, Unit)
ML_2 (gtk_clist_set_sort_column, GtkCList_val, Int_val, Unit)
ML_2 (gtk_clist_set_sort_type, GtkCList_val, Sort_type_val, Unit)
ML_1 (gtk_clist_sort, GtkCList_val, Unit)
ML_2 (gtk_clist_set_auto_sort, GtkCList_val, Bool_val, Unit)

ML_1 (Scroll_type_val, ID, Val_long)

CAMLprim value ml_gtk_clist_get_row_state (value clist, value y)
{
    GtkCListRow *row;
    GList *list;
    gint row_num;

    list = GtkCList_val(clist)->row_list;

    for (row_num=0; row_num < Int_val(y) ; row_num++) {
      if (list == NULL) invalid_argument ("Gtk.Clist.get_row_state");
      list = list->next;
    }

    row = list->data;
    return (Val_state_type (row->state));
}
