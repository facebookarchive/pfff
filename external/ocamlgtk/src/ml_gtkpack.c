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

/* $Id: ml_gtkpack.c 1382 2007-09-26 07:41:01Z garrigue $ */

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

Make_Flags_val (Attach_options_val)

/* Init all */

CAMLprim value ml_gtkpack_init(value unit)
{
    /* Since these are declared const, must force gcc to call them! */
    GType t =
        gtk_hbox_get_type() +
        gtk_vbox_get_type() +
        gtk_hbutton_box_get_type() +
        gtk_vbutton_box_get_type() +
        gtk_fixed_get_type() +
        gtk_layout_get_type() +
        gtk_notebook_get_type() +
        gtk_hpaned_get_type() +
        gtk_vpaned_get_type() +
        gtk_table_get_type() +
        gtk_size_group_get_type();
    return Val_GType(t);
}

/* gtkbox.h */

#define GtkBox_val(val) check_cast(GTK_BOX,val)
ML_5 (gtk_box_pack_start, GtkBox_val, GtkWidget_val, Bool_val, Bool_val,
      Int_val, Unit)
ML_5 (gtk_box_pack_end, GtkBox_val, GtkWidget_val, Bool_val, Bool_val,
      Int_val, Unit)
ML_3 (gtk_box_reorder_child, GtkBox_val, GtkWidget_val, Int_val, Unit)
CAMLprim value ml_gtk_box_query_child_packing (value box, value child)
{
    int expand, fill;
    unsigned int padding;
    GtkPackType pack_type;
    value ret;
    gtk_box_query_child_packing (GtkBox_val(box), GtkWidget_val(child),
				 &expand, &fill, &padding, &pack_type);
    ret = alloc_small(4,0);
    Field(ret,0) = Val_bool(expand);
    Field(ret,1) = Val_bool(fill);
    Field(ret,2) = Val_int(padding);
    Field(ret,3) = Val_pack_type(pack_type);
    return ret;
}
CAMLprim value ml_gtk_box_set_child_packing (value vbox, value vchild, value vexpand,
				    value vfill, value vpadding, value vpack)
{
    GtkBox *box = GtkBox_val(vbox);
    GtkWidget *child = GtkWidget_val(vchild);
    int expand, fill;
    unsigned int padding;
    GtkPackType pack;
    gtk_box_query_child_packing (box, child, &expand, &fill, &padding, &pack);
    gtk_box_set_child_packing (box, child,
			       Option_val(vexpand, Bool_val, expand),
			       Option_val(vfill, Bool_val, fill),
			       Option_val(vpadding, Int_val, padding),
			       Option_val(vpack, Pack_type_val, pack));
    return Val_unit;
}
ML_bc6 (ml_gtk_box_set_child_packing)

/* gtkbbox.h */
    
#define GtkButtonBox_val(val) check_cast(GTK_BUTTON_BOX,val)
Make_Extractor (gtk_button_box_get, GtkButtonBox_val, child_min_width, Val_int)
Make_Extractor (gtk_button_box_get, GtkButtonBox_val, child_min_height,
		Val_int)
Make_Extractor (gtk_button_box_get, GtkButtonBox_val, child_ipad_x, Val_int)
Make_Extractor (gtk_button_box_get, GtkButtonBox_val, child_ipad_y, Val_int)
Make_Extractor (gtk_button_box_get, GtkButtonBox_val, layout_style,
		Val_button_box_style)
ML_3 (gtk_button_box_set_child_size, GtkButtonBox_val,
      Int_val, Int_val, Unit)
ML_3 (gtk_button_box_set_child_ipadding, GtkButtonBox_val,
      Int_val, Int_val, Unit)
#ifdef HASGTK24
ML_2 (gtk_button_box_get_child_secondary, GtkButtonBox_val, GtkWidget_val, Val_bool)
ML_3 (gtk_button_box_set_child_secondary, GtkButtonBox_val, GtkWidget_val, Bool_val, Unit)
#else
Unsupported_24 (gtk_button_box_get_child_secondary)
Unsupported_24 (gtk_button_box_set_child_secondary)
#endif

/* gtkfixed.h */

#define GtkFixed_val(val) check_cast(GTK_FIXED,val)
ML_4 (gtk_fixed_put, GtkFixed_val, GtkWidget_val, (gint16)Long_val, (gint16)Long_val, Unit)
ML_4 (gtk_fixed_move, GtkFixed_val, GtkWidget_val, (gint16)Long_val, (gint16)Long_val, Unit)
ML_2 (gtk_fixed_set_has_window, GtkFixed_val, Int_val, Unit)
ML_1 (gtk_fixed_get_has_window, GtkFixed_val, Val_bool)

/* gtklayout.h */

#define GtkLayout_val(val) check_cast(GTK_LAYOUT,val)
ML_4 (gtk_layout_put, GtkLayout_val, GtkWidget_val, Int_val, Int_val, Unit)
ML_4 (gtk_layout_move, GtkLayout_val, GtkWidget_val, Int_val, Int_val, Unit)
ML_1 (gtk_layout_freeze, GtkLayout_val, Unit)
ML_1 (gtk_layout_thaw, GtkLayout_val, Unit)
Make_Extractor(gtk_layout, GtkLayout_val, bin_window, Val_GdkWindow)

/* gtknotebook.h */

#define GtkNotebook_val(val) check_cast(GTK_NOTEBOOK,val)
#ifdef HASGTK24
ML_5 (gtk_notebook_insert_page_menu, GtkNotebook_val, GtkWidget_val, GtkWidget_val, GtkWidget_val, Option_val(arg5,Int_val,(-1)) Ignore, Val_int)
#else
CAMLprim value ml_gtk_notebook_insert_page_menu(value nb, value w1, 
                                                value w2, value w3, 
                                                value pos)
{
  gtk_notebook_insert_page_menu(GtkNotebook_val(nb),
                                GtkWidget_val(w1),
                                GtkWidget_val(w2),
                                GtkWidget_val(w3),
                                Option_val(pos,Int_val,-1));
  return Val_int(gtk_notebook_get_current_page(GtkNotebook_val(nb)));
}
#endif
ML_2 (gtk_notebook_remove_page, GtkNotebook_val, Int_val, Unit)

ML_1 (gtk_notebook_get_current_page, GtkNotebook_val, Val_int)
ML_2 (gtk_notebook_get_nth_page, GtkNotebook_val, Int_val, Val_GtkWidget)
ML_2 (gtk_notebook_page_num, GtkNotebook_val, GtkWidget_val, Val_int)
ML_1 (gtk_notebook_next_page, GtkNotebook_val, Unit)
ML_1 (gtk_notebook_prev_page, GtkNotebook_val, Unit)

ML_2 (gtk_notebook_get_tab_label, GtkNotebook_val, GtkWidget_val,
      Val_GtkWidget)
ML_3 (gtk_notebook_set_tab_label, GtkNotebook_val, GtkWidget_val,
      GtkWidget_val, Unit)
ML_2 (gtk_notebook_get_menu_label, GtkNotebook_val, GtkWidget_val,
      Val_GtkWidget)
ML_3 (gtk_notebook_set_menu_label, GtkNotebook_val, GtkWidget_val,
      GtkWidget_val, Unit)
ML_3 (gtk_notebook_reorder_child, GtkNotebook_val, GtkWidget_val,
      Int_val, Unit)

/* gtkpaned.h */

#define GtkPaned_val(val) check_cast(GTK_PANED,val)
ML_2 (gtk_paned_add1, GtkPaned_val, GtkWidget_val, Unit)
ML_2 (gtk_paned_add2, GtkPaned_val, GtkWidget_val, Unit)
ML_4 (gtk_paned_pack1, GtkPaned_val, GtkWidget_val, Int_val, Int_val, Unit)
ML_4 (gtk_paned_pack2, GtkPaned_val, GtkWidget_val, Int_val, Int_val, Unit)
Make_Extractor (gtk_paned, GtkPaned_val, child1, Val_GtkWidget)
Make_Extractor (gtk_paned, GtkPaned_val, child2, Val_GtkWidget)

/* gtktable.h */

#define GtkTable_val(val) check_cast(GTK_TABLE,val)
ML_10 (gtk_table_attach, GtkTable_val, GtkWidget_val,
       Int_val, Int_val, Int_val, Int_val,
       Flags_Attach_options_val, Flags_Attach_options_val,
       Int_val, Int_val, Unit)
ML_bc10 (ml_gtk_table_attach)
ML_3 (gtk_table_set_row_spacing, GtkTable_val, Int_val, Int_val, Unit)
ML_3 (gtk_table_set_col_spacing, GtkTable_val, Int_val, Int_val, Unit)

/* gtksizegroup.h */
#define GtkSizeGroup_val(val) check_cast(GTK_SIZE_GROUP,val)
ML_2 (gtk_size_group_add_widget, GtkSizeGroup_val, GtkWidget_val, Unit)
ML_2 (gtk_size_group_remove_widget, GtkSizeGroup_val, GtkWidget_val, Unit)
