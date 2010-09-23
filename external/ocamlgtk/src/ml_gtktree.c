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

/* $Id: ml_gtktree.c 1496 2010-01-14 15:38:03Z ben_99_9 $ */

#include <string.h>
#include <gtk/gtk.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/printexc.h>
#include "wrappers.h"
#include "ml_glib.h"
#include "ml_gobject.h"
#include "ml_gdk.h"
#include "ml_gtk.h"
#include "gtk_tags.h"
#include "ml_gtktree.h"

/* Forward declaration */

GType custom_model_get_type (void);

/* Init all */


CAMLprim value ml_gtktree_init(value unit)
{
    /* Since these are declared const, must force gcc to call them! */
    GType t =
        gtk_tree_view_get_type() +
        gtk_tree_view_column_get_type() +
        gtk_tree_store_get_type() +
        gtk_cell_renderer_pixbuf_get_type() +
        gtk_cell_renderer_text_get_type() +
        gtk_cell_renderer_toggle_get_type () +
        gtk_list_store_get_type() +
        gtk_tree_model_sort_get_type() +
        gtk_tree_path_get_type() +
        custom_model_get_type ()
#ifdef HASGTK24
        + gtk_tree_model_filter_get_type()
#endif
#ifdef HASGTK26
        + gtk_cell_renderer_progress_get_type()
        + gtk_cell_renderer_combo_get_type()
        + gtk_icon_view_get_type()
#endif
#ifdef HASGTK210
        + gtk_cell_renderer_accel_get_type ()
#endif
        ;
    return Val_GType(t);
}

/* gtktreemodel.h */

/* "Lighter" version: allocate in the ocaml heap */
CAMLprim value ml_gtk_tree_iter_copy (value it) {
  /* Only valid if in old generation and compaction off */
  return Val_GtkTreeIter(GtkTreeIter_val(it));
}
CAMLprim value ml_alloc_GtkTreeIter(value v) {
  return alloc_memblock_indirected(sizeof(GtkTreeIter));
}

#define GtkTreeModel_val(val) check_cast(GTK_TREE_MODEL,val)

Make_Val_final_pointer_compare (GtkTreePath, Ignore, gtk_tree_path_compare, gtk_tree_path_free, 1)
#define Val_GtkTreePath_copy(p) (Val_GtkTreePath(gtk_tree_path_copy(p)))
#define GtkTreePath_val(val) ((GtkTreePath*)Pointer_val(val))

Make_Val_final_pointer (GtkTreeRowReference, Ignore,
                        gtk_tree_row_reference_free, 5)
#define GtkTreeRowReference_val(val) ((GtkTreeRowReference*)Pointer_val(val))

/* TreePath */
ML_0 (gtk_tree_path_new, Val_GtkTreePath)
ML_1 (gtk_tree_path_new_from_string, String_val, Val_GtkTreePath)
ML_1 (gtk_tree_path_to_string, GtkTreePath_val, copy_string_g_free)
ML_2 (gtk_tree_path_append_index, GtkTreePath_val, Int_val, Unit)
ML_2 (gtk_tree_path_prepend_index, GtkTreePath_val, Int_val, Unit)
ML_1 (gtk_tree_path_get_depth, GtkTreePath_val, Val_int)
CAMLprim value ml_gtk_tree_path_get_indices(value p)
{
  gint *indices = gtk_tree_path_get_indices(GtkTreePath_val(p));
  gint i, depth = gtk_tree_path_get_depth(GtkTreePath_val(p));
  value ret = alloc_tuple(depth);
  for (i = 0; i < depth; i++) Field(ret,i) = Val_int(indices[i]);
  return ret;
}
ML_1 (gtk_tree_path_copy, GtkTreePath_val, Val_GtkTreePath)
ML_1 (gtk_tree_path_next, GtkTreePath_val, Unit)
ML_1 (gtk_tree_path_prev, GtkTreePath_val, Val_bool)
ML_1 (gtk_tree_path_up, GtkTreePath_val, Val_bool)
ML_1 (gtk_tree_path_down, GtkTreePath_val, Unit)
ML_2 (gtk_tree_path_is_ancestor, GtkTreePath_val, GtkTreePath_val, Val_bool)

/* RowReference */
ML_2 (gtk_tree_row_reference_new, GtkTreeModel_val, GtkTreePath_val,
      Val_GtkTreeRowReference)
ML_1 (gtk_tree_row_reference_valid, GtkTreeRowReference_val, Val_bool)
ML_1 (gtk_tree_row_reference_get_path, GtkTreeRowReference_val,
      Val_GtkTreePath) /* already copied! */

/* TreeModel */
#define Val_TreeModel_flags(f) ml_lookup_flags_getter(ml_table_tree_model_flags,f)
ML_1 (gtk_tree_model_get_flags, GtkTreeModel_val, Val_TreeModel_flags)
ML_1 (gtk_tree_model_get_n_columns, GtkTreeModel_val, Val_int)
ML_2 (gtk_tree_model_get_column_type, GtkTreeModel_val, Int_val, Val_GType)
ML_3 (gtk_tree_model_get_iter, GtkTreeModel_val, GtkTreeIter_val,
      GtkTreePath_val, Val_bool)
ML_2 (gtk_tree_model_get_path, GtkTreeModel_val, GtkTreeIter_val,
      Val_GtkTreePath)
ML_4 (gtk_tree_model_get_value, GtkTreeModel_val, GtkTreeIter_val, Int_val,
      GValue_val, Unit)
ML_2 (gtk_tree_model_get_iter_first, GtkTreeModel_val, GtkTreeIter_val, Val_bool)
ML_2 (gtk_tree_model_iter_next, GtkTreeModel_val, GtkTreeIter_val,
      Val_bool)
ML_2 (gtk_tree_model_iter_has_child, GtkTreeModel_val, GtkTreeIter_val, Val_bool)
ML_2 (gtk_tree_model_iter_n_children, GtkTreeModel_val, GtkTreeIter_optval,
      Val_int)
ML_4 (gtk_tree_model_iter_nth_child, GtkTreeModel_val, GtkTreeIter_val,
      GtkTreeIter_optval, Int_val, Val_bool)
ML_3 (gtk_tree_model_iter_parent, GtkTreeModel_val, GtkTreeIter_val,
      GtkTreeIter_val, Val_bool)
static gboolean gtk_tree_model_foreach_func(GtkTreeModel *model,
					    GtkTreePath *path, GtkTreeIter *iter,
					    gpointer data)
{
  value *closure = data;
  CAMLparam0();
  CAMLlocal3(vpath, viter, vret);
  vpath = Val_GtkTreePath_copy(path);
  viter = Val_GtkTreeIter(iter);
  vret = callback2_exn(*closure, vpath, viter);
  if (Is_exception_result(vret)) {
    CAML_EXN_LOG("gtk_tree_model_foreach_func");
    CAMLreturn(FALSE);
  }
  CAMLreturn(Bool_val(vret));
}
CAMLprim value ml_gtk_tree_model_foreach(value m, value cb)
{
  CAMLparam1(cb);
  gtk_tree_model_foreach(GtkTreeModel_val(m),
			 gtk_tree_model_foreach_func,
			 &cb);
  CAMLreturn(Val_unit);
}
ML_3 (gtk_tree_model_row_changed, GtkTreeModel_val, GtkTreePath_val, GtkTreeIter_val, Unit)

/* gtktreestore.h */

#define GtkTreeStore_val(val) check_cast(GTK_TREE_STORE,val)
CAMLprim value ml_gtk_tree_store_newv(value arr)
{
  CAMLparam1(arr);
  int n_columns = Wosize_val(arr);
  int i;
  GType *types = (GType*)
    (n_columns ? alloc (Wosize_asize(n_columns * sizeof(GType)), Abstract_tag)
     : 0);
  for (i=0; i<n_columns; i++)
    types[i] = GType_val(Field(arr,i));
  CAMLreturn (Val_GObject_new(&gtk_tree_store_newv(n_columns, types)->parent));
}

ML_4 (gtk_tree_store_set_value, GtkTreeStore_val, GtkTreeIter_val,
      Int_val, GValue_val, Unit)
#ifdef HASGTK22
ML_2 (gtk_tree_store_remove, GtkTreeStore_val, GtkTreeIter_val, Val_bool)
#else
ML_2 (gtk_tree_store_remove, GtkTreeStore_val, GtkTreeIter_val, Val_false Ignore)
#endif

ML_4 (gtk_tree_store_insert, GtkTreeStore_val, GtkTreeIter_val,
      Option_val(arg3,GtkTreeIter_val,NULL) Ignore, Int_val, Unit)
ML_4 (gtk_tree_store_insert_before, GtkTreeStore_val, GtkTreeIter_val,
      Option_val(arg3,GtkTreeIter_val,NULL) Ignore, GtkTreeIter_val, Unit)
ML_4 (gtk_tree_store_insert_after, GtkTreeStore_val, GtkTreeIter_val,
      Option_val(arg3,GtkTreeIter_val,NULL) Ignore, GtkTreeIter_val, Unit)
ML_3 (gtk_tree_store_append, GtkTreeStore_val, GtkTreeIter_val,
      Option_val(arg3,GtkTreeIter_val,NULL) Ignore, Unit)
ML_3 (gtk_tree_store_prepend, GtkTreeStore_val, GtkTreeIter_val,
      Option_val(arg3,GtkTreeIter_val,NULL) Ignore, Unit)
ML_3 (gtk_tree_store_is_ancestor, GtkTreeStore_val, GtkTreeIter_val,
      GtkTreeIter_val, Val_bool)
ML_2 (gtk_tree_store_iter_depth, GtkTreeStore_val, GtkTreeIter_val, Val_int)
ML_1 (gtk_tree_store_clear, GtkTreeStore_val, Unit)

#ifdef HASGTK22
ML_2 (gtk_tree_store_iter_is_valid, GtkTreeStore_val, GtkTreeIter_val,
      Val_bool)
ML_3 (gtk_tree_store_swap, GtkTreeStore_val, GtkTreeIter_val,
      GtkTreeIter_val, Unit)
ML_3 (gtk_tree_store_move_before, GtkTreeStore_val, GtkTreeIter_val,
      GtkTreeIter_val, Unit)
ML_3 (gtk_tree_store_move_after, GtkTreeStore_val, GtkTreeIter_val,
      GtkTreeIter_val, Unit)
#else
Unsupported(gtk_tree_store_iter_is_valid)
Unsupported(gtk_tree_store_swap)
Unsupported(gtk_tree_store_move_before)
Unsupported(gtk_tree_store_move_after)
#endif

/* GtkListStore */

#define GtkListStore_val(val) check_cast(GTK_LIST_STORE,val)
CAMLprim value ml_gtk_list_store_newv(value arr)
{
  CAMLparam1(arr);
  int n_columns = Wosize_val(arr);
  int i;
  GType *types = (GType*)
    (n_columns ? alloc (Wosize_asize(n_columns * sizeof(GType)), Abstract_tag)
     : 0);
  for (i=0; i<n_columns; i++)
    types[i] = GType_val(Field(arr,i));
  CAMLreturn (Val_GObject_new(&gtk_list_store_newv(n_columns, types)->parent));
}

ML_4 (gtk_list_store_set_value, GtkListStore_val, GtkTreeIter_val,
      Int_val, GValue_val, Unit)

#ifdef HASGTK22
ML_2 (gtk_list_store_remove, GtkListStore_val, GtkTreeIter_val, Val_bool)
#else
ML_2 (gtk_list_store_remove, GtkListStore_val, GtkTreeIter_val, Unit)
#endif

ML_3 (gtk_list_store_insert, GtkListStore_val, GtkTreeIter_val, Int_val, Unit)
ML_3 (gtk_list_store_insert_before, GtkListStore_val, GtkTreeIter_val,
      GtkTreeIter_val, Unit)
ML_3 (gtk_list_store_insert_after, GtkListStore_val, GtkTreeIter_val,
      GtkTreeIter_val, Unit)
ML_2 (gtk_list_store_append, GtkListStore_val, GtkTreeIter_val,
      Unit)
ML_2 (gtk_list_store_prepend, GtkListStore_val, GtkTreeIter_val, Unit)
ML_1 (gtk_list_store_clear, GtkListStore_val, Unit)
#ifdef HASGTK22
ML_2 (gtk_list_store_iter_is_valid, GtkListStore_val, GtkTreeIter_val,
      Val_bool)
ML_3 (gtk_list_store_swap, GtkListStore_val, GtkTreeIter_val,
      GtkTreeIter_val, Unit)
ML_3 (gtk_list_store_move_before, GtkListStore_val, GtkTreeIter_val,
      GtkTreeIter_val, Unit)
ML_3 (gtk_list_store_move_after, GtkListStore_val, GtkTreeIter_val,
      GtkTreeIter_val, Unit)
#else
Unsupported(gtk_list_store_iter_is_valid)
Unsupported(gtk_list_store_swap)
Unsupported(gtk_list_store_move_before)
Unsupported(gtk_list_store_move_after)
#endif

/* GtkTreeSelection */

#define GtkTreeSelection_val(val) check_cast(GTK_TREE_SELECTION,val)
ML_2 (gtk_tree_selection_set_mode, GtkTreeSelection_val, Selection_mode_val,
      Unit)
ML_1 (gtk_tree_selection_get_mode, GtkTreeSelection_val, Val_selection_mode)
static gboolean gtk_tree_selection_func(GtkTreeSelection *s, GtkTreeModel *m,
					GtkTreePath *p, gboolean cs,
					gpointer clos_p)
{
  value vp = Val_GtkTreePath_copy(p);
  value ret = callback2_exn(*(value*)clos_p, vp, Val_bool(cs));
  if (Is_exception_result(ret)) {
    CAML_EXN_LOG("gtk_tree_selection_func");
    return TRUE;
  }
  return Bool_val(ret);
}
CAMLprim value ml_gtk_tree_selection_set_select_function (value s, value clos)
{
  value *clos_p = ml_global_root_new(clos);
  gtk_tree_selection_set_select_function (GtkTreeSelection_val(s),
                                          gtk_tree_selection_func,
                                          clos_p,
                                          ml_global_root_destroy);
  return Val_unit;
}
static void gtk_tree_selection_foreach_func(GtkTreeModel      *model,
					    GtkTreePath       *path,
					    GtkTreeIter       *iter,
					    gpointer           data)
{
  value p = Val_GtkTreePath_copy(path);
  value ret = callback_exn(*(value*)data, p);
  if (Is_exception_result(ret))
    CAML_EXN_LOG("gtk_tree_selection_foreach_func");
}
CAMLprim value ml_gtk_tree_selection_selected_foreach (value s, value clos)
{
  CAMLparam1(clos);
  gtk_tree_selection_selected_foreach(GtkTreeSelection_val(s),
                                      gtk_tree_selection_foreach_func,
                                      &clos);
  CAMLreturn(Val_unit);
}
#ifdef HASGTK22
ML_1 (gtk_tree_selection_count_selected_rows, GtkTreeSelection_val, Val_int)
#else
Unsupported(gtk_tree_selection_count_selected_rows)
#endif

ML_2 (gtk_tree_selection_select_path, GtkTreeSelection_val, GtkTreePath_val,
      Unit)
ML_2 (gtk_tree_selection_unselect_path, GtkTreeSelection_val, GtkTreePath_val,
      Unit)
ML_2 (gtk_tree_selection_select_iter, GtkTreeSelection_val, GtkTreeIter_val,
      Unit)
ML_2 (gtk_tree_selection_unselect_iter, GtkTreeSelection_val, GtkTreeIter_val,
      Unit)
ML_2 (gtk_tree_selection_path_is_selected, GtkTreeSelection_val,
      GtkTreePath_val, Val_bool)
ML_2 (gtk_tree_selection_iter_is_selected, GtkTreeSelection_val,
      GtkTreeIter_val, Val_bool)
ML_1 (gtk_tree_selection_select_all, GtkTreeSelection_val, Unit)
ML_1 (gtk_tree_selection_unselect_all, GtkTreeSelection_val, Unit)
ML_3 (gtk_tree_selection_select_range, GtkTreeSelection_val, GtkTreePath_val,
      GtkTreePath_val, Unit)

#ifdef HASGTK22
ML_3 (gtk_tree_selection_unselect_range, GtkTreeSelection_val, GtkTreePath_val,
      GtkTreePath_val, Unit)
#else
Unsupported(gtk_tree_selection_unselect_range)
#endif

/* GtkCellRenderer{Text,...} */

#define GtkCellRenderer_val(val) check_cast(GTK_CELL_RENDERER,val)
#define GtkCellRendererText_val(val) check_cast(GTK_CELL_RENDERER_TEXT,val)
ML_0 (gtk_cell_renderer_pixbuf_new, Val_GtkAny_sink)
ML_0 (gtk_cell_renderer_text_new, Val_GtkAny_sink)
ML_2 (gtk_cell_renderer_text_set_fixed_height_from_font,
      GtkCellRendererText_val, Int_val, Unit)
ML_0 (gtk_cell_renderer_toggle_new, Val_GtkAny_sink)

/* GtkTreeViewColumn */

#define GtkTreeViewColumn_val(val) check_cast(GTK_TREE_VIEW_COLUMN,val)
ML_0 (gtk_tree_view_column_new, Val_GtkWidget_sink)
ML_1 (gtk_tree_view_column_clear, GtkTreeViewColumn_val, Unit)
ML_3 (gtk_tree_view_column_pack_start, GtkTreeViewColumn_val,
      GtkCellRenderer_val, Int_val, Unit)
ML_3 (gtk_tree_view_column_pack_end, GtkTreeViewColumn_val,
      GtkCellRenderer_val, Int_val, Unit)
ML_2 (gtk_tree_view_column_clear_attributes, GtkTreeViewColumn_val,
      GtkCellRenderer_val, Unit)
ML_4 (gtk_tree_view_column_add_attribute, GtkTreeViewColumn_val,
      GtkCellRenderer_val, String_val, Int_val, Unit)
ML_2 (gtk_tree_view_column_set_sort_column_id, GtkTreeViewColumn_val,
      Int_val, Unit)
ML_1 (gtk_tree_view_column_get_sort_column_id, GtkTreeViewColumn_val, Val_int)

static void gtk_tree_cell_data_func(GtkTreeViewColumn *tree_column,
                                    GtkCellRenderer *cell,
				    GtkTreeModel *tree_model,
                                    GtkTreeIter *iter, gpointer data)
{
  value *closure = data;
  CAMLparam0();
  CAMLlocal3(vmod,vit,ret);
  vmod  = Val_GAnyObject(tree_model);
  vit   = Val_GtkTreeIter(iter);
  ret = callback2_exn(*closure, vmod, vit);
  if (Is_exception_result(ret))
    CAML_EXN_LOG_VERBOSE("gtk_tree_cell_data_func",ret);
  CAMLreturn0;
}
CAMLprim value
ml_gtk_tree_view_column_set_cell_data_func(value vcol, value cr, value cb)
{
  value *glob_root = NULL;
  if (Is_block(cb))
    glob_root = ml_global_root_new(Field(cb, 0));
  gtk_tree_view_column_set_cell_data_func
       (GtkTreeViewColumn_val(vcol),
        GtkCellRenderer_val(cr),
        (Is_block(cb) ? gtk_tree_cell_data_func : NULL),
        glob_root,
        ml_global_root_destroy);
  return Val_unit;
}
CAMLprim value ml_gtk_tree_view_column_get_button (value vcol)
{
  return (Val_GtkWidget(GtkTreeViewColumn_val(vcol)->button));
}

/* GtkTreeView */

#define GtkTreeView_val(val) check_cast(GTK_TREE_VIEW,val)
ML_0 (gtk_tree_view_new, Val_GtkWidget_sink)
ML_1 (gtk_tree_view_new_with_model, GtkTreeModel_val, Val_GtkWidget_sink)
ML_1 (gtk_tree_view_get_selection, GtkTreeView_val, Val_GtkWidget)
ML_1 (gtk_tree_view_columns_autosize, GtkTreeView_val, Unit)
ML_2 (gtk_tree_view_append_column, GtkTreeView_val, GtkTreeViewColumn_val,
      Val_int)
ML_2 (gtk_tree_view_remove_column, GtkTreeView_val, GtkTreeViewColumn_val,
      Val_int)
ML_3 (gtk_tree_view_insert_column, GtkTreeView_val, GtkTreeViewColumn_val,
      Int_val, Val_int)
ML_2 (gtk_tree_view_get_column, GtkTreeView_val, Int_val, Val_GtkWidget)
ML_3 (gtk_tree_view_move_column_after, GtkTreeView_val, GtkTreeViewColumn_val,
      GtkTreeViewColumn_val, Unit)
ML_3 (gtk_tree_view_scroll_to_point, GtkTreeView_val, Int_val, Int_val, Unit)
ML_4 (gtk_tree_view_scroll_to_cell, GtkTreeView_val, GtkTreePath_val,
      GtkTreeViewColumn_val, Insert(Bool_val(arg4))
      Insert(Bool_val(arg4) ? Float_val(Field(Field(arg4,0),0)) : 0)
      (Bool_val(arg4) ? Float_val(Field(Field(arg4,0),1)) : 0) Ignore,
      Unit)
ML_3 (gtk_tree_view_row_activated, GtkTreeView_val, GtkTreePath_val,
      GtkTreeViewColumn_val, Unit)
ML_1 (gtk_tree_view_expand_all, GtkTreeView_val, Unit)
ML_1 (gtk_tree_view_collapse_all, GtkTreeView_val, Unit)
ML_3 (gtk_tree_view_expand_row, GtkTreeView_val, GtkTreePath_val,
      Bool_val, Unit)
#ifdef HASGTK22
ML_2 (gtk_tree_view_expand_to_path, GtkTreeView_val, GtkTreePath_val, Unit)
#else
Unsupported(gtk_tree_view_expand_to_path)
#endif
ML_2 (gtk_tree_view_collapse_row, GtkTreeView_val, GtkTreePath_val, Unit)
ML_2 (gtk_tree_view_row_expanded, GtkTreeView_val, GtkTreePath_val, Val_bool)
ML_4 (gtk_tree_view_set_cursor, GtkTreeView_val, GtkTreePath_val,
      GtkTreeViewColumn_val, Bool_val, Unit)

#ifdef HASGTK22
ML_5 (gtk_tree_view_set_cursor_on_cell, GtkTreeView_val, GtkTreePath_val,
      GtkTreeViewColumn_val, GtkCellRenderer_val, Bool_val, Unit)
#else
Unsupported(gtk_tree_view_set_cursor_on_cell)
#endif

CAMLprim value ml_gtk_tree_view_get_cursor (value arg)
{
  CAMLparam0();
  CAMLlocal1(ret);
  GtkTreePath *path;
  GtkTreeViewColumn *col;
  gtk_tree_view_get_cursor(GtkTreeView_val(arg), &path, &col);
  ret = alloc_tuple(2);
  Store_field(ret,0,Val_option(path,Val_GtkTreePath));
  Store_field(ret,1,Val_option(col,Val_GtkWidget));
  CAMLreturn(ret);
}

CAMLprim value ml_gtk_tree_view_get_path_at_pos(value treeview,
                                                value x, value y)
{
  gint cell_x;
  gint cell_y;
  GtkTreePath *gpath;
  GtkTreeViewColumn *gcolumn;

  if (gtk_tree_view_get_path_at_pos( GtkTreeView_val(treeview),
				     Int_val(x),
				     Int_val(y),
				     &gpath,
				     &gcolumn,
				     &cell_x, &cell_y))
  { /* return Some */
    CAMLparam0 ();
    CAMLlocal1(tup);

    tup = alloc_tuple(4);
    Store_field(tup,0,Val_GtkTreePath(gpath));
    Store_field(tup,1,Val_GtkAny(gcolumn));
    Store_field(tup,2,Val_int(cell_x));
    Store_field(tup,3,Val_int(cell_y));
    CAMLreturn(ml_some (tup));
  }
  return Val_unit;
}

CAMLprim value
ml_gtk_tree_view_get_cell_area(value treeview, value path, value col)
{
  CAMLparam0 ();
  GdkRectangle grect;

  gtk_tree_view_get_cell_area(
    GtkTreeView_val(treeview),
    Option_val(path,GtkTreePath_val,NULL),
    Option_val(col,GtkTreeViewColumn_val,NULL),
    &grect);
  CAMLreturn (Val_copy (grect));
}

CAMLprim value
ml_gtk_tree_view_enable_model_drag_dest (value tv, value t, value a)
{
  CAMLparam3 (tv,t,a);
  GtkTargetEntry *targets = NULL;
  int i, n_targets = Wosize_val(t);
  
  if (n_targets)
    targets = (GtkTargetEntry *) alloc
      ( Wosize_asize(n_targets * sizeof(GtkTargetEntry))
      , Abstract_tag );
  for (i=0; i<n_targets; i++)
  {
    targets[i].target = String_val(Field(Field(t, i), 0));
    targets[i].flags  = Flags_Target_flags_val(Field(Field(t, i), 1));
    targets[i].info   = Int_val(Field(Field(t, i), 2));
  }
  gtk_tree_view_enable_model_drag_dest
    ( GtkTreeView_val(tv)
    , targets
    , n_targets
    , Flags_GdkDragAction_val(a) );
  CAMLreturn(Val_unit);
}
ML_1 (gtk_tree_view_unset_rows_drag_dest, GtkTreeView_val, Unit)

CAMLprim value
ml_gtk_tree_view_enable_model_drag_source (value tv, value m, value t, value a)
{
  CAMLparam4 (tv,m,t,a);
  GtkTargetEntry *targets = NULL;
  int i, n_targets = Wosize_val(t);
  
  if (n_targets)
    targets = (GtkTargetEntry *) alloc
      ( Wosize_asize(n_targets * sizeof(GtkTargetEntry))
      , Abstract_tag );
  for (i=0; i<n_targets; i++)
  {
    targets[i].target = String_val(Field(Field(t, i), 0));
    targets[i].flags  = Flags_Target_flags_val(Field(Field(t, i), 1));
    targets[i].info   = Int_val(Field(Field(t, i), 2));
  }
  gtk_tree_view_enable_model_drag_source
    ( GtkTreeView_val(tv)
    , OptFlags_GdkModifier_val(m)
    , targets
    , n_targets
    , Flags_GdkDragAction_val(a) );
  CAMLreturn(Val_unit);
}
ML_1 (gtk_tree_view_unset_rows_drag_source, GtkTreeView_val, Unit)

CAMLprim value
ml_gtk_tree_view_get_dest_row_at_pos (value treeview, value x, value y)
{
  GtkTreePath *path;
  GtkTreeViewDropPosition pos;

  if (gtk_tree_view_get_dest_row_at_pos(
    GtkTreeView_val(treeview),
    Int_val(x), Int_val(y),
    &path, &pos))
  { /* return Some */
    CAMLparam0 ();
    CAMLlocal1(tup);

    tup = alloc_tuple(2);
    Store_field(tup,0,Val_GtkTreePath(path));
    Store_field(tup,1,Val_tree_view_drop_position(pos));
    CAMLreturn(ml_some (tup));
  }
  return Val_unit;
}

#ifdef HASGTK26
gboolean
ml_gtk_row_separator_func (GtkTreeModel *model,
			   GtkTreeIter *iter,
			   gpointer data)
{
  gboolean ret = FALSE;
  value *closure = data;
  CAMLparam0();
  CAMLlocal3 (arg1, arg2, mlret);
  arg1 = Val_GAnyObject (model);
  arg2 = Val_GtkTreeIter (iter);
  mlret = callback2_exn (*closure, arg1, arg2);
  if (Is_exception_result (ret))
    CAML_EXN_LOG ("gtk_row_separator_func");
  else
    ret = Bool_val (mlret);
  CAMLreturn (ret);
}

CAMLprim value
ml_gtk_tree_view_set_row_separator_func (value cb, value fun_o)
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
  gtk_tree_view_set_row_separator_func (GtkTreeView_val (cb), func, data, dnotify);
  return Val_unit;
}
#else
Unsupported_26 (gtk_tree_view_set_row_separator_func)
#endif /* HASGTK26 */

#ifdef HASGTK212
CAMLprim value
ml_gtk_tree_view_set_tooltip_cell (value treeview, value tooltip,
                                   value path, value col, value cell,
                                   value unit)
{
  gtk_tree_view_set_tooltip_cell (
    GtkTreeView_val(treeview),
    GtkTooltip_val(tooltip),
    GtkTreePath_optval(path),
    GtkTreeViewColumn_optval(col),
    GtkCellRenderer_optval(cell) );
  return (Val_unit);
} /* All those lines because of that: http://caml.inria.fr/mantis/view.php?id=4396 */
ML_bc6(ml_gtk_tree_view_set_tooltip_cell)
ML_3 (gtk_tree_view_set_tooltip_row, GtkTreeView_val, GtkTooltip_val, GtkTreePath_val, Unit)
CAMLprim value
ml_gtk_tree_view_get_tooltip_context (value treeview, value x, value y, value kbd)
{
  CAMLparam4 (treeview, x, y, kbd);
  CAMLlocal3(tup, opt, sub);
  gint _x = Int_val(x);
  gint _y = Int_val(y);
  GtkTreeModel *model;
  GtkTreePath *path;
  GtkTreeIter iter;
  gboolean boo;
  
  boo = gtk_tree_view_get_tooltip_context (
    GtkTreeView_val(treeview),
    &_x, &_y, Bool_val(kbd),
    &model, &path, &iter );
  
  tup = alloc_tuple(3);
  Store_field(tup, 0, Val_int(_x));
  Store_field(tup, 1, Val_int(_y));
  opt = Val_unit;
  if (boo) {
    sub = alloc_tuple(3);
    Store_field(sub, 0, Val_GAnyObject(model));
    Store_field(sub, 1, Val_GtkTreePath(path));
    Store_field(sub, 2, Val_GtkTreeIter(&iter));
    opt = ml_some(sub);
  }
  Store_field(tup, 2, opt);
  
  CAMLreturn (tup);
}
ML_1 (gtk_tree_view_get_tooltip_column, GtkTreeView_val, Val_int)
ML_2 (gtk_tree_view_set_tooltip_column, GtkTreeView_val, Int_val, Unit)
#else
Unsupported_212 (gtk_tree_view_set_tooltip_cell)
Unsupported_212 (gtk_tree_view_set_tooltip_cell_bc)
Unsupported_212 (gtk_tree_view_set_tooltip_row)
Unsupported_212 (gtk_tree_view_get_tooltip_context)
Unsupported_212 (gtk_tree_view_get_tooltip_column)
Unsupported_212 (gtk_tree_view_set_tooltip_column)
#endif /* HASGTK212 */

/* GtkCellLayout */
#ifdef HASGTK24
#define GtkCellLayout_val(val) check_cast(GTK_CELL_LAYOUT,val)
ML_3 (gtk_cell_layout_pack_start, GtkCellLayout_val, GtkCellRenderer_val, Bool_val, Unit)
ML_3 (gtk_cell_layout_pack_end,   GtkCellLayout_val, GtkCellRenderer_val, Bool_val, Unit)
ML_3 (gtk_cell_layout_reorder,   GtkCellLayout_val, GtkCellRenderer_val, Int_val, Unit)
ML_1 (gtk_cell_layout_clear, GtkCellLayout_val, Unit)
ML_4 (gtk_cell_layout_add_attribute, GtkCellLayout_val, GtkCellRenderer_val, String_val, Int_val, Unit)
ML_2 (gtk_cell_layout_clear_attributes, GtkCellLayout_val, GtkCellRenderer_val, Unit)

CAMLprim value ml_gtk_cell_layout_set_cell_data_func(value lay, value cr, value cb)
{
  if (Is_block(cb)) {
    value *glob_root = ml_global_root_new(Field(cb, 0));
    gtk_cell_layout_set_cell_data_func (GtkCellLayout_val(lay),
				        GtkCellRenderer_val(cr),
				        (GtkCellLayoutDataFunc) gtk_tree_cell_data_func,
					glob_root,
					ml_global_root_destroy);
  }
  else
    gtk_cell_layout_set_cell_data_func (GtkCellLayout_val(lay), GtkCellRenderer_val(cr),
					NULL, NULL, NULL);

  return Val_unit;
}

#else
Unsupported_24(gtk_cell_layout_pack_start)
Unsupported_24(gtk_cell_layout_pack_end)
Unsupported_24(gtk_cell_layout_reorder)
Unsupported_24(gtk_cell_layout_clear)
Unsupported_24(gtk_cell_layout_add_attribute)
Unsupported_24(gtk_cell_layout_clear_attributes)
Unsupported_24(gtk_cell_layout_set_cell_data_func)
#endif

/* TreeModelSort */
#define GtkTreeModelSort_val(val) check_cast(GTK_TREE_MODEL_SORT,val)
ML_2 (gtk_tree_model_sort_convert_child_path_to_path, GtkTreeModelSort_val, GtkTreePath_val, Val_GtkTreePath)
CAMLprim value ml_gtk_tree_model_sort_convert_child_iter_to_iter(value m, value it)
{
  GtkTreeIter dst_it;
  gtk_tree_model_sort_convert_child_iter_to_iter(GtkTreeModelSort_val(m), &dst_it, GtkTreeIter_val(it));
  return Val_GtkTreeIter(&dst_it);
}
ML_2 (gtk_tree_model_sort_convert_path_to_child_path, GtkTreeModelSort_val, GtkTreePath_val, Val_GtkTreePath)
CAMLprim value ml_gtk_tree_model_sort_convert_iter_to_child_iter(value m, value it)
{
  GtkTreeIter dst_it;
  gtk_tree_model_sort_convert_iter_to_child_iter(GtkTreeModelSort_val(m), &dst_it, GtkTreeIter_val(it));
  return Val_GtkTreeIter(&dst_it);
}
ML_1 (gtk_tree_model_sort_reset_default_sort_func, GtkTreeModelSort_val, Unit)
#ifdef HASGTK22
ML_2 (gtk_tree_model_sort_iter_is_valid, GtkTreeModelSort_val, GtkTreeIter_val, Val_bool)
#else
Unsupported(gtk_tree_model_sort_iter_is_valid)
#endif

/* TreeSortable */
#define GtkTreeSortable_val(val) check_cast(GTK_TREE_SORTABLE,val)
ML_1 (gtk_tree_sortable_sort_column_changed, GtkTreeSortable_val, Unit)
CAMLprim value ml_gtk_tree_sortable_get_sort_column_id(value m)
{
  gint sort_column_id;
  GtkSortType order;
  if (! gtk_tree_sortable_get_sort_column_id(GtkTreeSortable_val(m),
					     &sort_column_id, &order))
    return Val_unit;
  {
    value vo, ret;
    vo = Val_sort_type(order);
    ret = alloc_small(2, 0);
    Field(ret, 0) = Val_int(sort_column_id);
    Field(ret, 1) = vo;
    return ml_some(ret);
  }
}
ML_3 (gtk_tree_sortable_set_sort_column_id, GtkTreeSortable_val, Int_val, Sort_type_val, Unit)

static gint gtk_tree_iter_compare_func(GtkTreeModel *model,
				       GtkTreeIter  *a,
				       GtkTreeIter  *b,
				       gpointer      user_data)
{
  value *clos = user_data;
  CAMLparam0();
  CAMLlocal4(ret, obj, iter_a, iter_b);
  iter_a = Val_GtkTreeIter(a);
  iter_b = Val_GtkTreeIter(b);
  obj = Val_GAnyObject(model);
  ret = callback3_exn(*clos, obj, iter_a, iter_b);
  if (Is_exception_result(ret)) {
    CAML_EXN_LOG("gtk_tree_iter_compare_func");
    CAMLreturn(0);
  }
  CAMLreturn(Int_val(ret));
}

CAMLprim value ml_gtk_tree_sortable_set_sort_func(value m, value id,
						  value sort_fun)
{
  value *clos = ml_global_root_new(sort_fun);
  gtk_tree_sortable_set_sort_func(GtkTreeSortable_val(m), Int_val(id),
				  gtk_tree_iter_compare_func,
				  clos, ml_global_root_destroy);
  return Val_unit;
}

CAMLprim value ml_gtk_tree_sortable_set_default_sort_func(value m,
							  value sort_fun)
{
  value *clos = ml_global_root_new(sort_fun);
  gtk_tree_sortable_set_default_sort_func(GtkTreeSortable_val(m),
					  gtk_tree_iter_compare_func,
					  clos, ml_global_root_destroy);
  return Val_unit;
}

ML_1 (gtk_tree_sortable_has_default_sort_func, GtkTreeSortable_val, Val_bool)

/* TreeModelFilter */
#ifdef HASGTK24
#define GtkTreeModelFilter_val(val) check_cast(GTK_TREE_MODEL_FILTER,val)

static gboolean gtk_tree_model_filter_visible_func(GtkTreeModel *model,
						   GtkTreeIter  *iter,
						   gpointer      data)
{
  value *clos = data;
  CAMLparam0();
  CAMLlocal3(ret, obj, it);
  it  = Val_GtkTreeIter(iter);
  obj = Val_GAnyObject(model);
  ret = callback2_exn(*clos, obj, it);
  if (Is_exception_result(ret)) {
    CAML_EXN_LOG("gtk_tree_model_filter_visible_func");
    CAMLreturn(FALSE);
  }
  CAMLreturn(Bool_val(ret));
}

CAMLprim value ml_gtk_tree_model_filter_set_visible_func(value m, value f)
{
  gtk_tree_model_filter_set_visible_func(GtkTreeModelFilter_val(m),
					 gtk_tree_model_filter_visible_func,
					 ml_global_root_new(f),
					 ml_global_root_destroy);
  return Val_unit;
}

ML_2 (gtk_tree_model_filter_set_visible_column, GtkTreeModelFilter_val,
      Int_val, Unit)
ML_1 (gtk_tree_model_filter_refilter, GtkTreeModelFilter_val, Unit)
ML_2 (gtk_tree_model_filter_convert_child_path_to_path, GtkTreeModelFilter_val,
      GtkTreePath_val, Val_GtkTreePath)
CAMLprim value ml_gtk_tree_model_filter_convert_child_iter_to_iter(value m,
								   value it)
{
  GtkTreeIter dst_it;
  gtk_tree_model_filter_convert_child_iter_to_iter(GtkTreeModelFilter_val(m),
						   &dst_it,
						   GtkTreeIter_val(it));
  return Val_GtkTreeIter(&dst_it);
}
ML_2 (gtk_tree_model_filter_convert_path_to_child_path, GtkTreeModelFilter_val,
      GtkTreePath_val, Val_GtkTreePath)
CAMLprim value ml_gtk_tree_model_filter_convert_iter_to_child_iter(value m,
								   value it)
{
  GtkTreeIter dst_it;
  gtk_tree_model_filter_convert_iter_to_child_iter(GtkTreeModelFilter_val(m),
						   &dst_it,
						   GtkTreeIter_val(it));
  return Val_GtkTreeIter(&dst_it);
}

#else

Unsupported_24 (gtk_tree_model_filter_set_visible_func)
Unsupported_24 (gtk_tree_model_filter_set_visible_column)
Unsupported_24 (gtk_tree_model_filter_refilter)
Unsupported_24 (gtk_tree_model_filter_convert_child_path_to_path)
Unsupported_24 (gtk_tree_model_filter_convert_child_iter_to_iter)
Unsupported_24 (gtk_tree_model_filter_convert_path_to_child_path)
Unsupported_24 (gtk_tree_model_filter_convert_iter_to_child_iter)

#endif /* HASGTK24 */

/* GtkIconView */
#ifdef HASGTK26
#define GtkIconView_val(val) check_cast(GTK_ICON_VIEW,val)
#define Val_option_GtkTreePath(v) Val_option(v,Val_GtkTreePath)
ML_3 (gtk_icon_view_get_path_at_pos, GtkIconView_val, Int_val, Int_val, Val_option_GtkTreePath)
static void ml_iconview_foreach (GtkIconView *icon_view, GtkTreePath *path, gpointer data)
{
  value *cb = data;
  value p;
  p = Val_GtkTreePath_copy(path);
  callback_exn(*cb, p);
}
CAMLprim value ml_gtk_icon_view_selected_foreach (value i, value cb)
{
  CAMLparam2(i, cb);
  gtk_icon_view_selected_foreach (GtkIconView_val(i), ml_iconview_foreach, &cb);
  CAMLreturn(Val_unit);
}
ML_2 (gtk_icon_view_select_path, GtkIconView_val, GtkTreePath_val, Unit)
ML_2 (gtk_icon_view_unselect_path, GtkIconView_val, GtkTreePath_val, Unit)
ML_2 (gtk_icon_view_path_is_selected, GtkIconView_val, GtkTreePath_val, Val_bool)
CAMLprim value ml_gtk_icon_view_get_selected_items (value i)
{
  CAMLparam1(i);
  CAMLlocal3(path, cell, list);
  GList *l, *head;
  head = gtk_icon_view_get_selected_items (GtkIconView_val(i));
  l = g_list_last (head);
  list = Val_emptylist;
  while (l) {
    GtkTreePath *p = l->data;
    path = Val_GtkTreePath(p);
    cell = alloc_small(2, Tag_cons);
    Field(cell, 0) = path;
    Field(cell, 1) = list;
    list = cell;
    l=l->prev;
  }
  g_list_free(head);
  CAMLreturn(list);
}
ML_1 (gtk_icon_view_select_all, GtkIconView_val, Unit)
ML_1 (gtk_icon_view_unselect_all, GtkIconView_val, Unit)
ML_2 (gtk_icon_view_item_activated, GtkIconView_val, GtkTreePath_val, Unit)

#else

Unsupported_26(gtk_icon_view_get_path_at_pos)
Unsupported_26(gtk_icon_view_selected_foreach)
Unsupported_26(gtk_icon_view_select_path)
Unsupported_26(gtk_icon_view_unselect_path)
Unsupported_26(gtk_icon_view_path_is_selected)
Unsupported_26(gtk_icon_view_get_selected_items)
Unsupported_26(gtk_icon_view_select_all)
Unsupported_26(gtk_icon_view_unselect_all)
Unsupported_26(gtk_icon_view_item_activated)

#endif /* HASGTK26 */

/* Custom models: this code is inspired by the code of Robert Schneck <schneck@gmail.com> */

extern void caml_minor_collection(void);

#if 0
/* Debugging code */
char *buf1;
char buf2[1000];
#define USER_DATA(iter) (iter?(long)((iter)->user_data):0)
#define USER_DATA2(iter) (iter?(long)((iter)->user_data2):0)
#define USER_DATA3(iter) (iter?(long)((iter)->user_data3):0)
#define PRINT4(iter) iter,USER_DATA(iter),USER_DATA2(iter),USER_DATA3(iter)
#define PRINT4_VALID(iter) iter, (long)(iter)->user_data, (long)(iter)->user_data2, (long)(iter)->user_data3
#define PATH_STRING(path) (buf1 = (path) ? gtk_tree_path_to_string(path) : "[]", strcpy(buf2,buf1), (path) ? g_free(buf1) : 0, buf2)
#define debug_print printf
#else
#define debug_print(...)
#endif

value callback4(value closure, value arg1, value arg2, value arg3, value arg4)
{
  value arg[4];
  arg[0] = arg1;
  arg[1] = arg2;
  arg[2] = arg3;
  arg[3] = arg4;
  return callbackN(closure, 4, arg);
}

#define ACCESS_PUBLIC_METHOD(method,object, name, block)       \
  {static value method_hash = 0; \
   if (method_hash==0) method_hash = caml_hash_variant(name); \
    {value method = caml_get_public_method(object,method_hash); \
     if ((void*)method == NULL) \
      {printf("Internal error: could not access method '%s'\n", name); \
       exit(2);}; \
     block ; }};

/*****************************************************************************
 * GObject stuff
 *****************************************************************************/

/* Some boilerplate GObject defines. 'klass' is used instead of 'class', because 'class' is a C++ keyword */
#define TYPE_CUSTOM_MODEL                  (custom_model_get_type ())
#define CUSTOM_MODEL(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_CUSTOM_MODEL, Custom_model))
#define CUSTOM_MODEL_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass),  TYPE_CUSTOM_MODEL, Custom_model_class))
#define IS_CUSTOM_MODEL(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_CUSTOM_MODEL))
#define IS_CUSTOM_MODEL_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass),  TYPE_CUSTOM_MODEL))
#define CUSTOM_MODEL_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj),  TYPE_CUSTOM_MODEL, Custom_model_class))

static GObjectClass *parent_class = NULL;  /* GObject stuff - nothing to worry about */

typedef struct _Custom_model       Custom_model;
typedef struct _Custom_model_class  Custom_model_class;

struct _Custom_model
{
  GObject parent;      /* this MUST be the first member */

  gint stamp;
  value callback_object;
};

/* Custom_model_class: more boilerplate GObject stuff */
struct _Custom_model_class
{
  GObjectClass parent_class;
};

/* boring declarations of local functions */
/* GObject stuff */

static void custom_model_init (Custom_model *pkg_tree);
static void custom_model_class_init (Custom_model_class *klass);
static void custom_model_tree_model_init (GtkTreeModelIface *iface);
static void custom_model_finalize (GObject *object);

/* tree model stuff */
static GtkTreeModelFlags custom_model_get_flags (GtkTreeModel *tree_model);
static gint custom_model_get_n_columns (GtkTreeModel *tree_model);
static GType custom_model_get_column_type (GtkTreeModel *tree_model, gint index);
static gboolean custom_model_get_iter (GtkTreeModel *tree_model, GtkTreeIter *iter, GtkTreePath *path);
static GtkTreePath *custom_model_get_path (GtkTreeModel *tree_model, GtkTreeIter *iter);
static void custom_model_get_value (GtkTreeModel *tree_model, GtkTreeIter *iter, gint column, GValue *value);
static gboolean custom_model_iter_next (GtkTreeModel *tree_model, GtkTreeIter *iter);
static gboolean custom_model_iter_children (GtkTreeModel *tree_model, GtkTreeIter *iter, GtkTreeIter *parent);
static gboolean custom_model_iter_has_child (GtkTreeModel *tree_model, GtkTreeIter *iter);
static gint custom_model_iter_n_children (GtkTreeModel *tree_model, GtkTreeIter *iter);
static gboolean custom_model_iter_nth_child (GtkTreeModel *tree_model, GtkTreeIter *iter, GtkTreeIter *parent, gint n);
static gboolean custom_model_iter_parent (GtkTreeModel *tree_model, GtkTreeIter *iter, GtkTreeIter *child);
static void custom_model_ref_node (GtkTreeModel *tree_model, GtkTreeIter *iter);
static void custom_model_unref_node (GtkTreeModel *tree_model, GtkTreeIter *iter);

GType
custom_model_get_type (void)
{
  /* Some boilerplate type registration stuff */
  static GType custom_model_type = 0;

  if (!custom_model_type)
  {
    static const GTypeInfo custom_model_info =
    {
      sizeof (Custom_model_class),
      NULL,                                         /* base_init */
      NULL,                                         /* base_finalize */
      (GClassInitFunc) custom_model_class_init,
      NULL,                                         /* class finalize */
      NULL,                                         /* class_data */
      sizeof (Custom_model),
      0,                                           /* n_preallocs */
      (GInstanceInitFunc) custom_model_init
    };

    static const GInterfaceInfo tree_model_info =
    {
      (GInterfaceInitFunc) custom_model_tree_model_init,
      NULL,
      NULL
    };

    custom_model_type = g_type_register_static (G_TYPE_OBJECT, "Custom_model",
                                               &custom_model_info, (GTypeFlags)0);

    /* Here we register our GtkTreeModel interface with the type system */
    g_type_add_interface_static (custom_model_type, GTK_TYPE_TREE_MODEL, &tree_model_info);
  }

  return custom_model_type;
}

/* more boilerplate GObject stuff */
static void
custom_model_class_init (Custom_model_class *klass)
{
  GObjectClass *object_class;

  parent_class = (GObjectClass*) g_type_class_peek_parent (klass);
  object_class = (GObjectClass*) klass;

  object_class->finalize = custom_model_finalize;
}

static void
custom_model_tree_model_init (GtkTreeModelIface *iface)
{
  iface->get_flags       = custom_model_get_flags;
  iface->get_n_columns   = custom_model_get_n_columns;
  iface->get_column_type = custom_model_get_column_type;
  iface->get_iter        = custom_model_get_iter;
  iface->get_path        = custom_model_get_path;
  iface->get_value       = custom_model_get_value;
  iface->iter_next       = custom_model_iter_next;
  iface->iter_children   = custom_model_iter_children;
  iface->iter_has_child  = custom_model_iter_has_child;
  iface->iter_n_children = custom_model_iter_n_children;
  iface->iter_nth_child  = custom_model_iter_nth_child;
  iface->iter_parent     = custom_model_iter_parent;
  iface->ref_node        = custom_model_ref_node;
  iface->unref_node      = custom_model_unref_node;
}

/* called every time a new custom model object is created */
static void
custom_model_init (Custom_model *custom_model)
{
  debug_print("custom_model_init %p\n",custom_model);
  do
    {
      custom_model->stamp = g_random_int ();
    }
  while (custom_model->stamp == 0);
}

/* called just before a custom model object is destroyed */
static void
custom_model_finalize (GObject *object)
{
  /* must chain up - finalize parent */
  (* parent_class->finalize) (object);
}

/*****************************************************************************
 * Tree Model interface stuff
 *****************************************************************************/

#define UNWRAP_OPTION(id,expr,block) \
  {value id##_aux = expr;			\
   value id = Option_val(id##_aux,/* blank */,0); \
   block }

#define decode_iter_option(custom_model,iter)			\
  (iter ? ml_some(decode_iter(custom_model,iter)) : Val_unit)

void
encode_iter(Custom_model *custom_model, GtkTreeIter *iter, value v)
{
  debug_print("encode_iter %p %p %p\n",custom_model,iter,(void*)v);
  g_return_if_fail (IS_CUSTOM_MODEL (custom_model));
  { value callback_object = custom_model->callback_object;
  ACCESS_PUBLIC_METHOD(method,callback_object,"custom_encode_iter",

  { value triple = callback2(method,callback_object,v);
    value v1 = Field(triple,0);
    value v2 = Field(triple,1);
    value v3 = Field(triple,2);
  /* Ideally, the user would already have ensured all these were stable...
     and in any case, it is always up to the user to ensure that they will
     not get garbage collected */
    if((Is_block(v1) && (char*)v1 < (char*)caml_young_end && (char*)v1 > (char*)caml_young_start) ||
       (Is_block(v2) && (char*)v2 < (char*)caml_young_end && (char*)v2 > (char*)caml_young_start) ||
       (Is_block(v3) && (char*)v3 < (char*)caml_young_end && (char*)v3 > (char*)caml_young_start))
      {
	caml_register_global_root (&v1);
	caml_register_global_root (&v2);
	caml_register_global_root (&v3);
	caml_minor_collection();
	caml_remove_global_root (&v1);
	caml_remove_global_root (&v2);
	caml_remove_global_root (&v3);
    }
    
    iter->stamp = custom_model->stamp;
    iter->user_data = (gpointer) v1;
    iter->user_data2 = (gpointer) v2;
    iter->user_data3 = (gpointer) v3;
  })
    }
}

value
decode_iter(Custom_model *custom_model, GtkTreeIter *iter)
{
  debug_print("decode_iter %p %p:%ld:%ld:%ld\n",custom_model,PRINT4(iter));
  g_return_val_if_fail (IS_CUSTOM_MODEL (custom_model), 0);
  { value callback_object = custom_model->callback_object;

  ACCESS_PUBLIC_METHOD(method, callback_object, "custom_decode_iter",

  return callback4(method,callback_object,
		      (value)iter->user_data,
		      (value)iter->user_data2,
		      (value)iter->user_data3);)
  }
}

static GtkTreeModelFlags
custom_model_get_flags (GtkTreeModel *tree_model)
{
  debug_print("get_flags %p\n",tree_model);
  g_return_val_if_fail (IS_CUSTOM_MODEL (tree_model), 0);
  {
	  Custom_model *custom_model = (Custom_model *) tree_model;
	  value callback_object = custom_model->callback_object;

  ACCESS_PUBLIC_METHOD(method, callback_object, "custom_flags",
  { value flags_list = callback(method, callback_object);
    GtkTreeModelFlags flags = (GtkTreeModelFlags) 0;
    static value iter_persist_hash=0;
    static value list_only_hash=0;
    if (iter_persist_hash==0) 
      iter_persist_hash=caml_hash_variant("ITERS_PERSIST");
    if (list_only_hash==0) list_only_hash=caml_hash_variant("LIST_ONLY");
    while (flags_list != Val_int(0))
      { value flag = Field(flags_list,0);
	flags_list = Field(flags_list,1);
	if (flag == iter_persist_hash)
    	  flags = flags | GTK_TREE_MODEL_ITERS_PERSIST;
	if (flag == list_only_hash)
    	  flags = flags | GTK_TREE_MODEL_LIST_ONLY;
      }
    return flags;
  })
    }
}

static gint
custom_model_get_n_columns (GtkTreeModel *tree_model)
{
  debug_print("get_n_columns %p\n",tree_model);
  g_return_val_if_fail (IS_CUSTOM_MODEL (tree_model), 0);
  { Custom_model *custom_model = (Custom_model *) tree_model;
    value callback_object = custom_model->callback_object;
    ACCESS_PUBLIC_METHOD(method,callback_object,"custom_n_columns",
    { value n_columns = callback(method,callback_object);
      return Int_val(n_columns);})}
}

static GType
custom_model_get_column_type (GtkTreeModel *tree_model, gint index)
{
  debug_print("get_column_type %p %d\n",tree_model,index);
  g_return_val_if_fail (IS_CUSTOM_MODEL (tree_model), G_TYPE_INVALID);
  { Custom_model *custom_model = (Custom_model *) tree_model;
    value callback_object = custom_model->callback_object;

  ACCESS_PUBLIC_METHOD(method,callback_object,"custom_get_column_type",
  { value t = callback2(method,callback_object, Val_int(index));
    return GType_val(t);})}
}

static gboolean
custom_model_get_iter (GtkTreeModel *tree_model,
                      GtkTreeIter  *iter,
                      GtkTreePath  *path)
{
  debug_print("get_iter %p %p %s\n",tree_model,iter,PATH_STRING(path));
  g_return_val_if_fail (iter != NULL, FALSE);
  g_return_val_if_fail (path != NULL, FALSE);
  g_return_val_if_fail (IS_CUSTOM_MODEL (tree_model), FALSE);
  { Custom_model *custom_model = (Custom_model *) tree_model;
    value callback_object = custom_model->callback_object;
    ACCESS_PUBLIC_METHOD(method,callback_object,"custom_get_iter",
  /* This copy is needed because GTK will eventually free the path;
     and Val_GtkTreePath creates a Caml value which frees the path upon
     finalization; don't want to free twice!  The alternative (of
     avoiding both copy and finalization) means trusting the OCaml
     programmer not to store the path somewhere... */
  { UNWRAP_OPTION(res,
		  callback2(method,
			    callback_object,
			    Val_GtkTreePath(gtk_tree_path_copy(path))),
		  if (res) {
		    encode_iter(custom_model,iter,res);
		    return TRUE;
		  }
		  else {
		    return FALSE;)
		  }})}
}

static GtkTreePath *
custom_model_get_path (GtkTreeModel *tree_model,
                      GtkTreeIter  *iter)
{
  debug_print("get_path %p %p:%ld:%ld:%ld\n",tree_model,PRINT4(iter));
  g_return_val_if_fail(iter != NULL, NULL);
  g_return_val_if_fail (IS_CUSTOM_MODEL (tree_model), NULL);
  { Custom_model *custom_model = (Custom_model *) tree_model;
  g_return_val_if_fail (iter->stamp == custom_model->stamp, NULL);
  {value callback_object = custom_model->callback_object;
  ACCESS_PUBLIC_METHOD(method, callback_object,"custom_get_path",

   /* This copy is needed because Caml will eventually free the path from
     the callback when that Caml value is finalized; and GTK will eventually
     free the path we return to it. */
  { value path = callback2(method,callback_object,
			 decode_iter(custom_model,iter));
    return gtk_tree_path_copy(GtkTreePath_val(path));})}}
}

static void
custom_model_get_value (GtkTreeModel *tree_model,
                       GtkTreeIter  *iter,
                       gint          column,
                       GValue       *value_arg)
{
  debug_print("get_value %p %p:%ld:%ld:%ld %d\n",tree_model,PRINT4(iter),column);
  g_return_if_fail(iter != NULL);
  g_return_if_fail (IS_CUSTOM_MODEL (tree_model));
  { Custom_model *custom_model = (Custom_model *) tree_model;
    g_return_if_fail (iter->stamp == custom_model->stamp);
    { value callback_object = custom_model->callback_object;
      value row = decode_iter(custom_model,iter);
      value wrap = Val_GValue_wrap(value_arg);
      ACCESS_PUBLIC_METHOD(method,callback_object,"custom_get_value",
      callback4(method,callback_object,
		row,Val_int(column),wrap);)}}
}

static gboolean
custom_model_iter_next (GtkTreeModel  *tree_model,
                       GtkTreeIter   *iter)
{
  debug_print("iter_next %p %p:%ld:%ld:%ld\n",tree_model,PRINT4(iter));
  g_return_val_if_fail(iter != NULL, FALSE);
  g_return_val_if_fail(IS_CUSTOM_MODEL (tree_model),FALSE);
  { Custom_model *custom_model = (Custom_model *) tree_model;
  g_return_val_if_fail (iter->stamp == custom_model->stamp, FALSE);
  { value callback_object = custom_model->callback_object;
    ACCESS_PUBLIC_METHOD(method,callback_object,"custom_iter_next",
    { value row = decode_iter(custom_model, iter);
      UNWRAP_OPTION(res,callback2(method,callback_object, row),
		    if (res) {
		      encode_iter(custom_model,iter,res);
		      return TRUE;
		    }
		    else {
		      return FALSE;
		    })})}}
}

static gboolean
custom_model_iter_children (GtkTreeModel *tree_model,
                           GtkTreeIter  *iter,
                           GtkTreeIter  *parent)
{
  debug_print("iter_children %p %p %p:%ld:%ld:%ld\n",tree_model,iter,PRINT4(parent));
  g_return_val_if_fail (iter != NULL, FALSE);
  g_return_val_if_fail(IS_CUSTOM_MODEL (tree_model),FALSE);
  { Custom_model *custom_model = (Custom_model *) tree_model;
  g_return_val_if_fail (parent == NULL || parent->stamp == custom_model->stamp, FALSE);
  { value callback_object = custom_model->callback_object;
  ACCESS_PUBLIC_METHOD(method,callback_object,"custom_iter_children",
  { value arg = decode_iter_option(custom_model,parent);
    UNWRAP_OPTION(res, callback2(method,callback_object,arg),
		  if (res) {
		    encode_iter(custom_model,iter,res);
		    return TRUE;
		  }
		  else {
    return FALSE;
		  })})}}
}

static gboolean
custom_model_iter_has_child (GtkTreeModel *tree_model,
                            GtkTreeIter  *iter)
{
  debug_print("iter_has_child %p %p:%ld:%ld:%ld\n",tree_model,PRINT4(iter));
  g_return_val_if_fail (iter != NULL, FALSE);
  g_return_val_if_fail(IS_CUSTOM_MODEL (tree_model),FALSE);
  { Custom_model *custom_model = (Custom_model *) tree_model;
  g_return_val_if_fail (iter->stamp == custom_model->stamp, FALSE);
  { value callback_object = custom_model->callback_object;
  ACCESS_PUBLIC_METHOD(method, callback_object,"custom_iter_has_child",

  { value row = decode_iter(custom_model,iter);
    return Bool_val(callback2(method,callback_object, row));})}}
}

static gint
custom_model_iter_n_children (GtkTreeModel *tree_model,
                             GtkTreeIter  *iter)
{
  debug_print("iter_n_children %p %p:%ld:%ld:%ld\n",tree_model,PRINT4(iter));
  g_return_val_if_fail(IS_CUSTOM_MODEL (tree_model),0);
  { Custom_model *custom_model = (Custom_model *) tree_model;
  g_return_val_if_fail (iter == NULL || iter->stamp == custom_model->stamp, 0);
  { value callback_object = custom_model->callback_object;
  ACCESS_PUBLIC_METHOD(method,callback_object, "custom_iter_n_children",
  { value arg = decode_iter_option(custom_model,iter);
    return Int_val(callback2(method,callback_object, arg));})}}
}

static gboolean
custom_model_iter_nth_child (GtkTreeModel *tree_model,
                            GtkTreeIter  *iter,
                            GtkTreeIter  *parent,
                            gint          n)
{
  debug_print("iter_nth_child %p %p %p:%ld:%ld:%ld %d\n",tree_model,iter,PRINT4(parent),n);
  g_return_val_if_fail(iter != NULL, FALSE);
  g_return_val_if_fail(IS_CUSTOM_MODEL (tree_model),FALSE);
  { Custom_model *custom_model = (Custom_model *) tree_model;
  g_return_val_if_fail (parent == NULL || parent->stamp == custom_model->stamp, FALSE);
  { value callback_object = custom_model->callback_object;
  ACCESS_PUBLIC_METHOD(method, callback_object, "custom_iter_nth_child",

  { value arg = decode_iter_option(custom_model,parent);
    UNWRAP_OPTION(res,callback3(method, callback_object, arg, Val_int(n)),
		  if (res) {
		    encode_iter(custom_model,iter,res);
		    return TRUE;
		  }
		  else {
		    return FALSE;
		  })})}}
}

static gboolean
custom_model_iter_parent (GtkTreeModel *tree_model,
                         GtkTreeIter  *iter,
                         GtkTreeIter  *child)
{
  debug_print("iter_parent %p %p %p:%ld:%ld:%ld\n",tree_model,iter,PRINT4(child));
  g_return_val_if_fail(iter != NULL, FALSE);
  g_return_val_if_fail(IS_CUSTOM_MODEL (tree_model),FALSE);
  { Custom_model *custom_model = (Custom_model *) tree_model;
    g_return_val_if_fail (child != NULL, FALSE);
    g_return_val_if_fail (child->stamp == custom_model->stamp, FALSE);
    { value callback_object = custom_model->callback_object;
      ACCESS_PUBLIC_METHOD(method,callback_object, "custom_iter_parent",
      { value row = decode_iter(custom_model,child);
	UNWRAP_OPTION(res,callback2(method,callback_object,row),
		      if (res) {
			encode_iter(custom_model,iter,res);
			return TRUE;
		      }
		      else {
			return FALSE;
		      })})}}
}

static void
custom_model_ref_node (GtkTreeModel *tree_model, GtkTreeIter *iter)
{
  debug_print("ref_node %p %p:%ld:%ld:%ld\n",tree_model,PRINT4(iter));
  g_return_if_fail(iter != NULL);
  g_return_if_fail (IS_CUSTOM_MODEL (tree_model));
  { Custom_model *custom_model = (Custom_model *) tree_model;
    g_return_if_fail (iter->stamp == custom_model->stamp);
    { value callback_object = custom_model->callback_object;
      ACCESS_PUBLIC_METHOD(method, callback_object, "custom_ref_node",
      { value row = decode_iter(custom_model,iter);
	callback2(method, callback_object, row);})}}
}

static void
custom_model_unref_node (GtkTreeModel *tree_model, GtkTreeIter *iter)
{
  debug_print("unref_node %p %p:%ld:%ld:%ld\n",tree_model,PRINT4(iter));
  g_return_if_fail(iter != NULL);
  g_return_if_fail (IS_CUSTOM_MODEL (tree_model));
  { Custom_model *custom_model = (Custom_model *) tree_model;
    g_return_if_fail (iter->stamp == custom_model->stamp);
    { value callback_object = custom_model->callback_object;
      ACCESS_PUBLIC_METHOD(method, callback_object, "custom_unref_node",
      { value row = decode_iter(custom_model,iter);
	callback2(method, callback_object, row);})}}
}

/*****************************************************************************
 * Creating a new custom model object
 *****************************************************************************/

Custom_model *
custom_model_new (void)
{
  Custom_model *new_custom_model;
  new_custom_model = (Custom_model*) g_object_new (TYPE_CUSTOM_MODEL, NULL);
  g_assert( new_custom_model != NULL );
  return new_custom_model;
}

CAMLprim value ml_custom_model_create(value unit)
{
  Custom_model *new_custom_model = custom_model_new();
  return Val_GObject_new(&new_custom_model->parent);
}

CAMLprim value ml_register_custom_model_callback_object(value custom_model,
							value callback_object)
{
  GObject *obj = GObject_val(custom_model);
  g_return_val_if_fail (IS_CUSTOM_MODEL (obj),Val_unit);
  if(Is_block(callback_object) &&
      (char*)callback_object < (char*)caml_young_end &&
      (char*)callback_object > (char*)caml_young_start)
    {
      caml_register_global_root (&callback_object);
      caml_minor_collection();
      caml_remove_global_root (&callback_object);
    }
  debug_print("register_custom_model_callback_object %p %p\n",obj,(void*)callback_object);
  ((Custom_model *)obj)->callback_object = callback_object;
  return Val_unit;
}

/*****************************************************************************
 * Caml callbacks for signals
 *****************************************************************************/

CAMLprim value ml_custom_model_row_inserted (value tree_model_val, value path, value row)
{
  GtkTreeModel *tree_model = GtkTreeModel_val(tree_model_val);
  g_return_val_if_fail(IS_CUSTOM_MODEL(tree_model), Val_unit);
  { Custom_model *custom_model = (Custom_model *) tree_model;
  GtkTreeIter iter;
  encode_iter (custom_model,&iter,row);
  debug_print("row_inserted %p %s %p:%ld:%ld:%ld\n",custom_model,
              PATH_STRING(GtkTreePath_val(path)),PRINT4_VALID(&iter));
  gtk_tree_model_row_inserted (tree_model,
			       GtkTreePath_val(path),
			       &iter);
  return Val_unit;}
}

CAMLprim value ml_custom_model_row_changed (value tree_model_val, value path, value row)
{
  GtkTreeModel *tree_model = GtkTreeModel_val(tree_model_val);
  g_return_val_if_fail(IS_CUSTOM_MODEL(tree_model), Val_unit);
  { Custom_model *custom_model = (Custom_model *) tree_model;
  GtkTreeIter iter;
  encode_iter (custom_model,&iter,row);
  debug_print("row_changed %p %s %p:%ld:%ld:%ld\n",
              custom_model,PATH_STRING(GtkTreePath_val(path)),PRINT4_VALID(&iter));
  gtk_tree_model_row_changed (tree_model,
			       GtkTreePath_val(path),
			       &iter);
  return Val_unit;}
}

CAMLprim value ml_custom_model_row_has_child_toggled (value tree_model_val, value path, value row)
{
  GtkTreeModel *tree_model = GtkTreeModel_val(tree_model_val);
  g_return_val_if_fail(IS_CUSTOM_MODEL(tree_model), Val_unit);
  { Custom_model *custom_model = (Custom_model *) tree_model;
  GtkTreeIter iter;
  encode_iter (custom_model,&iter,row);
  debug_print("row_has_child_toggled %p %s %p:%ld:%ld:%ld\n",custom_model,
              PATH_STRING(GtkTreePath_val(path)),PRINT4_VALID(&iter));
  gtk_tree_model_row_has_child_toggled (tree_model,
			       GtkTreePath_val(path),
			       &iter);
  return Val_unit;}
}

CAMLprim value ml_custom_model_row_deleted (value tree_model_val, value path)
{
  debug_print("row_deleted %p %s\n",(GtkTreeModel_val(tree_model_val)),PATH_STRING(GtkTreePath_val(path)));
  gtk_tree_model_row_deleted (GtkTreeModel_val(tree_model_val),
			      GtkTreePath_val(path));
  return Val_unit;
}

CAMLprim value ml_custom_model_rows_reordered (value tree_model_val, value path, value row_option, value new_order)
{
  debug_print("rows_reordered\n");
  UNWRAP_OPTION(row, row_option,
  if (row) {
    GtkTreeModel *tree_model = GtkTreeModel_val(tree_model_val);
    g_return_val_if_fail(IS_CUSTOM_MODEL(tree_model), Val_unit);
    { Custom_model *custom_model = (Custom_model *) tree_model;
      GtkTreeIter iter;
    encode_iter(custom_model,&iter,row);
    gtk_tree_model_rows_reordered (tree_model,
				   GtkTreePath_val(path),
				   &iter,
				   (gint*) &Field(new_order,0));
  }}
  else {
    gtk_tree_model_rows_reordered (GtkTreeModel_val(tree_model_val),
				   GtkTreePath_val(path),
				   NULL,
				   (gint*) &Field(new_order,0));
  }
  return Val_unit;)
}

CAMLprim value ml_gtk_tree_view_get_visible_range(value treeview) {
     CAMLparam1(treeview);
     CAMLlocal1(result);
     GtkTreePath *startp, *endp;
     if (! gtk_tree_view_get_visible_range(GtkTreeView_val(treeview),
					   &startp, &endp))
	  CAMLreturn(Val_unit);
     result = alloc_tuple(2);
     Store_field(result, 0, Val_GtkTreePath(startp));
     Store_field(result, 1, Val_GtkTreePath(endp));
     CAMLreturn(ml_some(result));
}
	  
