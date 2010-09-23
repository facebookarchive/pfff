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

/* $Id: ml_gtktext.c 1499 2010-04-08 08:00:42Z garrigue $ */
/* Author: Benjamin Monate */

#include <stdio.h>
#include <string.h>
#include <glib.h>
#include <gtk/gtk.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/callback.h>

#include "wrappers.h"
#include "ml_glib.h"
#include "ml_gdk.h"
#include "ml_gtk.h"
#include "ml_gobject.h"
#include "ml_gdkpixbuf.h"
#include "ml_pango.h"
#include "ml_gtktext.h"
#include "gtk_tags.h"
#include "gdk_tags.h"

/* Init all */

CAMLprim value ml_gtktext_init(value unit)
{
  /* Since these are declared const, must force gcc to call them! */
  GType t =
    gtk_text_view_get_type() +
    gtk_text_buffer_get_type() +
    gtk_text_child_anchor_get_type() +
    gtk_text_mark_get_type() +
    gtk_text_tag_get_type() +
    gtk_text_tag_table_get_type();
    return Val_GType(t);
}

CAMLprim value Val_GtkTextMark_func(gpointer val){
  return(Val_GtkTextMark(val));
}
static value Val_GtkTextMark_opt(GtkTextMark *mrk) {
  return Val_option(mrk, Val_GtkTextMark);
}

/* TextIter are not GObjects. They are stack allocated. */
/* This is the Custom_block version for latter...
static void text_iter_free (value v)
{
  gtk_text_iter_free((GtkTextIter*)v);
}
static struct custom_operations textiter_custom_operations =
  {"gtk_textiter/2.0/",text_iter_free,custom_compare_default,
   custom_hash_default,custom_serialize_default,custom_deserialize_default}
;
#define GtkTextIter_val(val) ((GtkTextIter*)Data_custom_val(val))

CAMLprim value Val_GtkTextIter_new(GtkTextIter* val){
  value res = alloc_custom(&textiter_custom_operations,1,1,2);
  Field(res,1)=(value)gtk_text_iter_copy(val);
  return(res);
}
*/

/* This is the classical version for lablgtk */
/*
#define GtkTextIter_val(val) ((GtkTextIter*)Pointer_val(val))
Make_Val_final_pointer_ext(GtkTextIter, _mine,Ignore,gtk_text_iter_free,1)
CAMLprim value Val_GtkTextIter(GtkTextIter* it){
  return(Val_GtkTextIter_mine(gtk_text_iter_copy(it))); 
}

ML_1 (gtk_text_iter_copy, GtkTextIter_val, Val_GtkTextIter_mine)
*/


CAMLprim value ml_gtk_text_iter_copy (value it) {
  /* Only valid if in old generation and compaction off */
  return Val_GtkTextIter(GtkTextIter_val(it));
}

/* gtktextmark */

ML_2(gtk_text_mark_set_visible, GtkTextMark_val, Bool_val, Unit)
ML_1(gtk_text_mark_get_visible, GtkTextMark_val, Val_bool)
ML_1(gtk_text_mark_get_deleted, GtkTextMark_val, Val_bool)
CAMLprim value ml_gtk_text_mark_get_name (value tm)
{
  CAMLparam1(tm);
  CAMLlocal1(res);
  const gchar* tmp;
  tmp = gtk_text_mark_get_name(GtkTextMark_val(tm));
  res = Val_option(tmp,Val_string);
  CAMLreturn(res);
}
CAMLprim value ml_gtk_text_mark_get_buffer (value tm)
{
  CAMLparam1(tm);
  CAMLlocal1(res);
  GtkTextBuffer* tmp;
  tmp = gtk_text_mark_get_buffer(GtkTextMark_val(tm));
  res = Val_option(tmp,Val_GtkTextBuffer);
  CAMLreturn(res);
}

ML_1(gtk_text_mark_get_left_gravity, GtkTextMark_val, Val_bool)

/* gtktexttag */

ML_1(gtk_text_tag_new, String_val, Val_GtkTextTag_new)
ML_1(gtk_text_tag_get_priority, GtkTextTag_val, Val_int)
ML_2(gtk_text_tag_set_priority, GtkTextTag_val, Int_val, Unit)
ML_4(gtk_text_tag_event, GtkTextTag_val, GObject_val, GdkEvent_val, 
     GtkTextIter_val, Val_bool)
/* export needed conversion */
ML_1(Wrap_mode_val, (value), Val_int)

/* gtktexttagtable */

ML_0(gtk_text_tag_table_new, Val_GtkTextTagTable_new)
ML_2(gtk_text_tag_table_add, GtkTextTagTable_val, GtkTextTag_val,Unit)
ML_2(gtk_text_tag_table_remove, GtkTextTagTable_val, GtkTextTag_val,Unit)

CAMLprim value ml_gtk_text_tag_table_lookup (value tv, value s)
{
  CAMLparam2(tv,s);
  CAMLlocal1(res);
  GtkTextTag* tmp;
  tmp = gtk_text_tag_table_lookup(GtkTextTagTable_val(tv), String_val(s));
  res = Val_option(tmp,Val_GtkTextTag);
  CAMLreturn(res);
}

ML_1(gtk_text_tag_table_get_size, GtkTextTagTable_val, Val_int)

static void tag_foreach_func (GtkTextTag* t, gpointer user_data)
{
  value arg = Val_GtkTextTag(t);
  callback_exn (*(value*)user_data, arg);
}

CAMLprim value ml_gtk_text_tag_table_foreach (value t, value fun)
{
  CAMLparam1(fun);
  gtk_text_tag_table_foreach(GtkTextTagTable_val(t), tag_foreach_func, &fun);
  CAMLreturn(Val_unit);
}

/* gtktextbuffer */

ML_1 (gtk_text_buffer_new,
      Option_val(arg1,GtkTextTagTable_val,NULL) Ignore, Val_GtkTextBuffer_new)

ML_1 (gtk_text_buffer_get_line_count,GtkTextBuffer_val,Val_int)

ML_1 (gtk_text_buffer_get_char_count,GtkTextBuffer_val,Val_int)

/* ML_1 (gtk_text_buffer_get_tag_table,GtkTextBuffer_val,Val_GtkTextTagTable)
*/


/* [Benjamin] 
   WARNING : something strange happens here : various segfaults 
   we we insert non constant string a lot of times and the signal "insert-text"
   is connected. See : 

let bug () = 
  let w = GWindow.window  ~title:"Insertion bug"  () in
  let b = GText.buffer () in
  b#set_text  "Initial text\n";
  GText.view ~buffer:b ~packing:(w#add) ();
  w#show ();
  b#connect#insert_text (fun it s -> Gc.full_major ();
                        Printf.printf "Handler got: \"%s\"\n" s;
			flush stdout);
  b#connect#delete_range (fun ~start ~stop -> Gc.full_major ());
  let s = "azert"^"yuiop" in
  let iter_ref = ref b#start_iter in
  for i = 0 to 10 do 
    let start = !iter_ref#offset in
    Printf.printf "Number %d, \"%s\", %d\n" i s start;
    flush stdout;
    let iter = !iter_ref#copy in
    b#insert ~iter s;
    let iter' = iter#copy in
    b#delete (b#get_iter (`OFFSET(start+2))) iter';
    iter_ref := iter'
  done
;;

   The GC seems to free/move the string too early...

   An explicite allocation of the string seems to fix it.
   Jacques : any idea of what is happening ?

Update: This has probably something to do with garbage at the end of the caml 
string.

[Jacques] No, I think the first answer is right: the GC is moving the string.
Caml string are correctly 0-terminated, so this is not the cause.
By the way, I had problems with "light" textiters for the same reason.

Now the above code is OK, but replacing [full_major] by [compact] it will fail,
as will do most code... Disabling compaction is essential.

Note that I also allocate stable strings in the old generation now, to avoid
problems with alloca on Linux.

*/

ML_3 (gtk_text_buffer_insert, GtkTextBuffer_val,
      GtkTextIter_val, SizedString_val, Unit)

ML_2 (gtk_text_buffer_insert_at_cursor, GtkTextBuffer_val,
      SizedString_val, Unit)

ML_4 (gtk_text_buffer_insert_interactive,GtkTextBuffer_val,
      GtkTextIter_val, SizedString_val, Bool_val, Val_bool)

ML_3 (gtk_text_buffer_insert_interactive_at_cursor,GtkTextBuffer_val,
      SizedString_val, Bool_val, Val_bool)

ML_4 (gtk_text_buffer_insert_range,GtkTextBuffer_val,
      GtkTextIter_val, GtkTextIter_val,GtkTextIter_val,Unit)

ML_5 (gtk_text_buffer_insert_range_interactive,GtkTextBuffer_val,
      GtkTextIter_val, GtkTextIter_val,GtkTextIter_val,Bool_val,Val_bool)

ML_3 (gtk_text_buffer_delete,GtkTextBuffer_val,
      GtkTextIter_val, GtkTextIter_val,Unit)

ML_4 (gtk_text_buffer_delete_interactive,GtkTextBuffer_val,
      GtkTextIter_val, GtkTextIter_val,Bool_val,Val_bool)

ML_2 (gtk_text_buffer_set_text, GtkTextBuffer_val, SizedString_val, Unit)

ML_4 (gtk_text_buffer_get_text, GtkTextBuffer_val, 
      GtkTextIter_val,GtkTextIter_val,Bool_val, copy_string_g_free)

ML_4 (gtk_text_buffer_get_slice, GtkTextBuffer_val, 
      GtkTextIter_val,GtkTextIter_val,Bool_val, copy_string_g_free)

ML_3 (gtk_text_buffer_insert_pixbuf, GtkTextBuffer_val, 
      GtkTextIter_val,GdkPixbuf_val,Unit)

ML_4 (gtk_text_buffer_create_mark, GtkTextBuffer_val, 
      String_option_val, GtkTextIter_val, Bool_val, Val_GtkTextMark)

ML_2 (gtk_text_buffer_get_mark, GtkTextBuffer_val, 
      String_val, Val_GtkTextMark_opt)

ML_1 (gtk_text_buffer_get_insert, GtkTextBuffer_val, Val_GtkTextMark)

ML_1 (gtk_text_buffer_get_selection_bound, GtkTextBuffer_val, Val_GtkTextMark)

ML_3(gtk_text_buffer_move_mark, GtkTextBuffer_val, GtkTextMark_val, 
     GtkTextIter_val, Unit)

ML_3(gtk_text_buffer_move_mark_by_name, GtkTextBuffer_val, String_val, 
     GtkTextIter_val, Unit)

ML_2 (gtk_text_buffer_delete_mark, GtkTextBuffer_val, 
      GtkTextMark_val,Unit)

ML_2 (gtk_text_buffer_delete_mark_by_name, GtkTextBuffer_val, 
      String_val, Unit)

ML_2 (gtk_text_buffer_place_cursor, GtkTextBuffer_val, 
      GtkTextIter_val, Unit)

#ifdef HASGTK24
ML_3 (gtk_text_buffer_select_range, GtkTextBuffer_val, 
      GtkTextIter_val, GtkTextIter_val, Unit)
#else
Unsupported_24(gtk_text_buffer_select_range)
#endif

ML_4 (gtk_text_buffer_apply_tag, GtkTextBuffer_val, 
      GtkTextTag_val, GtkTextIter_val, GtkTextIter_val, Unit)

ML_4 (gtk_text_buffer_remove_tag, GtkTextBuffer_val, 
      GtkTextTag_val, GtkTextIter_val, GtkTextIter_val, Unit)

ML_4 (gtk_text_buffer_apply_tag_by_name, GtkTextBuffer_val, 
      String_val, GtkTextIter_val, GtkTextIter_val, Unit)

ML_4 (gtk_text_buffer_remove_tag_by_name, GtkTextBuffer_val, 
      String_val, GtkTextIter_val, GtkTextIter_val, Unit)

ML_3 (gtk_text_buffer_remove_all_tags, GtkTextBuffer_val, 
      GtkTextIter_val, GtkTextIter_val, Unit)

ML_2_name (ml_gtk_text_buffer_create_tag_0,gtk_text_buffer_create_tag,
	   GtkTextBuffer_val, 
	   Split(Option_val(arg2,String_val,NULL),
		 Id,
		 NULL Ignore),
	   Val_GtkTextTag)

CAMLprim value  ml_gtk_text_buffer_create_tag_1
(value arg1, value arg2, value arg3) 
{ return
    (Val_GtkTextTag
     (gtk_text_buffer_create_tag
      (GtkTextBuffer_val(arg1),Option_val(arg2,String_val,NULL),
       String_val(arg3),NULL)));};

CAMLprim value  ml_gtk_text_buffer_create_tag_2
(value arg1, value arg2, value arg3, value arg4) 
{ return
    (Val_GtkTextTag
     (gtk_text_buffer_create_tag
      (GtkTextBuffer_val(arg1),Option_val(arg2,String_val,NULL),
       String_val(arg3),String_val(arg4),NULL)));};


CAMLprim value ml_gtk_text_buffer_get_iter_at_line_offset(value tb, 
							  value l,
							  value c)
{
  CAMLparam3(tb,l,c);
  GtkTextIter res;
  gtk_text_buffer_get_iter_at_line_offset(GtkTextBuffer_val(tb),
					  &res,
					  Int_val(l),
					  Int_val(c));
  CAMLreturn(Val_GtkTextIter(&res));
}

CAMLprim value ml_gtk_text_buffer_get_iter_at_offset(value tb, value l)
{
  CAMLparam2(tb,l);
  GtkTextIter res;
  gtk_text_buffer_get_iter_at_offset(GtkTextBuffer_val(tb),
				     &res,
				     Int_val(l));
  CAMLreturn(Val_GtkTextIter(&res));
}

CAMLprim value ml_gtk_text_buffer_get_iter_at_line(value tb, value l)
{
  CAMLparam2(tb,l);
  GtkTextIter res;
  gtk_text_buffer_get_iter_at_line(GtkTextBuffer_val(tb),
				   &res,
				   Int_val(l));
  CAMLreturn(Val_GtkTextIter(&res));
}

CAMLprim value ml_gtk_text_buffer_get_iter_at_line_index(value tb, 
							 value l,
							 value c)
{
  CAMLparam3(tb,l,c);
  GtkTextIter res;
  gtk_text_buffer_get_iter_at_line_offset(GtkTextBuffer_val(tb),
					  &res,
					  Int_val(l),
					  Int_val(c));
  CAMLreturn(Val_GtkTextIter(&res));
}


CAMLprim value ml_gtk_text_buffer_get_iter_at_mark(value tb, value l)
{
  CAMLparam2(tb,l);
  GtkTextIter res;
  gtk_text_buffer_get_iter_at_mark(GtkTextBuffer_val(tb),
				   &res,
				   GtkTextMark_val(l));
  CAMLreturn(Val_GtkTextIter(&res));
}

CAMLprim value ml_gtk_text_buffer_get_start_iter(value tb)
{
  CAMLparam1(tb);
  GtkTextIter res;
  gtk_text_buffer_get_start_iter(GtkTextBuffer_val(tb), &res);
  CAMLreturn(Val_GtkTextIter(&res));
}

CAMLprim value ml_gtk_text_buffer_get_end_iter(value tb)
{
  CAMLparam1(tb);
  GtkTextIter res;
  gtk_text_buffer_get_end_iter(GtkTextBuffer_val(tb), &res);
  CAMLreturn(Val_GtkTextIter(&res));
}

CAMLprim value ml_gtk_text_buffer_get_bounds(value tb)
{
  CAMLparam1(tb);
  CAMLlocal1(res);
  GtkTextIter res1,res2;
  gtk_text_buffer_get_bounds(GtkTextBuffer_val(tb), &res1, &res2);

  res = alloc_tuple(2);
  Store_field(res,0,Val_GtkTextIter(&res1));
  Store_field(res,1,Val_GtkTextIter(&res2));

  CAMLreturn(res);
}

ML_1 (gtk_text_buffer_get_modified, GtkTextBuffer_val, Val_bool)

ML_2 (gtk_text_buffer_set_modified, GtkTextBuffer_val, Bool_val, Unit)

ML_3 (gtk_text_buffer_delete_selection, GtkTextBuffer_val, 
      Bool_val, Bool_val, Val_bool)

CAMLprim value ml_gtk_text_buffer_get_selection_bounds(value tb)
{
  CAMLparam1(tb);
  CAMLlocal1(res);
  GtkTextIter res1,res2;
  gtk_text_buffer_get_selection_bounds(GtkTextBuffer_val(tb), &res1, &res2);
  res = alloc_tuple(2);
  Store_field(res,0,Val_GtkTextIter(&res1));
  Store_field(res,1,Val_GtkTextIter(&res2));
  CAMLreturn(res);
}

ML_1(gtk_text_buffer_begin_user_action,GtkTextBuffer_val,Unit)

ML_1(gtk_text_buffer_end_user_action,GtkTextBuffer_val,Unit)

     /* no ref returned to the caller. */
ML_2(gtk_text_buffer_create_child_anchor,
     GtkTextBuffer_val,GtkTextIter_val,Val_GtkTextChildAnchor)

ML_3(gtk_text_buffer_insert_child_anchor,
     GtkTextBuffer_val,GtkTextIter_val,GtkTextChildAnchor_val,Unit)


CAMLprim value ml_gtk_text_buffer_paste_clipboard
     (value arg1, value arg2, value arg3, value arg4) 
{ 
  gtk_text_buffer_paste_clipboard
    (GtkTextBuffer_val(arg1),
     GtkClipboard_val(arg2),
     Option_val(arg3,GtkTextIter_val,NULL),
     Bool_val(arg4)
     );
  return(Val_unit); 
}

ML_2(gtk_text_buffer_copy_clipboard,
     GtkTextBuffer_val,
     GtkClipboard_val,
     Unit)

ML_3(gtk_text_buffer_cut_clipboard,
     GtkTextBuffer_val,
     GtkClipboard_val,
     Bool_val,
     Unit)

ML_2(gtk_text_buffer_add_selection_clipboard,
     GtkTextBuffer_val,
     GtkClipboard_val,
     Unit)

ML_2(gtk_text_buffer_remove_selection_clipboard,
     GtkTextBuffer_val,
     GtkClipboard_val,
     Unit)

/* gtktextview.h */

ML_1 (Val_delete_type, Int_val, (value))
ML_1 (Val_movement_step, Int_val, (value))

ML_0 (gtk_text_view_new, Val_GtkWidget_sink)

ML_1 (gtk_text_view_new_with_buffer, GtkTextBuffer_val, Val_GtkWidget_sink)

ML_2 (gtk_text_view_set_buffer, GtkTextView_val, GtkTextBuffer_val, Unit)
ML_1 (gtk_text_view_get_buffer, GtkTextView_val, Val_GtkTextBuffer)

ML_6(gtk_text_view_scroll_to_mark, GtkTextView_val, GtkTextMark_val,
     Float_val, Bool_val, Float_val,Float_val, Unit)
ML_bc6(ml_gtk_text_view_scroll_to_mark)

ML_6(gtk_text_view_scroll_to_iter, GtkTextView_val, GtkTextIter_val,
     Float_val, Bool_val, Float_val,Float_val, Val_bool)
ML_bc6(ml_gtk_text_view_scroll_to_iter)

ML_2(gtk_text_view_scroll_mark_onscreen, GtkTextView_val, GtkTextMark_val,Unit)

ML_2(gtk_text_view_move_mark_onscreen, GtkTextView_val, GtkTextMark_val,
     Val_bool)

ML_1(gtk_text_view_place_cursor_onscreen, GtkTextView_val, Val_bool)

CAMLprim value ml_gtk_text_view_get_visible_rect (value tv)
{
    GdkRectangle res;
    gtk_text_view_get_visible_rect(GtkTextView_val(tv), &res);
    return Val_copy(res);
}

CAMLprim value ml_gtk_text_view_get_iter_location (value tv, value ti)
{
    GdkRectangle res;
    gtk_text_view_get_iter_location(GtkTextView_val(tv),GtkTextIter_val(ti),
				    &res);
    return Val_copy(res);
}

CAMLprim value ml_gtk_text_view_get_line_at_y (value tv, value y)
{
  CAMLparam2(tv,y);
  CAMLlocal1(res);
  GtkTextIter res1;
  int res2;
  gtk_text_view_get_line_at_y(GtkTextView_val(tv),&res1,
				    Int_val(y),&res2);
  res = alloc_tuple(2);
  Store_field(res,0,Val_GtkTextIter(&res1));
  Store_field(res,1,Val_int(res2));

  CAMLreturn(res);
}


CAMLprim value ml_gtk_text_view_get_line_yrange (value tv, value ti)
{
  CAMLparam2(tv,ti);
  CAMLlocal1(res);
  int y,h;
  
  gtk_text_view_get_line_yrange(GtkTextView_val(tv),
				GtkTextIter_val(ti),
				&y,&h);
  res = alloc_tuple(2);
  Store_field(res,0,Val_int(y));
  Store_field(res,1,Val_int(h));
  CAMLreturn(res);
}

CAMLprim value ml_gtk_text_view_get_iter_at_location (value tv, 
						      value x,
						      value y)
{
  CAMLparam3(tv,x,y);
  GtkTextIter res;
  gtk_text_view_get_iter_at_location(GtkTextView_val(tv),&res,
				    Int_val(x),Int_val(y));
  CAMLreturn(Val_GtkTextIter(&res));
}

CAMLprim value ml_gtk_text_view_buffer_to_window_coords (value tv, 
							 value tt,
							 value x,
							 value y)
{
  CAMLparam4(tv,tt,x,y);
  CAMLlocal1(res);
  int bx,by = 0;

  gtk_text_view_buffer_to_window_coords(GtkTextView_val(tv),
					(GtkTextWindowType)tt,
					Int_val(x),Int_val(y),
					&bx,&by);

  res = alloc_tuple(2);
  Store_field(res,0,Val_int(bx));
  Store_field(res,1,Val_int(by));
  CAMLreturn(res);
}

CAMLprim value ml_gtk_text_view_window_to_buffer_coords (value tv, 
							 value tt,
							 value x,
							 value y)
{
  CAMLparam4(tv,tt,x,y);
  CAMLlocal1(res);
  int bx,by = 0;
  gtk_text_view_window_to_buffer_coords(GtkTextView_val(tv),
					Text_window_type_val(tt),
					Int_val(x),Int_val(y),
					&bx,&by);

  res = alloc_tuple(2);
  Store_field(res,0,Val_int(bx));
  Store_field(res,1,Val_int(by));
  CAMLreturn(res);
}

CAMLprim value ml_gtk_text_view_get_window (value tv, value tt)
{
  CAMLparam2(tv,tt);
  CAMLlocal1(res);
  GdkWindow* tmp;
  tmp = gtk_text_view_get_window(GtkTextView_val(tv), Text_window_type_val(tt));
  res = Val_option(tmp,Val_GdkWindow);
  CAMLreturn(res);
}

ML_2(gtk_text_view_get_window_type,GtkTextView_val,GdkWindow_val,
     Val_text_window_type)

ML_3(gtk_text_view_set_border_window_size,GtkTextView_val,
     Text_window_type_val,Int_val, Unit)

ML_2(gtk_text_view_get_border_window_size,GtkTextView_val,
     Text_window_type_val,Val_int)

ML_2(gtk_text_view_forward_display_line,GtkTextView_val,
     GtkTextIter_val,Val_bool)

ML_2(gtk_text_view_backward_display_line,GtkTextView_val,
     GtkTextIter_val,Val_bool)

ML_2(gtk_text_view_forward_display_line_end,GtkTextView_val,
     GtkTextIter_val,Val_bool)

ML_2(gtk_text_view_backward_display_line_start,GtkTextView_val,
     GtkTextIter_val,Val_bool)

ML_2(gtk_text_view_starts_display_line,GtkTextView_val,
     GtkTextIter_val,Val_bool)


ML_3(gtk_text_view_move_visually,GtkTextView_val,
     GtkTextIter_val,Int_val,Val_bool)

ML_3(gtk_text_view_add_child_at_anchor,GtkTextView_val,
     GtkWidget_val,GtkTextChildAnchor_val,Unit)

ML_0(gtk_text_child_anchor_new,Val_GtkTextChildAnchor_new)

CAMLprim value ml_gtk_text_child_anchor_get_widgets (value tca) {
  return Val_GList_free
    (gtk_text_child_anchor_get_widgets(GtkTextChildAnchor_val(tca)),
     Val_GtkWidget_func);
}

ML_1(gtk_text_child_anchor_get_deleted,GtkTextChildAnchor_val,Val_bool)

ML_5(gtk_text_view_add_child_in_window,GtkTextView_val,
     GtkWidget_val,Text_window_type_val,Int_val,Int_val,
     Unit)

ML_4(gtk_text_view_move_child,GtkTextView_val,
     GtkWidget_val,Int_val,Int_val,
     Unit)

/* gtktextiter */

ML_1 (gtk_text_iter_get_buffer, GtkTextIter_val, Val_GtkTextBuffer)
ML_1 (gtk_text_iter_get_offset, GtkTextIter_val, Val_int)
ML_1 (gtk_text_iter_get_line, GtkTextIter_val, Val_int)
ML_1 (gtk_text_iter_get_line_offset, GtkTextIter_val, Val_int)
ML_1 (gtk_text_iter_get_line_index, GtkTextIter_val, Val_int)
ML_1 (gtk_text_iter_get_visible_line_index, GtkTextIter_val, Val_int)
ML_1 (gtk_text_iter_get_visible_line_offset, GtkTextIter_val, Val_int)

ML_1 (gtk_text_iter_get_char, GtkTextIter_val, Val_int)

ML_2 (gtk_text_iter_get_slice, GtkTextIter_val, GtkTextIter_val,
      copy_string_g_free)
ML_2 (gtk_text_iter_get_text, GtkTextIter_val, GtkTextIter_val,
      copy_string_g_free)

ML_2 (gtk_text_iter_get_visible_slice, GtkTextIter_val,
      GtkTextIter_val, copy_string_g_free)
ML_2 (gtk_text_iter_get_visible_text, GtkTextIter_val,
      GtkTextIter_val, copy_string_g_free)

CAMLprim value ml_gtk_text_iter_get_pixbuf(value ti)
{
  GdkPixbuf *ret = gtk_text_iter_get_pixbuf(GtkTextIter_val(ti));
  return Val_option(ret,Val_GdkPixbuf);
}

CAMLprim value ml_gtk_text_iter_get_marks(value ti) {
  return Val_GSList_free(gtk_text_iter_get_marks(GtkTextIter_val(ti)),
                         Val_GtkTextMark_func);
}

CAMLprim value ml_gtk_text_iter_get_toggled_tags(value ti, value b) {
  return Val_GSList_free
    (gtk_text_iter_get_toggled_tags(GtkTextIter_val(ti), Bool_val(b)),
     Val_GtkTextMark_func);
}

CAMLprim value ml_gtk_text_iter_get_child_anchor(value ti)
{
  GtkTextChildAnchor *ret =
    gtk_text_iter_get_child_anchor(GtkTextIter_val(ti));
  return Val_option(ret,Val_GtkTextChildAnchor);
}

ML_2 (gtk_text_iter_begins_tag,GtkTextIter_val,
      Option_val(arg2,GtkTextTag_val,NULL) Ignore, Val_bool)

ML_2 (gtk_text_iter_ends_tag,GtkTextIter_val,
      Option_val(arg2,GtkTextTag_val,NULL) Ignore, Val_bool)

ML_2 (gtk_text_iter_toggles_tag,GtkTextIter_val,
      Option_val(arg2,GtkTextTag_val,NULL) Ignore, Val_bool)

ML_2 (gtk_text_iter_has_tag,GtkTextIter_val,
      GtkTextTag_val, Val_bool)

CAMLprim value ml_gtk_text_iter_get_tags(value ti) {
  return Val_GSList_free(gtk_text_iter_get_tags(GtkTextIter_val(ti)),
                         Val_GtkTextMark_func);
}

ML_2 (gtk_text_iter_editable,GtkTextIter_val,
      Bool_val, Val_bool)

ML_2 (gtk_text_iter_can_insert,GtkTextIter_val,
      Bool_val, Val_bool)

ML_1 (gtk_text_iter_starts_word, GtkTextIter_val, Val_bool)

ML_1 (gtk_text_iter_ends_word, GtkTextIter_val, Val_bool)

ML_1 (gtk_text_iter_inside_word,GtkTextIter_val, Val_bool)

ML_1 (gtk_text_iter_starts_line,GtkTextIter_val, Val_bool)

ML_1 (gtk_text_iter_ends_line,GtkTextIter_val, Val_bool)

ML_1 (gtk_text_iter_starts_sentence,GtkTextIter_val, Val_bool)

ML_1 (gtk_text_iter_ends_sentence,GtkTextIter_val, Val_bool)

ML_1 (gtk_text_iter_inside_sentence,GtkTextIter_val, Val_bool)

ML_1 (gtk_text_iter_is_cursor_position,GtkTextIter_val, Val_bool)

ML_1 (gtk_text_iter_get_chars_in_line, GtkTextIter_val, Val_int)

ML_1 (gtk_text_iter_get_bytes_in_line, GtkTextIter_val, Val_int)

ML_1 (gtk_text_iter_get_language, GtkTextIter_val, Val_PangoLanguage)

ML_1 (gtk_text_iter_is_end,GtkTextIter_val, Val_bool)

ML_1 (gtk_text_iter_is_start,GtkTextIter_val, Val_bool)

ML_1 (gtk_text_iter_forward_char,GtkTextIter_val, Val_bool)

ML_1 (gtk_text_iter_backward_char,GtkTextIter_val, Val_bool)

ML_2 (gtk_text_iter_forward_chars,GtkTextIter_val, Int_val, Val_bool)

ML_2 (gtk_text_iter_backward_chars,GtkTextIter_val, Int_val, Val_bool)

ML_1 (gtk_text_iter_forward_line,GtkTextIter_val, Val_bool)

ML_1 (gtk_text_iter_backward_line,GtkTextIter_val, Val_bool)

ML_2 (gtk_text_iter_forward_lines,GtkTextIter_val, Int_val, Val_bool)

ML_2 (gtk_text_iter_backward_lines,GtkTextIter_val, Int_val, Val_bool)

ML_1 (gtk_text_iter_forward_word_end,GtkTextIter_val, Val_bool)

ML_2 (gtk_text_iter_forward_word_ends,GtkTextIter_val, Int_val, Val_bool)

ML_1 (gtk_text_iter_backward_word_start,GtkTextIter_val, Val_bool)

ML_2 (gtk_text_iter_backward_word_starts,GtkTextIter_val, Int_val, Val_bool)

ML_1 (gtk_text_iter_forward_cursor_position,GtkTextIter_val, Val_bool)

ML_1 (gtk_text_iter_backward_cursor_position,GtkTextIter_val, Val_bool)

ML_2 (gtk_text_iter_forward_cursor_positions, GtkTextIter_val,
      Int_val, Val_bool)

ML_2 (gtk_text_iter_backward_cursor_positions, GtkTextIter_val, 
      Int_val, Val_bool)

ML_1 (gtk_text_iter_forward_sentence_end, GtkTextIter_val, Val_bool)

ML_1 (gtk_text_iter_backward_sentence_start, GtkTextIter_val, Val_bool)

ML_2 (gtk_text_iter_forward_sentence_ends, GtkTextIter_val,
      Int_val, Val_bool)

ML_2 (gtk_text_iter_backward_sentence_starts, GtkTextIter_val,
      Int_val, Val_bool)

ML_2 (gtk_text_iter_set_offset, GtkTextIter_val, Int_val, Unit)
ML_2 (gtk_text_iter_set_line, GtkTextIter_val, Int_val, Unit)
ML_2 (gtk_text_iter_set_line_offset, GtkTextIter_val, Int_val, Unit)
ML_2 (gtk_text_iter_set_line_index, GtkTextIter_val, Int_val, Unit)
ML_2 (gtk_text_iter_set_visible_line_index, GtkTextIter_val, Int_val, Unit)
ML_2 (gtk_text_iter_set_visible_line_offset, GtkTextIter_val, Int_val, Unit)


ML_1 (gtk_text_iter_forward_to_end, GtkTextIter_val, Unit)
ML_1 (gtk_text_iter_forward_to_line_end, GtkTextIter_val, Val_bool)
ML_2 (gtk_text_iter_forward_to_tag_toggle, GtkTextIter_val, 
      Option_val(arg2,GtkTextTag_val,NULL) Ignore,
      Val_bool)
ML_2 (gtk_text_iter_backward_to_tag_toggle, GtkTextIter_val, 
      Option_val(arg2,GtkTextTag_val,NULL) Ignore,
      Val_bool)
     

ML_2 (gtk_text_iter_equal, GtkTextIter_val, GtkTextIter_val, Val_bool)
ML_2 (gtk_text_iter_compare, GtkTextIter_val, GtkTextIter_val, Val_int)
ML_3 (gtk_text_iter_in_range, GtkTextIter_val, GtkTextIter_val,
      GtkTextIter_val, Val_bool)
ML_2 (gtk_text_iter_order, GtkTextIter_val, GtkTextIter_val, Unit)

Make_OptFlags_val(Text_search_flag_val)

#define Make_search(dir) \
CAMLprim value ml_gtk_text_iter_##dir##_search (value ti_start, \
						value str,\
						value flag,\
						value ti_lim)\
{ CAMLparam4(ti_start,str,flag,ti_lim);\
  CAMLlocal2(res,coup);\
  GtkTextIter* ti1,*ti2;\
  gboolean b;\
  ti1=gtk_text_iter_copy(GtkTextIter_val(ti_start));\
  ti2=gtk_text_iter_copy(GtkTextIter_val(ti_start));\
  b=gtk_text_iter_##dir##_search(GtkTextIter_val(ti_start),\
				 String_val(str),\
				 OptFlags_Text_search_flag_val(flag),\
				 ti1,\
				 ti2,\
				 Option_val(ti_lim,GtkTextIter_val,NULL));\
  if (!b) res = Val_unit;\
  else \
    { res = alloc(1,0);\
      coup = alloc_tuple(2);\
      Store_field(coup,0,Val_GtkTextIter(ti1));\
      Store_field(coup,1,Val_GtkTextIter(ti2));\
      Store_field(res,0,coup);};\
  CAMLreturn(res);}

Make_search(forward);
Make_search(backward);

static gboolean ml_gtk_text_char_predicate(gunichar ch, gpointer user_data)
{
  value res, *clos = user_data;
  res = callback_exn (*clos, Val_int(ch));
  if (Is_exception_result (res)) {
    CAML_EXN_LOG ("ml_gtk_text_char_predicate");
    return FALSE;
  }
  return Bool_val(res);
}

CAMLprim value ml_gtk_text_iter_forward_find_char(value i,value fun,value ito)
{
  CAMLparam1(fun);
  CAMLreturn
    (Val_bool
     (gtk_text_iter_forward_find_char(GtkTextIter_val(i),
                                      ml_gtk_text_char_predicate,
                                      &fun,
                                      Option_val(ito,GtkTextIter_val,NULL))));
}
     
CAMLprim value ml_gtk_text_iter_backward_find_char(value i,value fun,value ito)
{
  CAMLparam1(fun);
  CAMLreturn
    (Val_bool
     (gtk_text_iter_backward_find_char(GtkTextIter_val(i),
                                       ml_gtk_text_char_predicate,
                                       &fun,
                                       Option_val(ito,GtkTextIter_val,NULL))));
}
