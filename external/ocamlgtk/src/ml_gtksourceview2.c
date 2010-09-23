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

#include <assert.h>
#include <gtksourceview/gtksourceview.h>
#include <gtksourceview/gtksourcelanguagemanager.h>
#include <gtksourceview/gtksourceiter.h>
#include <gtksourceview/gtksourcestylescheme.h>
#include <gtksourceview/gtksourcestyleschememanager.h>

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
#include "sourceView2_tags.h"
#include <string.h>
#include "sourceView2_tags.c"

Make_OptFlags_val(Source_search_flag_val)

CAMLprim value ml_gtk_source_style_scheme_init(value unit)
{	/* Since these are declared const, must force gcc to call them! */
    GType t = gtk_source_style_scheme_get_type();
    return Val_GType(t);
}

CAMLprim value ml_gtk_source_style_scheme_manager_init(value unit)
{	/* Since these are declared const, must force gcc to call them! */
    GType t = gtk_source_style_scheme_manager_get_type();
    return Val_GType(t);
}

CAMLprim value ml_gtk_source_language_init(value unit)
{	/* Since these are declared const, must force gcc to call them! */
    GType t = gtk_source_language_get_type();
    return Val_GType(t);
}

CAMLprim value ml_gtk_source_language_manager_init(value unit)
{	/* Since these are declared const, must force gcc to call them! */
    GType t =
      gtk_source_language_manager_get_type();
    return Val_GType(t);
}

CAMLprim value ml_gtk_source_buffer_init(value unit)
{	/* Since these are declared const, must force gcc to call them! */
    GType t = gtk_source_buffer_get_type();
    return Val_GType(t);
}

CAMLprim value ml_gtk_source_view_init(value unit)
{	/* Since these are declared const, must force gcc to call them! */
    GType t = gtk_source_view_get_type();
    return Val_GType(t);
}

static gpointer string_val(value v)
{
	return String_val(v);
}

GSList *ml_gslist_of_string_list(value list)
{
	return GSList_val(list, string_val);
}

#define GtkSourceStyleScheme_val(val) check_cast(GTK_SOURCE_STYLE_SCHEME,val)
#define Val_GtkSourceStyleScheme(val) (Val_GObject((GObject*)val))
#define Val_GtkSourceStyleScheme_new(val) (Val_GObject_new((GObject*)val))
#define Val_option_GtkSourceStyleScheme(val) \
     Val_option(val, Val_GtkSourceStyleScheme)

#define GtkSourceStyleSchemeManager_val(val) \
     check_cast(GTK_SOURCE_STYLE_SCHEME_MANAGER,val)
#define Val_GtkSourceStyleSchemeManager(val) (Val_GObject((GObject*)val))

#define Val_GtkSourceLanguage(val)  (Val_GObject((GObject*)val))
#define Val_option_GtkSourceLanguage(val) Val_option(val,Val_GtkSourceLanguage)

#define GtkSourceLanguage_val(val) check_cast(GTK_SOURCE_LANGUAGE,val)
#define GtkSourceLanguageManager_val(val)\
	check_cast(GTK_SOURCE_LANGUAGE_MANAGER,val)
#define Val_GtkSourceLanguageManager(val)  (Val_GObject((GObject*)val))

#define GtkSourceTagStyle_val(val) Pointer_val(val)

#define GtkSourceMark_val(val) check_cast(GTK_SOURCE_MARK,val)
#define Val_GtkSourceMark(val)  (Val_GObject((GObject*)val))
#define Val_GtkSourceMark_new(val) (Val_GObject_new((GObject*)val))
#define Val_option_GtkSourceMark(val) Val_option(val,Val_GtkSourceMark)

#define GtkSourceBuffer_val(val) check_cast(GTK_SOURCE_BUFFER,val)
#define Val_GtkSourceBuffer(val) (Val_GObject((GObject*)val))
#define Val_GtkSourceBuffer_new(val) (Val_GObject_new((GObject*)val))
#define GtkSourceView_val(val) check_cast(GTK_SOURCE_VIEW,val)
#define GtkTextIter_val(val) ((GtkTextIter*)MLPointer_val(val))
#define Val_GtkTextIter(it) (copy_memblock_indirected(it,sizeof(GtkTextIter)))
#define Val_option_GtkAny(v) Val_option(v,Val_GtkAny)
#define string_list_of_GSList(l) Val_GSList(l, (value_in) Val_string)

#define GdkPixbuf_option_val(val) Option_val(val, GdkPixbuf_val, NULL)
#define GdkColor_option_val(val) Option_val(val, GdkColor_val, NULL)

static value val_gtksourcemark(gpointer v)
{
  return Val_GtkSourceMark(v);
}

value source_marker_list_of_GSList(gpointer list)
{
  return Val_GSList(list, val_gtksourcemark);
}

static value val_gtksourcelanguage(gpointer v)
{
  return Val_GtkSourceLanguage(v);
}

value source_language_list_of_GSList(gpointer list)
{
  return Val_GSList(list, val_gtksourcelanguage);
}

ML_1 (gtk_source_style_scheme_get_name, GtkSourceStyleScheme_val, Val_string)
ML_1 (gtk_source_style_scheme_get_description, GtkSourceStyleScheme_val, Val_string)

ML_0 (gtk_source_style_scheme_manager_new, Val_GtkAny_sink)
ML_0 (gtk_source_style_scheme_manager_get_default,
      Val_GtkSourceStyleSchemeManager)
ML_2 (gtk_source_style_scheme_manager_get_scheme,
      GtkSourceStyleSchemeManager_val, String_val,
      Val_option_GtkSourceStyleScheme)
ML_1 (gtk_source_style_scheme_manager_get_scheme_ids,
      GtkSourceStyleSchemeManager_val, string_list_of_strv)
ML_1 (gtk_source_style_scheme_manager_get_search_path,
      GtkSourceStyleSchemeManager_val, string_list_of_strv)
ML_2 (gtk_source_style_scheme_manager_set_search_path,
      GtkSourceStyleSchemeManager_val, strv_of_string_list, Unit)
ML_2 (gtk_source_style_scheme_manager_prepend_search_path,
      GtkSourceStyleSchemeManager_val, String_val, Unit)
ML_2 (gtk_source_style_scheme_manager_append_search_path,
      GtkSourceStyleSchemeManager_val, String_val, Unit)
ML_1 (gtk_source_style_scheme_manager_force_rescan,
      GtkSourceStyleSchemeManager_val, Unit)

ML_1 (gtk_source_language_get_id, GtkSourceLanguage_val, Val_string)
ML_1 (gtk_source_language_get_name, GtkSourceLanguage_val, Val_string)
ML_1 (gtk_source_language_get_section, GtkSourceLanguage_val, Val_string)
ML_1 (gtk_source_language_get_hidden, GtkSourceLanguage_val, Val_bool)

ML_2 (gtk_source_language_get_metadata, GtkSourceLanguage_val,
      String_option_val, Val_optstring)

ML_1 (gtk_source_language_get_mime_types, GtkSourceLanguage_val,
      string_list_of_strv2)
ML_1 (gtk_source_language_get_globs, GtkSourceLanguage_val,
      string_list_of_strv2)

ML_2 (gtk_source_language_get_style_name, GtkSourceLanguage_val, String_val,
      Val_optstring)

ML_1 (gtk_source_language_get_style_ids, GtkSourceLanguage_val,
      string_list_of_strv2)


ML_0 (gtk_source_language_manager_new, Val_GtkAny_sink)

ML_0(gtk_source_language_manager_get_default,Val_GtkSourceLanguageManager)

/* This function leaks the strv. It needs to be freed before returning. */
ML_2(gtk_source_language_manager_set_search_path,GtkSourceLanguageManager_val,
     strv_of_string_list,Unit)

#if 0
// I need to find a test for this code
CAMLprim value ml_gtk_source_language_manager_set_search_path(value lm, value sl)
{
  gchar** strv = strv_of_string_list(sl);
  gchar **index = strv;
  gtk_source_language_manager_set_search_path(GtkSourceLanguageManager_val(lm),strv);

  while(*index != NULL) {g_free(*strv); strv++; };
  g_free(strv);
  return Val_unit;
}
#endif

ML_1(gtk_source_language_manager_get_search_path,GtkSourceLanguageManager_val,
     string_list_of_strv)
ML_1(gtk_source_language_manager_get_language_ids,GtkSourceLanguageManager_val,
     string_list_of_strv)
ML_2(gtk_source_language_manager_get_language,GtkSourceLanguageManager_val,
     String_val,Val_option_GtkSourceLanguage)
ML_3 (gtk_source_language_manager_guess_language, GtkSourceLanguageManager_val,
      String_option_val, String_option_val, Val_option_GtkSourceLanguage)


ML_2 (gtk_source_mark_new, String_val, String_val, Val_GtkSourceMark_new)

ML_1 (gtk_source_mark_get_category, GtkSourceMark_val, Val_string)
ML_2 (gtk_source_mark_next, GtkSourceMark_val, String_option_val, Val_option_GtkSourceMark)
ML_2 (gtk_source_mark_prev, GtkSourceMark_val, String_option_val, Val_option_GtkSourceMark)


ML_1 (gtk_source_buffer_new, GtkTextTagTable_val, Val_GtkSourceBuffer_new)
ML_1 (gtk_source_buffer_new_with_language, GtkSourceLanguage_val, Val_GtkAny_sink)
ML_1 (gtk_source_buffer_can_undo, GtkSourceBuffer_val, Val_bool)
ML_1 (gtk_source_buffer_can_redo, GtkSourceBuffer_val, Val_bool)
ML_1 (gtk_source_buffer_undo, GtkSourceBuffer_val, Unit)
ML_1 (gtk_source_buffer_redo, GtkSourceBuffer_val, Unit)
ML_1 (gtk_source_buffer_begin_not_undoable_action, GtkSourceBuffer_val, Unit)
ML_1 (gtk_source_buffer_end_not_undoable_action, GtkSourceBuffer_val, Unit)
ML_4 (gtk_source_buffer_create_source_mark, GtkSourceBuffer_val,
      String_option_val, String_option_val, GtkTextIter_val, Val_GtkSourceMark)
ML_4 (gtk_source_buffer_remove_source_marks, GtkSourceBuffer_val,
      GtkTextIter_val, GtkTextIter_val, String_option_val, Unit)
ML_3 (gtk_source_buffer_get_source_marks_at_iter, GtkSourceBuffer_val,
      GtkTextIter_val,String_option_val, source_marker_list_of_GSList)
ML_3 (gtk_source_buffer_get_source_marks_at_line, GtkSourceBuffer_val,
      Int_val,String_option_val, source_marker_list_of_GSList)

ML_3 (gtk_source_buffer_forward_iter_to_source_mark, GtkSourceBuffer_val, GtkTextIter_val, String_option_val, Val_bool)
ML_3 (gtk_source_buffer_backward_iter_to_source_mark, GtkSourceBuffer_val, GtkTextIter_val, String_option_val, Val_bool)

ML_3 (gtk_source_buffer_ensure_highlight, GtkSourceBuffer_val,
      GtkTextIter_val, GtkTextIter_val, Unit)

ML_2 (gtk_source_buffer_set_highlight_matching_brackets, GtkSourceBuffer_val, Bool_val, Unit);


ML_0 (gtk_source_view_new, Val_GtkWidget_sink)
ML_1 (gtk_source_view_new_with_buffer, GtkSourceBuffer_val, Val_GtkWidget_sink)

ML_2 (gtk_source_view_get_mark_category_priority,
      GtkSourceView_val, String_val, Val_int)
ML_3 (gtk_source_view_set_mark_category_priority,
      GtkSourceView_val, String_val, Int_val, Unit)
ML_3 (gtk_source_view_set_mark_category_pixbuf, GtkSourceView_val,
      String_val, GdkPixbuf_option_val, Unit)
ML_2 (gtk_source_view_get_mark_category_pixbuf, GtkSourceView_val,
      String_val, Val_option_GdkPixbuf)
ML_3 (gtk_source_view_set_mark_category_background,
      GtkSourceView_val, String_val, GdkColor_option_val, Unit)

CAMLprim value ml_gtk_source_view_get_mark_category_background
(value sv, value s, value c) {
     CAMLparam3(sv, s, c);
     CAMLlocal2(color, result);
     GdkColor dest;

     if (gtk_source_view_get_mark_category_background(
	      GtkSourceView_val(sv), String_val(s), &dest)) {
	  color = Val_copy(dest);
	  result = alloc_small(1, 0);
	  Field(result, 0) = color;
     }
     else
	  result = Val_unit;

     CAMLreturn(result);
}

Make_Flags_val(Source_draw_spaces_flags_val)
#define Val_flags_Draw_spaces_flags(val) \
     ml_lookup_flags_getter(ml_table_source_draw_spaces_flags, val)

ML_1 (gtk_source_view_get_draw_spaces,
      GtkSourceView_val, Val_flags_Draw_spaces_flags)
ML_2 (gtk_source_view_set_draw_spaces,
      GtkSourceView_val, Flags_Source_draw_spaces_flags_val, Unit)


/* This code was taken from gedit */
/* assign a unique name */
static G_CONST_RETURN gchar *
get_widget_name (GtkWidget *w)
{
        const gchar *name;

        name = gtk_widget_get_name (w);
        g_return_val_if_fail (name != NULL, NULL);

        if (strcmp (name, g_type_name (GTK_WIDGET_TYPE (w))) == 0)
        {
                static guint d = 0;
                gchar *n;

                n = g_strdup_printf ("%s_%u_%u", name, d, g_random_int());
                d++;

                gtk_widget_set_name (w, n);
                g_free (n);

                name = gtk_widget_get_name (w);
        }

        return name;
}
/* There is no clean way to set the cursor-color, so we are stuck
 * with the following hack: set the name of each widget and parse
 * a gtkrc string.
 */
static void
gtk_modify_cursor_color (GtkWidget *textview,
                     GdkColor  *color)
{
        static const char cursor_color_rc[] =
                "style \"svs-cc\"\n"
                "{\n"
                        "GtkSourceView::cursor-color=\"#%04x%04x%04x\"\n"
                "}\n"
                "widget \"*.%s\" style : application \"svs-cc\"\n";

        const gchar *name;
        gchar *rc_temp;

        name = get_widget_name (textview);
        g_return_if_fail (name != NULL);

        if (color != NULL)
        {
                rc_temp = g_strdup_printf (cursor_color_rc,
                                           color->red,
                                           color->green,
                                           color->blue,
                                           name);
        }
        else
        {
                GtkRcStyle *rc_style;

                rc_style = gtk_widget_get_modifier_style (textview);

                rc_temp = g_strdup_printf (cursor_color_rc,
                                           rc_style->text [GTK_STATE_NORMAL].red,
                                           rc_style->text [GTK_STATE_NORMAL].green,
                                           rc_style->text [GTK_STATE_NORMAL].blue,
                                           name);
        }

        gtk_rc_parse_string (rc_temp);
        gtk_widget_reset_rc_styles (textview);

        g_free (rc_temp);
}
/* end of gedit code */

ML_2(gtk_modify_cursor_color,GtkWidget_val,GdkColor_val,Unit);

#define Make_search(dir) \
CAMLprim value ml_gtk_source_iter_##dir##_search (value ti,\
                                                value str,\
                                                value flag,\
                                                value ti_stop,\
                                                value ti_start,\
                                                value ti_lim)\
{ CAMLparam5(ti,str,flag,ti_start,ti_stop);\
  CAMLxparam1(ti_lim);\
  CAMLlocal2(res,coup);\
  GtkTextIter* ti1,*ti2;\
  gboolean b;\
  ti1=gtk_text_iter_copy(GtkTextIter_val(ti_start));\
  ti2=gtk_text_iter_copy(GtkTextIter_val(ti_stop));\
  b=gtk_source_iter_##dir##_search(GtkTextIter_val(ti),\
                                 String_val(str),\
                                 OptFlags_Source_search_flag_val(flag),\
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
ML_bc6(ml_gtk_source_iter_forward_search);
ML_bc6(ml_gtk_source_iter_backward_search);