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

#include <gtksourceview/gtksourcelanguagesmanager.h>
#include <gtksourceview/gtksourcetag.h>
#include <gtksourceview/gtksourceiter.h>
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
#include "sourceView_tags.h"
#include <string.h>
#include "sourceView_tags.c"


Make_OptFlags_val(Source_search_flag_val)

CAMLprim value ml_gtk_source_tag_style_init(value unit)
{	/* Since these are declared const, must force gcc to call them! */
    GType t = gtk_source_tag_style_get_type();
    return Val_GType(t);
}

CAMLprim value ml_gtk_source_tag_init(value unit)
{
  /* Since these are declared const, must force gcc to call them! */
  GType t = gtk_source_tag_get_type();
  return Val_GType(t);
}

CAMLprim value ml_gtk_source_tag_table_init(value unit)
{
  /* Since these are declared const, must force gcc to call them! */
  GType t = gtk_source_tag_table_get_type();
  return Val_GType(t);
}

CAMLprim value ml_gtk_source_style_scheme_init(value unit)
{	/* Since these are declared const, must force gcc to call them! */
    GType t = gtk_source_style_scheme_get_type();
    return Val_GType(t);
}

CAMLprim value ml_gtk_source_language_init(value unit)
{	/* Since these are declared const, must force gcc to call them! */
    GType t = gtk_source_language_get_type();
    return Val_GType(t);
}

CAMLprim value ml_gtk_source_languages_manager_init(value unit)
{	/* Since these are declared const, must force gcc to call them! */
    GType t = 
      gtk_source_languages_manager_get_type();
    return Val_GType(t);
}

CAMLprim value ml_gtk_source_marker_init(value unit)
{
  /* Since these are declared const, must force gcc to call them! */
  GType t = gtk_source_marker_get_type();
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

/* CAMLprim value
ml_gtk_source_languages_manager_set_lang_files_dirs(GObject *obj, value list)
{
	GSList *gslist = ml_gslist_of_string_list(list);
	g_object_set_property(obj, "lang-files-dirs", gslist);
	return Val_unit;
} */

#define GtkSourceStyleScheme_val(val) check_cast(GTK_SOURCE_STYLE_SCHEME,val)
#define Val_GtkSourceStyleScheme(val) (Val_GObject((GObject*)val))
#define Val_GtkSourceStyleScheme_new(val) (Val_GObject_new((GObject*)val))

#define Val_GtkSourceLanguage(val)  (Val_GObject((GObject*)val))
#define GtkSourceLanguage_val(val) check_cast(GTK_SOURCE_LANGUAGE,val)
#define GtkSourceLanguagesManager_val(val)\
	check_cast(GTK_SOURCE_LANGUAGES_MANAGER,val)

#define GtkSourceTagStyle_val(val) Pointer_val(val)
#define Val_GtkSourceTagStyle(val)  Val_pointer(val)
#define Val_GtkSourceTagStyle_new(val) (Val_pointer((GtkSourceTagStyle*)val))
#define Val_option_GtkSourceTagStyle(val) Val_option(val,Val_GtkSourceTagStyle)

#define GtkSourceTag_val(val) check_cast(GTK_SOURCE_TAG,val)
#define Val_GtkSourceTag(val)  (Val_GObject((GObject*)val))
#define Val_GtkSourceTag_new(val) (Val_GObject_new((GObject*)val))

#define GtkSourceTagTable_val(val) check_cast(GTK_SOURCE_TAG_TABLE,val)
#define Val_GtkSourceTagTable(val)  (Val_GObject((GObject*)val))
#define Val_GtkSourceTagTable_new(val) (Val_GObject_new((GObject*)val))

#define GtkSourceMarker_val(val) check_cast(GTK_SOURCE_MARKER,val)
#define Val_GtkSourceMarker(val)  (Val_GObject((GObject*)val))
#define Val_GtkSourceMarker_new(val) (Val_GObject_new((GObject*)val))
#define Val_option_GtkSourceMarker(val) Val_option(val,Val_GtkSourceMarker)

#define GtkSourceBuffer_val(val) check_cast(GTK_SOURCE_BUFFER,val)
#define Val_GtkSourceBuffer(val) (Val_GObject((GObject*)val))
#define Val_GtkSourceBuffer_new(val) (Val_GObject_new((GObject*)val))
#define GtkSourceView_val(val) check_cast(GTK_SOURCE_VIEW,val)
#define GtkTextIter_val(val) ((GtkTextIter*)MLPointer_val(val))
#define Val_GtkTextIter(it) (copy_memblock_indirected(it,sizeof(GtkTextIter)))
#define Val_option_GtkAny(v) Val_option(v,Val_GtkAny)
#define string_list_of_GSList(l) Val_GSList(l, (value_in) Val_string)

static gpointer gtksourcetag_val(value v)
{
  return GtkSourceTag_val(v);
}

GSList *gslist_of_source_tag_list(value list)
{
  return GSList_val(list, gtksourcetag_val);
}

static value val_gtksourcetag (gpointer v)
{
  return Val_GtkSourceTag(v);
}

value source_tag_list_of_GSList(gpointer list)
{
  return Val_GSList(list, val_gtksourcetag);
}

static value val_gtksourcemarker(gpointer v)
{
  return Val_GtkSourceMarker(v);
}

value source_marker_list_of_GSList(gpointer list)
{
  return Val_GSList(list, val_gtksourcemarker);
}

static value val_gtksourcelanguage(gpointer v)
{
  return Val_GtkSourceLanguage(v);
}

value source_language_list_of_GSList(gpointer list)
{
  return Val_GSList(list, val_gtksourcelanguage);
}

#define STS_GET(prop,conv) \
CAMLprim value ml_gtk_source_tag_style_get_##prop (value v) \
{ CAMLparam1(v);\
  GtkSourceTagStyle *sts = Pointer_val(v);\
  CAMLreturn(conv (sts->prop));}

#define STS_SET(prop,conv) \
CAMLprim value ml_gtk_source_tag_style_set_##prop (value arg1, value arg2) \
{ CAMLparam2(arg1,arg2);\
  GtkSourceTagStyle *sts = Pointer_val(arg1);\
  sts->prop = conv(arg2);\
  CAMLreturn(Val_unit);}

#define Val_GdkColor_addr(v) Val_GdkColor(&v)

STS_GET(background,Val_GdkColor_addr)
STS_GET(bold,Val_bool)
STS_GET(foreground,Val_GdkColor_addr)
STS_GET(italic,Val_bool)
STS_GET(strikethrough,Val_bool)
STS_GET(underline,Val_bool)

STS_SET(background,*GdkColor_val)
STS_SET(bold,Bool_val)
STS_SET(foreground,*GdkColor_val)
STS_SET(italic,Bool_val)
STS_SET(strikethrough,Bool_val)
STS_SET(underline,Bool_val)

#define STS_CHANGE_MASK(name,themask) \
CAMLprim value ml_gtk_source_tag_style_set_use_##name (value v, value b) \
{ CAMLparam2(v,b);\
  GtkSourceTagStyle *sts = Pointer_val(v);\
  if (Bool_val(b)) \
    sts->mask = sts->mask | themask ;\
  else \
    sts->mask = sts->mask & (0xFFFF - themask); \
  CAMLreturn(Val_unit); }

#define STS_GET_MASK(name,themask) \
CAMLprim value ml_gtk_source_tag_style_get_use_##name (value v) \
{ CAMLparam1(v);\
  GtkSourceTagStyle *sts = Pointer_val(v);\
  CAMLreturn((Val_bool(sts->mask & themask)));}

STS_CHANGE_MASK(background,GTK_SOURCE_TAG_STYLE_USE_BACKGROUND)
STS_CHANGE_MASK(foreground,GTK_SOURCE_TAG_STYLE_USE_FOREGROUND)
STS_GET_MASK(background,GTK_SOURCE_TAG_STYLE_USE_BACKGROUND)
STS_GET_MASK(foreground,GTK_SOURCE_TAG_STYLE_USE_FOREGROUND)
/*
CAMLprim value ml_source_tag_style_get_background (value v)
{ GtkSourceTagStyle *sts = Pointer_val(v);
  CAMLreturn(Val_GdkColor_addr(sts->background));}
*/

ML_0 (gtk_source_style_scheme_get_default, Val_GtkSourceStyleScheme_new)
ML_2 (gtk_source_style_scheme_get_tag_style, GtkSourceStyleScheme_val,
      String_val, Val_option_GtkSourceTagStyle)
ML_1 (gtk_source_style_scheme_get_name, GtkSourceStyleScheme_val, Val_string)

/* Internal function of gtk_source_language */
GtkSourceLanguage *_gtk_source_language_new_from_file (const gchar *filename,
                                                       GtkSourceLanguagesManager *lm);
ML_2 (_gtk_source_language_new_from_file, String_val,
		GtkSourceLanguagesManager_val, Val_option_GtkAny)


ML_1 (gtk_source_language_get_name, GtkSourceLanguage_val, Val_string)
ML_1 (gtk_source_language_get_section, GtkSourceLanguage_val, Val_string)
ML_1 (gtk_source_language_get_escape_char, GtkSourceLanguage_val, Val_int)
ML_1 (gtk_source_language_get_tags, GtkSourceLanguage_val, source_tag_list_of_GSList)
ML_1 (gtk_source_language_get_style_scheme, GtkSourceLanguage_val, Val_GtkSourceStyleScheme)
ML_2 (gtk_source_language_set_style_scheme,
      GtkSourceLanguage_val, GtkSourceStyleScheme_val, Unit)

ML_0 (gtk_source_languages_manager_new, Val_GtkAny_sink)
ML_2 (gtk_source_languages_manager_get_language_from_mime_type,
		GtkSourceLanguagesManager_val, String_val,
		Val_option_GtkAny)
ML_1 (gtk_source_languages_manager_get_lang_files_dirs,
		GtkSourceLanguagesManager_val, string_list_of_GSList)
ML_1 (gtk_source_languages_manager_get_available_languages,
		GtkSourceLanguagesManager_val, source_language_list_of_GSList)

ML_2 (gtk_source_language_get_tag_style, GtkSourceLanguage_val,
      String_val, Val_GtkSourceTagStyle)
ML_3 (gtk_source_language_set_tag_style, GtkSourceLanguage_val,
      String_val, GtkSourceTagStyle_val, Unit)
ML_2 (gtk_source_language_get_tag_default_style, GtkSourceLanguage_val,
      String_val, Val_GtkSourceTagStyle)

ML_4 (gtk_syntax_tag_new, String_val, String_val, String_val, String_val, Val_GtkSourceTag_new)
ML_3 (gtk_pattern_tag_new, String_val, String_val, String_val, Val_GtkSourceTag_new)

ML_8 (gtk_keyword_list_tag_new, String_val, String_val,
      ml_gslist_of_string_list, Bool_val, Bool_val, Bool_val,
      String_option_val, String_option_val, Val_GtkSourceTag_new)
ML_bc8 (ml_gtk_keyword_list_tag_new)

ML_3 (gtk_line_comment_tag_new, String_val, String_val, String_val, Val_GtkSourceTag_new)
ML_5 (gtk_string_tag_new, String_val, String_val, String_val, String_val, Bool_val, Val_GtkSourceTag_new)
ML_1 (gtk_source_tag_get_style,GtkSourceTag_val,Val_option_GtkSourceTagStyle)
ML_2 (gtk_source_tag_set_style,GtkSourceTag_val,GtkSourceTagStyle_val,Unit)


ML_0 (gtk_source_tag_style_new, Val_GtkSourceTagStyle_new)
ML_1 (gtk_source_tag_style_copy, GtkSourceTagStyle_val, Val_GtkSourceTagStyle)

ML_0 (gtk_source_tag_table_new, Val_GtkSourceTagTable_new)
ML_1 (gtk_source_tag_table_remove_source_tags, GtkSourceTagTable_val,Unit)
ML_2 (gtk_source_tag_table_add_tags, GtkSourceTagTable_val, gslist_of_source_tag_list, Unit)

ML_2 (gtk_source_marker_set_marker_type, GtkSourceMarker_val, String_val, Unit)
ML_1 (gtk_source_marker_get_marker_type, GtkSourceMarker_val, Val_string)
ML_1 (gtk_source_marker_get_line, GtkSourceMarker_val, Val_int)
ML_1 (gtk_source_marker_get_name, GtkSourceMarker_val, Val_string)
ML_1 (gtk_source_marker_get_buffer, GtkSourceMarker_val, Val_GtkSourceBuffer)
ML_1 (gtk_source_marker_next, GtkSourceMarker_val, Val_GtkSourceMarker)
ML_1 (gtk_source_marker_prev, GtkSourceMarker_val, Val_GtkSourceMarker)


ML_1 (gtk_source_buffer_new, GtkSourceTagTable_val, Val_GtkSourceBuffer_new)
ML_1 (gtk_source_buffer_new_with_language, GtkSourceLanguage_val, Val_GtkAny_sink)
ML_1 (gtk_source_buffer_can_undo, GtkSourceBuffer_val, Val_bool)
ML_1 (gtk_source_buffer_can_redo, GtkSourceBuffer_val, Val_bool)
ML_1 (gtk_source_buffer_undo, GtkSourceBuffer_val, Unit)
ML_1 (gtk_source_buffer_redo, GtkSourceBuffer_val, Unit)
ML_1 (gtk_source_buffer_begin_not_undoable_action, GtkSourceBuffer_val, Unit)
ML_1 (gtk_source_buffer_end_not_undoable_action, GtkSourceBuffer_val, Unit)
ML_4 (gtk_source_buffer_create_marker, GtkSourceBuffer_val,
      String_option_val, String_option_val, GtkTextIter_val, Val_GtkSourceMarker)
ML_3 (gtk_source_buffer_move_marker, GtkSourceBuffer_val,
      GtkSourceMarker_val, GtkTextIter_val, Unit)
ML_2 (gtk_source_buffer_delete_marker, GtkSourceBuffer_val,
      GtkSourceMarker_val, Unit)
ML_2 (gtk_source_buffer_get_marker, GtkSourceBuffer_val,
      String_val, Val_option_GtkSourceMarker)
ML_1 (gtk_source_buffer_get_first_marker, GtkSourceBuffer_val, Val_option_GtkSourceMarker)
ML_1 (gtk_source_buffer_get_last_marker, GtkSourceBuffer_val, Val_option_GtkSourceMarker)
ML_2 (gtk_source_buffer_get_next_marker, GtkSourceBuffer_val, GtkTextIter_val,
      Val_option_GtkSourceMarker)
ML_2 (gtk_source_buffer_get_prev_marker, GtkSourceBuffer_val, GtkTextIter_val,
      Val_option_GtkSourceMarker)
CAMLprim value ml_gtk_source_buffer_get_iter_at_marker(value vbuf, value vmark)
{ CAMLparam2(vbuf,vmark);
  GtkTextIter iter;
  gtk_source_buffer_get_iter_at_marker(GtkSourceBuffer_val(vbuf), &iter, GtkSourceMarker_val(vmark));
  CAMLreturn(Val_GtkTextIter(&iter));
}
ML_3 (gtk_source_buffer_get_markers_in_region, GtkSourceBuffer_val,
      GtkTextIter_val, GtkTextIter_val, source_marker_list_of_GSList);
ML_2 (gtk_source_buffer_set_bracket_match_style, GtkSourceBuffer_val, GtkSourceTagStyle_val, Unit);


ML_0 (gtk_source_view_new, Val_GtkWidget_sink)
ML_1 (gtk_source_view_new_with_buffer, GtkSourceBuffer_val, Val_GtkWidget_sink)
ML_3 (gtk_source_view_set_marker_pixbuf, GtkSourceView_val, String_val, GdkPixbuf_val, Unit)
ML_2 (gtk_source_view_get_marker_pixbuf, GtkSourceView_val, String_val, Val_GdkPixbuf)


ML_1 (gtk_source_iter_find_matching_bracket, GtkTextIter_val, Val_bool)

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

                n = g_strdup_printf ("%s_%u_%u", name, d, g_random_int ());
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
