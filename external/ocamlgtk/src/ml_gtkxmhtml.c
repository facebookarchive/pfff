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

/* $Id: ml_gtkxmhtml.c 1347 2007-06-20 07:40:34Z guesdon $ */

#include <string.h>
#include <gtk/gtk.h>
#include <gtk-xmhtml/gtk-xmhtml.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include "wrappers.h"
#include "ml_glib.h"
#include "ml_gdk.h"
#include "ml_gtk.h"
#include "gtkxmhtml_tags.h"

/* conversion functions */

#include "gtkxmhtml_tags.c"

Make_Flags_val (Line_type_val)

#define GtkXmHTML_val(val) ((GtkXmHTML*)GtkObject_val(val))

ML_0 (gtk_xmhtml_new, Val_GtkAny_sink)
ML_1 (gtk_xmhtml_freeze, GtkXmHTML_val, Unit)
ML_1 (gtk_xmhtml_thaw, GtkXmHTML_val, Unit)
ML_2 (gtk_xmhtml_source, GtkXmHTML_val, String_val, Unit)
ML_2 (gtk_xmhtml_set_string_direction, GtkXmHTML_val, String_direction_val,
      Unit)
ML_2 (gtk_xmhtml_set_alignment, GtkXmHTML_val, Alignment_val, Unit)
/* ML_2 (gtk_xmhtml_outline, GtkXmHTML_val, Bool_val, Unit) */
ML_3 (gtk_xmhtml_set_font_familty, GtkXmHTML_val, String_val, String_val, Unit)
ML_3 (gtk_xmhtml_set_font_familty_fixed, GtkXmHTML_val, String_val, String_val,
      Unit)
ML_2 (gtk_xmhtml_set_font_charset, GtkXmHTML_val, String_val, Unit)
ML_2 (gtk_xmhtml_set_allow_body_colors, GtkXmHTML_val, Bool_val, Unit)
ML_2 (gtk_xmhtml_set_hilight_on_enter, GtkXmHTML_val, Bool_val, Unit)
ML_2 (gtk_xmhtml_set_anchor_underline_type, GtkXmHTML_val, Flags_Line_type_val,
      Unit)
ML_2 (gtk_xmhtml_set_anchor_visited_underline_type, GtkXmHTML_val,
      Flags_Line_type_val, Unit)
ML_2 (gtk_xmhtml_set_anchor_target_underline_type, GtkXmHTML_val,
      Flags_Line_type_val, Unit)
ML_2 (gtk_xmhtml_set_allow_color_switching, GtkXmHTML_val, Bool_val, Unit)
ML_2 (gtk_xmhtml_set_dithering, GtkXmHTML_val, Dither_type_val, Unit)
ML_2 (gtk_xmhtml_set_allow_font_switching, GtkXmHTML_val, Bool_val, Unit)
ML_2 (gtk_xmhtml_set_max_image_colors, GtkXmHTML_val, Int_val, Unit)
ML_2 (gtk_xmhtml_set_allow_images, GtkXmHTML_val, Bool_val, Unit)
ML_4 (gtk_xmhtml_set_plc_intervals, GtkXmHTML_val, Int_val, Int_val, Int_val,
      Unit)
/* ML_2 (gtk_xmhtml_set_def_body_image_url, GtkXmHTML_val, String_val, Unit) */
ML_2 (gtk_xmhtml_set_anchor_buttons, GtkXmHTML_val, Bool_val, Unit)
CAMLprim value ml_gtk_xmhtml_set_anchor_cursor(value html, value cursor)
{
     gtk_xmhtml_set_anchor_cursor
          (GtkXmHTML_val(html), Option_val(cursor, GdkCursor_val, NULL),
           Bool_val(cursor));
     return Val_unit;
}
ML_2 (gtk_xmhtml_set_topline, GtkXmHTML_val, Int_val, Unit)
ML_1 (gtk_xmhtml_get_topline, GtkXmHTML_val, Val_int)
ML_2 (gtk_xmhtml_set_freeze_animations, GtkXmHTML_val, Bool_val, Unit)
/* ML_1 (gtk_xmhtml_get_source, GtkXmHTML_val, copy_string) */
ML_2 (gtk_xmhtml_set_screen_gamma, GtkXmHTML_val, Float_val, Unit)
/* ML_2 (gtk_xmhtml_set_event_proc, GtkXmHTML_val, ???, Unit) */
ML_2 (gtk_xmhtml_set_perfect_colors, GtkXmHTML_val, Bool_val, Unit)
ML_2 (gtk_xmhtml_set_uncompress_command, GtkXmHTML_val, String_val, Unit)
ML_2 (gtk_xmhtml_set_strict_checking, GtkXmHTML_val, Bool_val, Unit)
ML_2 (gtk_xmhtml_set_bad_html_warnings, GtkXmHTML_val, Bool_val, Unit)
ML_2 (gtk_xmhtml_set_allow_form_coloring, GtkXmHTML_val, Bool_val, Unit)
ML_2 (gtk_xmhtml_set_imagemap_draw, GtkXmHTML_val, Bool_val, Unit)
ML_2 (gtk_xmhtml_set_mime_type, GtkXmHTML_val, String_val, Unit)
ML_2 (gtk_xmhtml_set_alpha_processing, GtkXmHTML_val, Bool_val, Unit)
ML_2 (gtk_xmhtml_set_rgb_conv_mode, GtkXmHTML_val, Dither_type_val, Unit)
