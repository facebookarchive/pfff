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

#include <gtk/gtk.h>
#include <gtkspell/gtkspell.h>

#include <caml/mlvalues.h>

#include "wrappers.h"
#include "ml_gobject.h"
#include "ml_glib.h"

CAMLprim value
ml_gtkspell_init (value unit)
{
  ml_register_exn_map (GTKSPELL_ERROR, "gtkspell_error");
  return Val_unit;
}

CAMLprim value
ml_gtkspell_new_attach (value textview, value lang)
{
  GError *err = NULL;
  gtkspell_new_attach (check_cast(GTK_TEXT_VIEW, textview), 
		       String_option_val (lang), &err);
  if (err) ml_raise_gerror (err);
  return Val_unit;
}

CAMLprim value
ml_gtkspell_is_attached (value textview)
{
  return Val_bool (gtkspell_get_from_text_view (check_cast(GTK_TEXT_VIEW, textview)) != NULL);
} 

CAMLprim value
ml_gtkspell_get_from_text_view (value view)
{
  GtkSpell *s;
  s = gtkspell_get_from_text_view (check_cast(GTK_TEXT_VIEW, view));
  return s ? ml_some (Val_pointer (s)) : Val_unit;
}

#define GtkSpell_val(v) (GtkSpell *)Pointer_val(v)

ML_1 (gtkspell_detach, GtkSpell_val, Unit)

CAMLprim value
ml_gtkspell_set_language (value spell, value lang)
{
  GError *err = NULL;
  if (! gtkspell_set_language (GtkSpell_val (spell), 
			       String_option_val (lang), &err))
    ml_raise_gerror (err);
  return Val_unit;
}

ML_1 (gtkspell_recheck_all, GtkSpell_val, Unit)
