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

#include <glib-object.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>

#include "wrappers.h"
#include "ml_gobject.h"
#include "ml_gvaluecaml.h"

static gpointer caml_boxed_copy (gpointer boxed)
{
  value *val = boxed;
  return ml_global_root_new (*val);
}

GType g_caml_get_type()
{
  static GType type = G_TYPE_INVALID;
  if (type == G_TYPE_INVALID)
    type = g_boxed_type_register_static ("Caml",
					 caml_boxed_copy,
					 ml_global_root_destroy);
  return type;
}

CAMLprim value ml_g_caml_get_type(value unit)
{
  return Val_GType(G_TYPE_CAML);
}

void g_value_store_caml_value (GValue *val, value arg)
{
  g_return_if_fail (G_VALUE_HOLDS(val, G_TYPE_CAML));
  g_value_set_boxed (val, &arg);
}
