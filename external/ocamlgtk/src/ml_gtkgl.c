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

/* $Id: ml_gtkgl.c 1347 2007-06-20 07:40:34Z guesdon $ */

#include <gtk/gtk.h>
#include <gtkgl/gtkglarea.h>
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
#include "gtkgl_tags.h"

/* Conversion functions */
#include "gtkgl_tags.c"

#define GtkGLArea_val(val) check_cast(GTK_GL_AREA,val)

CAMLprim value ml_gtk_gl_area_new (value list, value share)
{
    value cursor, res;
    int len, i;
    int *attrs;

    for (len = 0, cursor = list; cursor != Val_unit; cursor = Field(cursor,1))
    {
	if (Is_block(Field(cursor,0))) len += 2;
	else len++;
    }

    attrs = (int*) stat_alloc ((len+1)*sizeof(int));
    
    for (i = 0, cursor = list; cursor != Val_unit; cursor = Field(cursor,1))
    {
	value option = Field(cursor,0);
	if (Is_block(option)) {
	    attrs[i++] = Visual_options_val(Field(option,0));
	    attrs[i++] = Int_val(Field(option,1));
	}
	else attrs[i++] = Visual_options_val(option);
    }
    attrs[i] = GDK_GL_NONE;

    res = Val_GtkObject_sink
	((GtkObject*)gtk_gl_area_share_new(attrs,GtkGLArea_val(share)));
    stat_free(attrs);
    return res;
}

ML_1 (gtk_gl_area_make_current, GtkGLArea_val, Val_bool)
ML_1 (gtk_gl_area_swap_buffers, GtkGLArea_val, Unit)
