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

/* $Id: ml_gobject.h 1390 2007-11-28 05:26:18Z garrigue $ */

/* Defined in ml_gobject.h */

#define GObject_val(val) ((GObject*)Pointer_val(val))
CAMLexport value Val_GObject (GObject *);
CAMLexport value Val_GObject_new (GObject *);
#define Val_GAnyObject(val) Val_GObject(G_OBJECT(val))
#define Val_GAnyObject_new(val) Val_GObject_new(G_OBJECT(val))
CAMLexport void ml_g_object_unref_later (GObject *);

#define GType_val(t) ((GType)Addr_val(t))
#define Val_GType    Val_addr

#define GClosure_val(val) ((GClosure*)Pointer_val(val))
CAMLexport value Val_GClosure (GClosure *);

#define GValueptr_val(val) ((GValue*)MLPointer_val(val))
CAMLexport GValue *GValue_val(value);          /* check for NULL pointer */
CAMLexport value Val_GValue_copy(GValue *);    /* copy from the stack */
#define Val_GValue_wrap Val_pointer /* just wrap a pointer */
CAMLexport value ml_g_value_new(void);

CAMLexport value Val_gboxed(GType t, gpointer p);     /* finalized gboxed */
CAMLexport value Val_gboxed_new(GType t, gpointer p); /* without copy */

/* Macro utilities for export */
/* used in ml_gtk.h for instance */

#ifdef G_DISABLE_CAST_CHECKS
#define check_cast(f,v) f(Pointer_val(v))
#else
#define check_cast(f,v) (Pointer_val(v) == NULL ? NULL : f(Pointer_val(v)))
#endif
