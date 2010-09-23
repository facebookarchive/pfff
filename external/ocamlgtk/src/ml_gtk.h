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

/* $Id: ml_gtk.h 1455 2009-05-12 10:31:37Z garrigue $ */

/* GObjects */

#define Val_GtkAccelGroup(val) (Val_GObject(&val->parent))
#define Val_GtkStyle(val) (Val_GObject(&val->parent_instance))

#define GtkAccelGroup_val(val) check_cast(GTK_ACCEL_GROUP,val)
#define GtkStyle_val(val) check_cast(GTK_STYLE,val)

/* GtkObjects */
CAMLexport value Val_GtkObject_sink (GtkObject *w);

#define Val_GtkAny(w) (Val_GObject((GObject*)w))
#define Val_GtkAny_sink(w) (Val_GtkObject_sink(GTK_OBJECT(w)))
#define Val_GtkWidget Val_GtkAny
#define Val_GtkWidget_sink Val_GtkAny_sink

/* For GList containing widgets */
CAMLexport value Val_GtkWidget_func(gpointer w);

#define GtkObject_val(val) check_cast(GTK_OBJECT,val)
#define GtkWidget_val(val) check_cast(GTK_WIDGET,val)
#define GtkAdjustment_val(val) check_cast(GTK_ADJUSTMENT,val)
#define GtkItem_val(val) check_cast(GTK_ITEM,val)
#define GtkTooltips_val(val) check_cast(GTK_TOOLTIPS,val)

#define GtkClipboard_val(val) ((GtkClipboard*)Pointer_val(val))
#define GtkWindow_val(val) check_cast(GTK_WINDOW,val)
#define GtkTooltip_val(val) check_cast(GTK_TOOLTIP,val)

CAMLprim int Flags_Target_flags_val (value list);
