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

/* $Id: ml_gdk.h 1402 2008-03-25 08:55:03Z garrigue $ */

#define GdkAtom_val(val) ((GdkAtom)Long_val(val))
#define Val_GdkAtom(val) (Val_long((long)val))

#define GdkColormap_val(val) check_cast(GDK_COLORMAP,val)
#define Val_GdkColormap Val_GAnyObject

#define GdkColor_val(val) ((GdkColor*)MLPointer_val(val))
#define Val_GdkColor Val_pointer

#define GdkRectangle_val(val) ((GdkRectangle*)MLPointer_val(val))
#define Val_GdkRectangle Val_pointer

#define GdkDrawable_val(val) check_cast(GDK_DRAWABLE,val)

#define GdkWindow_val(val) check_cast(GDK_WINDOW,val)
#define Val_GdkWindow Val_GAnyObject

#define GdkCursor_val(val) ((GdkCursor*)Pointer_val(val))

#define GdkDisplay_val(val) ((GdkDisplay*) val)
#define Val_GdkDisplay(display) ((value) display)

CAMLexport GdkPixmap *GdkPixmap_val (value);  /* check argument */
#define Val_GdkPixmap Val_GAnyObject
#define Val_GdkPixmap_no_ref Val_GAnyObject_new

#define GdkBitmap_val(val) ((GdkBitmap*)GdkPixmap_val(val))
#define Val_GdkBitmap Val_GdkPixmap
#define Val_GdkBitmap_no_ref Val_GdkPixmap_no_ref

#ifndef UnsafeImage
CAMLexport GdkImage *GdkImage_val (value);  /* check argument */
#else
#define GdkImage_val(val) check_cast(GDK_IMAGE,val)
#endif
#define Val_GdkImage Val_GAnyObject
#define Val_GdkImage_new Val_GAnyObject_new

#define GdkFont_val(val) ((GdkFont*)Pointer_val(val))
CAMLexport value Val_GdkFont (GdkFont *);

CAMLexport GdkRegion *GdkRegion_val (value); /* check argument */
CAMLexport value Val_GdkRegion (GdkRegion *); /* finalizer is destroy! */

#define GdkGC_val(val) check_cast(GDK_GC,val)
#define Val_GdkGC Val_GAnyObject
#define Val_GdkGC_no_ref Val_GAnyObject_new

#define GdkEvent_val (GdkEvent*)MLPointer_val
CAMLexport value Val_GdkEvent (GdkEvent *);

#define GdkVisual_val(val) ((GdkVisual*) val)
#define Val_GdkVisual(visual) ((value) visual)

#define GdkScreen_val(val) check_cast(GDK_SCREEN,val)
#define Val_GdkScreen Val_GAnyObject

#define GdkDevice_val(val) ((GdkDevice*) val)
#define Val_GdkDevice(device) ((value) device)

#if 0 
// Future replacement for XID?
#ifdef GDK_NATIVE_WINDOW_POINTER
#define GdkNativeWindow_val (GdkNativeWindow*)
#define Val_GdkNativeWindow(id) (value)
#else
#define Val_GdkNativeWindow(id) copy_int32((long) id)
#define GdkNativeWindow_val Int32_val
#endif
#endif

#ifdef _WIN32
#define Val_XID(id) copy_int32((long) id)
#else
#define Val_XID copy_int32
#endif
#define XID_val Int32_val


CAMLexport int OptFlags_GdkModifier_val (value);
CAMLexport int Flags_GdkModifier_val (value);
CAMLexport int Flags_Event_mask_val (value);
CAMLexport lookup_info *ml_table_extension_events;
#define Extension_events_val(key) ml_lookup_to_c(ml_table_extension_events,key)

#define GdkDragContext_val(val) check_cast(GDK_DRAG_CONTEXT,val)
#define Val_GdkDragContext Val_GAnyObject
CAMLexport int Flags_GdkDragAction_val (value);
