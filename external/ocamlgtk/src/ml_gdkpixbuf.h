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

/* $Id: ml_gdkpixbuf.h 1467 2009-08-29 21:28:12Z ben_99_9 $ */

#define GdkPixbuf_val(val)       (check_cast(GDK_PIXBUF, val))
CAMLexport value Val_GdkPixbuf_ (GdkPixbuf *, gboolean);
#define Val_GdkPixbuf(p)         Val_GdkPixbuf_(p, TRUE)
#define Val_option_GdkPixbuf(p)         Val_option(p,Val_GdkPixbuf)
#define Val_GdkPixbuf_new(p)     Val_GdkPixbuf_(p, FALSE)
