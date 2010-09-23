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

/* $Id: ml_pango.h 1501 2010-04-11 21:07:18Z oandrieu $ */

#include "pango_tags.h"

#define PangoFontDescription_val(val) ((PangoFontDescription*)Pointer_val(val))
CAMLexport value Val_PangoFontDescription_new(PangoFontDescription* it);
#define Val_PangoFontDescription(desc) \
  (Val_PangoFontDescription_new(pango_font_description_copy(desc)))

CAMLexport value ml_PangoStyle_Val (value val);

#define Val_PangoLanguage Val_pointer
#define PangoLanguage_val Pointer_val

#define PangoContext_val(val) check_cast(PANGO_CONTEXT,val)
#define Val_PangoContext Val_GAnyObject
#define Val_PangoContext_new Val_GAnyObject_new

#define PangoFont_val(val) check_cast(PANGO_FONT, val)
#define Val_PangoFont Val_GAnyObject

#define PangoFontMetrics_val(val) ((PangoFontMetrics*)Pointer_val(val))

#define PangoLayout_val(val) check_cast(PANGO_LAYOUT, val)
#define Val_PangoLayout Val_GAnyObject

#define PangoFontMap_val(val) check_cast(PANGO_FONT_MAP, val)
#define Val_PangoFontMap Val_GAnyObject

CAMLexport value Val_PangoRectangle(PangoRectangle *rect);
