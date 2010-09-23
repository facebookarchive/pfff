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

#define GtkTextMark_val(val) check_cast(GTK_TEXT_MARK,val)
#define Val_GtkTextMark(val) (Val_GObject((GObject*)val))
#define Val_GtkTextMark_new(val) (Val_GObject_new((GObject*)val))

        
#define GtkTextTag_val(val) check_cast(GTK_TEXT_TAG,val)
#define Val_GtkTextTag(val) (Val_GObject((GObject*)val))
#define Val_GtkTextTag_new(val) (Val_GObject_new((GObject*)val))

#define GtkTextTagTable_val(val) check_cast(GTK_TEXT_TAG_TABLE,val)
#define Val_GtkTextTagTable(val)  (Val_GObject((GObject*)val))
#define Val_GtkTextTagTable_new(val) (Val_GObject_new((GObject*)val))

#define GtkTextBuffer_val(val) check_cast(GTK_TEXT_BUFFER,val)
#define Val_GtkTextBuffer(val)  (Val_GObject((GObject*)val))
#define Val_GtkTextBuffer_new(val) (Val_GObject_new((GObject*)val))

#define GtkTextChildAnchor_val(val) check_cast(GTK_TEXT_CHILD_ANCHOR,val)
#define Val_GtkTextChildAnchor(val)  (Val_GObject((GObject*)val))
#define Val_GtkTextChildAnchor_new(val) (Val_GObject_new((GObject*)val))


/* "Lighter" version: allocate in the ocaml heap (see ml_gtktext.c 
   for other definitions. */
#define GtkTextIter_val(val) ((GtkTextIter*)MLPointer_val(val))
#define Val_GtkTextIter(it) (copy_memblock_indirected(it,sizeof(GtkTextIter)))
#define alloc_GtkTextIter() (alloc_memblock_indirected(sizeof(GtkTextIter))

#define GtkTextView_val(val) check_cast(GTK_TEXT_VIEW,val)


