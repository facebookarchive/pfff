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

#define GtkTreeIter_val(val) ((GtkTreeIter*)MLPointer_val(val))
#define Val_GtkTreeIter(it) (copy_memblock_indirected(it,sizeof(GtkTreeIter)))
#define GtkTreeIter_optval(v) Option_val(v, GtkTreeIter_val, NULL)
#define GtkTreePath_optval(v) Option_val(v, GtkTreePath_val, NULL)
#define GtkTreeModel_optval(v) Option_val(v, GtkTreeModel_val, NULL)
#define GtkCellRenderer_optval(v) Option_val(v, GtkCellRenderer_val, NULL)
#define GtkTreeViewColumn_optval(v) Option_val(v, GtkTreeViewColumn_val, NULL)

gboolean ml_gtk_row_separator_func (GtkTreeModel *model,
				    GtkTreeIter *iter,
				    gpointer data);

