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

/* $Id: ml_glib.h 1362 2007-08-17 03:52:05Z garrigue $ */

CAMLexport value copy_string_g_free (char *str); /* for g_strings only */

typedef value (*value_in)(gpointer);
typedef gpointer (*value_out)(value); /* should not trigger GC */

CAMLexport value Val_GList (GList *list, value_in);
CAMLexport value Val_GList_free (GList *list, value_in);
CAMLexport GList *GList_val (value list, value_out);

CAMLexport value Val_GSList (GSList *list, value_in);
CAMLexport value Val_GSList_free (GSList *list, value_in);
CAMLexport GSList *GSList_val (value list, value_out);

CAMLexport void ml_register_exn_map (GQuark domain, char *caml_name);
CAMLexport void ml_raise_gerror(GError *) Noreturn;
