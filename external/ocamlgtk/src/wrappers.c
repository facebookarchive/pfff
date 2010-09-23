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

/* $Id: wrappers.c 1467 2009-08-29 21:28:12Z ben_99_9 $ */

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <glib.h>

#include "wrappers.h"

CAMLexport value copy_memblock_indirected (void *src, asize_t size)
{
    mlsize_t wosize = Wosize_asize(size);
    value ret;
    if (!src) ml_raise_null_pointer ();
    ret = alloc_shr (wosize+2, Abstract_tag);
    Field(ret,1) = (value)2;
    memcpy ((value *) ret + 2, src, size);
    return ret;
}

value alloc_memblock_indirected (asize_t size)
{
    value ret = alloc_shr (Wosize_asize(size)+2, Abstract_tag);
    Field(ret,1) = (value)2;
    return ret;
}

CAMLprim value ml_some (value v)
{
     CAMLparam1(v);
     value ret = alloc_small(1,0);
     Field(ret,0) = v;
     CAMLreturn(ret);
}

value ml_cons (value v, value l)
{
  CAMLparam2(v, l);
  CAMLlocal1(cell);
  cell = alloc_small(2, Tag_cons);
  Field(cell, 0) = v;
  Field(cell, 1) = l;
  CAMLreturn(cell);
}

void ml_raise_null_pointer ()
{
  static value * exn = NULL;
  if (exn == NULL)
      exn = caml_named_value ("null_pointer");
  raise_constant (*exn);
}   

CAMLexport value Val_pointer (void *ptr)
{
    value ret = alloc_small (2, Abstract_tag);
    if (!ptr) ml_raise_null_pointer ();
    Field(ret,1) = (value)ptr;
    return ret;
}

CAMLprim value copy_string_check (const char*str)
{
    if (!str) ml_raise_null_pointer ();
    return copy_string ((char*) str);
}

value copy_string_or_null (const char*str)
{
    return copy_string (str ? (char*) str : "");
}

CAMLprim value *ml_global_root_new (value v)
{
    value *p = stat_alloc(sizeof(value));
    *p = v;
    register_global_root (p);
    return p;
}

CAMLexport void ml_global_root_destroy (void *data)
{
    remove_global_root ((value *)data);
    stat_free (data);
}

CAMLexport value ml_lookup_from_c (const lookup_info table[], int data)
{
    int i;
    for (i = table[0].data; i > 0; i--)
	if (table[i].data == data) return table[i].key;
    invalid_argument ("ml_lookup_from_c");
}
    
CAMLexport int ml_lookup_to_c (const lookup_info table[], value key)
{
    int first = 1, last = table[0].data, current;
    while (first < last) {
	current = (first+last)/2;
	if (table[current].key >= key) last = current;
	else first = current + 1;
    }
    if (table[first].key == key) return table[first].data;
    invalid_argument ("ml_lookup_to_c");
}

CAMLexport value ml_lookup_flags_getter (const lookup_info table[], int data)
{
  CAMLparam0();
  CAMLlocal2(cell, l);
  int i;
  l = Val_emptylist;
  for (i = table[0].data; i > 0; i--)
    if ((table[i].data & data) == table[i].data) {
      cell = alloc_small(2, Tag_cons);
      Field(cell, 0) = table[i].key;
      Field(cell, 1) = l;
      l = cell;
    }
  CAMLreturn(l);
}

ML_2 (ml_lookup_from_c, (lookup_info*), Int_val, 0+)
ML_2 (ml_lookup_to_c, (lookup_info*), 0+, Val_int)

gchar **
strv_of_string_list (value list)
{
  gchar **str_v;
  gsize i, len;
  value l;
  for (len = 0, l = list; l != Val_emptylist; len++, l = Field (l, 1))
    ;
  l = list;
  str_v = g_new (gchar *, len+1);
  for (i = 0; i < len; i++)
    {
      str_v[i] = g_strdup (String_val (Field (l, 0)));
      l = Field (l, 1);
    }
  str_v[len] = NULL;
  return str_v;
}

value
string_list_of_strv (const gchar * const *v)
{
  CAMLparam0();
  CAMLlocal4(head, l, cell, s);
  gsize i;
  if (v == NULL)
    CAMLreturn (Val_emptylist);
  i = 0;
  head = l = Val_emptylist;
  while (v[i] != NULL)
    {
      s = copy_string (v[i]);
      cell = alloc_small (2, Tag_cons);
      Field (cell, 0) = s;
      Field (cell, 1) = Val_emptylist;
      if (l == Val_emptylist)
	  head = l = cell;
      else
	{
	  Field (l, 1) = cell;
	  l = cell;
	}
      i++;
    }
  CAMLreturn (head);
}

value
string_list_of_strv2 (gchar **v) {
  return string_list_of_strv((const gchar* const*)v);
}



#ifdef ABSVALUE
CAMLexport intnat Long_val(value x)  { return (intnat)x >> 1; }
CAMLexport value  Val_long(intnat x) { return (value)((x << 1) + 1); }
CAMLexport int Is_long(value x)   { return ((intnat)(x) & 1) != 0; }
CAMLexport int Is_block(value x)  { return ((intnat)(x) & 1) == 0; }
#endif
