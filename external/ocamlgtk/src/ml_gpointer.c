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

/* $Id: ml_gpointer.c 1347 2007-06-20 07:40:34Z guesdon $ */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <string.h>

#include "wrappers.h"
#include "ml_gpointer.h"

CAMLprim value ml_stable_copy (value v)
{
    if (Is_block(v) && (char*)(v) < young_end && (char*)(v) > young_start)
    {
        CAMLparam1(v);
        mlsize_t i, wosize = Wosize_val(v);
        int tag = Tag_val(v);
        value ret;
        if (tag < No_scan_tag) invalid_argument("ml_stable_copy");
        ret = alloc_shr (wosize, tag);
        for (i=0; i < wosize; i++) Field(ret,i) = Field(v,i);
        CAMLreturn(ret);
    }
    return v;
}
CAMLprim value ml_string_at_pointer (value ofs, value len, value ptr)
{
    char *start = ((char*)Pointer_val(ptr)) + Option_val(ofs, Int_val, 0);
    int length = Option_val(len, Int_val, strlen(start));
    value ret = alloc_string(length);
    memcpy ((char*)ret, start, length);
    return ret;
}

CAMLprim value ml_int_at_pointer (value ptr)
{
    return Val_int(*(int*)Pointer_val(ptr));
}

CAMLprim value ml_set_int_at_pointer (value ptr, value n)
{
    *(int*)Pointer_val(ptr) = Int_val(n);
    return Val_unit;
}

CAMLprim value ml_long_at_pointer (value ptr)
{
    return copy_nativeint(*(long*)Pointer_val(ptr));
}

CAMLprim value ml_set_long_at_pointer (value ptr, value n)
{
    *(long*)Pointer_val(ptr) = Nativeint_val(n);
    return Val_unit;
}

CAMLexport unsigned char* ml_gpointer_base (value region)
{
    unsigned int i;
    value ptr = RegData_val(region);
    value path = RegPath_val(region);

    if (Is_block(path))
        for (i = 0; i < Wosize_val(path); i++)
            ptr = Field(ptr, Int_val(Field(path, i)));

    return (unsigned char*) ptr+RegOffset_val(region);
}

CAMLprim value ml_gpointer_get_char (value region, value pos)
{
    return Val_int(*(ml_gpointer_base (region) + Long_val(pos)));
}

CAMLprim value ml_gpointer_set_char (value region, value pos, value ch)
{
    *(ml_gpointer_base (region) + Long_val(pos)) = Int_val(ch);
    return Val_unit;
}

CAMLprim value ml_gpointer_blit (value region1, value region2)
{
    void *base1 = ml_gpointer_base (region1);
    void *base2 = ml_gpointer_base (region2);

    memcpy (base2, base1, RegLength_val(region1));
    return Val_unit;
}

CAMLprim value ml_gpointer_get_addr (value region)
{
    return copy_nativeint ((long)ml_gpointer_base (region));
}
