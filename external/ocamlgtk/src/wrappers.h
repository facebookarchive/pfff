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

/* $Id: wrappers.h 1467 2009-08-29 21:28:12Z ben_99_9 $ */

#ifndef _wrappers_
#define _wrappers_

/* Yell if a caml callback raised an exception */
#define CAML_EXN_LOG(name) g_critical("%s: callback raised an exception", name)
#define CAML_EXN_LOG_VERBOSE(name,exn) g_critical("%s: callback raised exception %s", name, format_caml_exception(Extract_exception(exn)))

#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/custom.h>
CAMLextern char *young_start, *young_end; /* from minor_gc.h */

CAMLexport value copy_memblock_indirected (void *src, asize_t size);
value alloc_memblock_indirected (asize_t size);
CAMLprim value ml_some (value);
value ml_cons (value, value);
CAMLexport void ml_raise_null_pointer (void) Noreturn;
CAMLexport value Val_pointer (void *);
CAMLprim value copy_string_check (const char*);
value copy_string_or_null (const char *);

value string_list_of_strv (const char * const *v);
value string_list_of_strv2 (char **v);
char ** strv_of_string_list (value list);

CAMLprim value *ml_global_root_new (value v);
CAMLexport void ml_global_root_destroy (void *data);

/* enums <-> polymorphic variants */
typedef struct { value key; int data; } lookup_info;
CAMLexport value ml_lookup_from_c (const lookup_info table[], int data);
CAMLexport int ml_lookup_to_c (const lookup_info table[], value key);
CAMLexport value ml_lookup_flags_getter (const lookup_info table[], int data);

/* Compatibility */
#include <gtk/gtkversion.h>
#if GTK_CHECK_VERSION(2,2,0) && !defined(DISABLE_GTK22)
#define HASGTK22
#endif
#if GTK_CHECK_VERSION(2,4,0) && !defined(DISABLE_GTK24)
#define HASGTK24
#endif
#if GTK_CHECK_VERSION(2,5,3) && !defined(DISABLE_GTK26)
#define HASGTK26
#endif
#if GTK_CHECK_VERSION(2,8,0) && !defined(DISABLE_GTK28)
#define HASGTK28
#endif
#if GTK_CHECK_VERSION(2,10,0) && !defined(DISABLE_GTK210)
#define HASGTK210
#endif
#if GTK_CHECK_VERSION(2,12,0) && !defined(DISABLE_GTK212)
#define HASGTK212
#endif

/* Wrapper generators */

#define Unsupported_22(cname) \
CAMLprim value ml_##cname () \
{ failwith(#cname " unsupported in Gtk 2.x < 2.2"); }
#define Unsupported Unsupported_22
#define Unsupported_24(cname) \
CAMLprim value ml_##cname () \
{ failwith(#cname " unsupported in Gtk 2.x < 2.4"); }
#define Unsupported_26(cname) \
CAMLprim value ml_##cname () \
{ failwith(#cname " unsupported in Gtk 2.x < 2.6"); }
#define Unsupported_28(cname) \
CAMLprim value ml_##cname () \
{ failwith(#cname " unsupported in Gtk 2.x < 2.8"); }

#define Unsupported_210(cname) \
CAMLprim value ml_##cname () \
{ failwith(#cname " unsupported in Gtk 2.x < 2.10"); }
#define Unsupported_212(cname) \
CAMLprim value ml_##cname () \
{ failwith(#cname " unsupported in Gtk 2.x < 2.12"); }

#define ID(x) (x)

#define ML_0(cname, conv) \
CAMLprim value ml_##cname (value unit) { return conv (cname ()); }
#define ML_1(cname, conv1, conv) \
CAMLprim value ml_##cname (value arg1) { return conv (cname (conv1 (arg1))); }
#define ML_1_post(cname, conv1, conv, post) \
CAMLprim value ml_##cname (value arg1) \
{ value ret = conv (cname (conv1(arg1))); post; return ret; }
#define ML_2(cname, conv1, conv2, conv) \
CAMLprim value ml_##cname (value arg1, value arg2) \
{ return conv (cname (conv1(arg1), conv2(arg2))); }
#define ML_2_name(mlname, cname, conv1, conv2, conv) \
CAMLprim value mlname (value arg1, value arg2) \
{ return conv (cname (conv1(arg1), conv2(arg2))); }
#define ML_3(cname, conv1, conv2, conv3, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3))); }
#define ML_3_name(mlname, cname, conv1, conv2, conv3, conv) \
CAMLprim value mlname (value arg1, value arg2, value arg3) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3))); }
#define ML_4(cname, conv1, conv2, conv3, conv4, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4))); }
#define ML_4_name(mlname, cname, conv1, conv2, conv3, conv4, conv) \
CAMLprim value mlname (value arg1, value arg2, value arg3, value arg4) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4))); }
#define ML_5(cname, conv1, conv2, conv3, conv4, conv5, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5))); }
#define ML_5_name(mlname, cname, conv1, conv2, conv3, conv4, conv5, conv) \
CAMLprim value mlname (value arg1, value arg2, value arg3, value arg4, \
                       value arg5) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5))); }
#define ML_6(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5), conv6(arg6))); }
#define ML_7(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5), conv6(arg6), conv7(arg7))); }
#define ML_8(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv8, \
	     conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7, value arg8) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5), conv6(arg6), conv7(arg7), conv8(arg8))); }
#define ML_9(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv8, \
	      conv9, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7, value arg8, \
                           value arg9) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5), conv6(arg6), conv7(arg7), conv8(arg8), \
		      conv9(arg9))); }
#define ML_9_name(mlname, cname, conv1, conv2, conv3, conv4, conv5, conv6, \
                  conv7, conv8, conv9, conv) \
CAMLprim value mlname (value arg1, value arg2, value arg3, value arg4, \
                       value arg5, value arg6, value arg7, value arg8, \
                       value arg9) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5), conv6(arg6), conv7(arg7), conv8(arg8), \
		      conv9(arg9))); }
#define ML_10(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv8, \
	      conv9, conv10, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7, value arg8, \
                           value arg9, value arg10)\
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5), conv6(arg6), conv7(arg7), conv8(arg8), \
		      conv9(arg9), conv10(arg10))); }
#define ML_11(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv8, \
	      conv9, conv10, conv11, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7, value arg8, \
                           value arg9, value arg10, value arg11) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5), conv6(arg6), conv7(arg7), conv8(arg8), \
		      conv9(arg9), conv10(arg10), conv11(arg11))); }
#define ML_12(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv8, \
	      conv9, conv10, conv11, conv12, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7, value arg8, \
                           value arg9, value arg10, value arg11, value arg12) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5), conv6(arg6), conv7(arg7), conv8(arg8), \
		      conv9(arg9), conv10(arg10), conv11(arg11), \
		      conv12(arg12))); }
#define ML_13(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv8, \
	      conv9, conv10, conv11, conv12, conv13, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7, value arg8, \
                           value arg9, value arg10, value arg11, value arg12, \
                           value arg13) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5), conv6(arg6), conv7(arg7), conv8(arg8), \
		      conv9(arg9), conv10(arg10), conv11(arg11), \
		      conv12(arg12), conv13(arg13))); }

/* Use with care: needs the argument index */
#define Ignore(x)
#define Insert(x) (x),
#define Split(x,f,g) f(x), g(x) Ignore
#define Split3(x,f,g,h) f(x), g(x), h(x) Ignore
#define Pair(x,f,g) f(Field(x,0)), g(Field(x,1)) Ignore
#define Triple(x,f,g,h) f(Field(x,0)), g(Field(x,1)), h(Field(x,2)) Ignore

/* For more than 5 arguments */
#define ML_bc6(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5]); }
#define ML_bc7(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6]); }
#define ML_bc8(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6], \
	       argv[7]); }
#define ML_bc9(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6], \
	       argv[7],argv[8]); }
#define ML_bc10(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6], \
	       argv[7],argv[8],argv[9]); }
#define ML_bc11(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6], \
	       argv[7],argv[8],argv[9],argv[10]); }
#define ML_bc12(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6], \
	       argv[7],argv[8],argv[9],argv[10],argv[11]); }
#define ML_bc13(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6], \
	       argv[7],argv[8],argv[9],argv[10],argv[11],argv[12]); }

/* result conversion */
#define Unit(x) ((x), Val_unit)
#define Id(x) x
#define Val_char Val_int

/* parameter conversion */
#define Bool_ptr(x) ((long) x - 1)
#define Char_val Int_val
#define Float_val(x) ((float)Double_val(x))
#define SizedString_val(x) String_val(x), string_length(x)

#define Option_val(val,unwrap,default) \
((long)val-1 ? unwrap(Field(val,0)) : default)
#define String_option_val(s) Option_val(s,String_val,NULL)

/* Strings are not always old, so they may move around... */
/* problems with alloca on Linux
#define StableString_val(val) \
  ((char*)(val) < young_end && (char*)(val) > young_start ? \
   memcpy(alloca(Bosize_val(val)), (char*)(val), Bosize_val(val)) : \
   String_val(val))
*/

/* Utility */

#define Copy_array(ret,l,src,conv) \
 if (!l) ret = Atom(0); \
 else if (l <= Max_young_wosize) { int i; ret = alloc_tuple(l); \
   for(i=0;i<l;i++) Field(ret,i) = conv(src[i]); } \
 else { int i; ret = alloc_shr(l,0); \
   for(i=0;i<l;i++) initialize (&Field(ret,i), conv(src[i])); }

#define Make_Val_final_pointer(type, init, final, adv) \
static void ml_final_##type (value val) \
{ if (Field(val,1)) final ((type*)Field(val,1)); } \
static struct custom_operations ml_custom_##type = \
{ #type"/2.0/", ml_final_##type, custom_compare_default, \
  custom_hash_default, custom_serialize_default, custom_deserialize_default };\
CAMLprim value Val_##type (type *p) \
{ value ret; if (!p) ml_raise_null_pointer(); \
  ret = alloc_custom (&ml_custom_##type, sizeof(value), adv, 1000); \
  initialize (&Field(ret,1), (value) p); init(p); return ret; }

#define Make_Val_final_pointer_ext(type, ext, init, final, adv) \
static void ml_final_##type##ext (value val) \
{ if (Field(val,1)) final ((type*)Field(val,1)); } \
static struct custom_operations ml_custom_##type##ext = \
{ #type#ext"/2.0/", ml_final_##type##ext, custom_compare_default, \
  custom_hash_default, custom_serialize_default, custom_deserialize_default };\
CAMLprim value Val_##type##ext (type *p) \
{ value ret; if (!p) ml_raise_null_pointer(); \
  ret = alloc_custom (&ml_custom_##type##ext, sizeof(value), adv, 1000); \
  initialize (&Field(ret,1), (value) p); init(p); return ret; }

#define Make_Val_final_pointer_compare(type, init, comp, final, adv) \
static void ml_final_##type (value val) \
{ if (Field(val,1)) final ((type*)Field(val,1)); } \
static int ml_comp_##type(value v1, value v2) \
{ return comp((type*)Field(v1,1), (type*)Field(v2,1)); } \
static struct custom_operations ml_custom_##type = \
{ #type"/2.0/", ml_final_##type, ml_comp_##type, \
  custom_hash_default, custom_serialize_default, custom_deserialize_default };\
CAMLprim value Val_##type (type *p) \
{ value ret; if (!p) ml_raise_null_pointer(); \
  ret = alloc_custom (&ml_custom_##type, sizeof(value), adv, 1000); \
  initialize (&Field(ret,1), (value) p); init(p); return ret; }

#define Pointer_val(val) ((void*)Field(val,1))
#define Store_pointer(val,p) (Field(val,1)=Val_bp(p))
#define MLPointer_val(val) \
        ((int)Field(val,1) == 2 ? &Field(val,2) : (void*)Field(val,1))

#define Val_addr(ptr) (1+(value)ptr)
#define Addr_val(val) ((void*)(val-1))

#define Wosize_asize(x) ((x-1)/sizeof(value)+1)
#define Wosizeof(x) Wosize_asize(sizeof(x))

#define Make_Extractor(name,conv1,field,conv2) \
CAMLprim value ml_##name##_##field (value val) \
{ return conv2 ((conv1(val))->field); }

#define Make_Setter(name,conv1,conv2,field) \
CAMLprim value ml_##name##_##field (value val, value new) \
{ (conv1(val))->field = conv2(new); return Val_unit; }

#define Make_Array_Extractor(name,conv1,conv2,field,conv) \
CAMLprim value ml_##name##_##field (value val, value index) \
{ return conv ((conv1(val))->field[conv2(index)]); }

#define Make_Array_Setter(name,conv1,conv2,conv3,field) \
CAMLprim value ml_##name##_##field (value val, value index, value new) \
{ (conv1(val))->field[conv2(index)] = conv3(new); return Val_unit; }

/* ML value is [flag list] */
#define Make_Flags_val(conv) \
CAMLprim int Flags_##conv (value list) \
{ int flags = 0L; \
  while(Is_block(list)){ flags |= conv(Field(list,0)); list = Field(list,1); }\
  return flags; }

/* ML value is [flag list option] */
#define Make_OptFlags_val(conv) \
CAMLprim int OptFlags_##conv (value list) \
{ int flags = 0L; \
  if (Is_block(list)) list = Field(list,0); \
  while(Is_block(list)){ flags |= conv(Field(list,0)); list = Field(list,1); }\
  return flags; }

#define Val_copy(val) copy_memblock_indirected (&val, sizeof(val))
#define Val_string copy_string_check
#define Val_optstring copy_string_or_null
#define Optstring_val(v) (string_length(v) ? String_val(v) : (char*)NULL)
#define Val_option(v,f) (v ? ml_some(f(v)) : Val_unit)

#define Check_null(v) (v ? v : (ml_raise_null_pointer (), v))

#define Val_nativeint copy_nativeint
#define Val_int64 copy_int64

#endif /* _wrappers_ */
