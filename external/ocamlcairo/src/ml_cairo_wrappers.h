/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#define wPointer_val(t, val)	(* ((t **) Data_custom_val(val)))

int ml_pointer_compare (value, value);
long ml_pointer_hash (value);

#define wMake_Val_final_pointer_full(type, final, adv, cmp_method, hash_method) \
static void ml_final_##type (value val) \
{ type **p = Data_custom_val(val); \
  if (*p) final (*p); } \
static struct custom_operations ml_custom_##type = \
{ #type "/001", ml_final_##type, \
  cmp_method, hash_method, \
  custom_serialize_default, custom_deserialize_default }; \
value Val_##type (type *p) \
{ type **store; value ret; \
  if (!p) report_null_pointer(); \
  ret = caml_alloc_custom (&ml_custom_##type, sizeof p, adv, 100); \
  store = Data_custom_val(ret); \
  *store = p; return ret; }

#define wMake_Val_final_pointer(type, final, adv) wMake_Val_final_pointer_full(type, final, adv, ml_pointer_compare, ml_pointer_hash)

#ifndef ARCH_ALIGN_DOUBLE
#define Double_array_val(v)    ((double *)(v))
#endif
#define Double_array_length(v) (Wosize_val(v) / Double_wosize)

#define Ignore(x)
#define Unit(x) ((x), Val_unit)

#define Cairo_Unsupported(fun, msg) \
CAMLprim value ml_##fun() { caml_failwith (msg); return Val_unit; }

#define wML_0(cname, conv) \
CAMLprim value ml_##cname (value unit) { return conv (cname ()); }
#define wML_1(cname, conv1, conv) \
CAMLprim value ml_##cname (value arg1) { return conv (cname (conv1 (arg1))); }
#define wML_1_post(cname, conv1, conv, t, post) \
CAMLprim value ml_##cname (value arg1) \
{ t ret = cname (conv1(arg1)); post; return conv(ret); }
#define wML_2(cname, conv1, conv2, conv) \
CAMLprim value ml_##cname (value arg1, value arg2) \
{ return conv (cname (conv1(arg1), conv2(arg2))); }
#define wML_2_post(cname, conv1, conv2, t, conv) \
CAMLprim value ml_##cname (value arg1, value arg2) \
{ t ret = cname (conv1(arg1), conv2(arg2)); post; return conv(ret); }
#define wML_3(cname, conv1, conv2, conv3, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3))); }
#define wML_3_post(cname, conv1, conv2, conv3, conv, t, post) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3) \
{ t ret = cname (conv1(arg1), conv2(arg2), conv3(arg3)); post; return conv(t); }
#define wML_4(cname, conv1, conv2, conv3, conv4, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4))); }
#define wML_4_post(cname, conv1, conv2, conv3, conv4, conv, t, post) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4) \
{ t ret = cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4)); post; return conv(ret); }
#define wML_5(cname, conv1, conv2, conv3, conv4, conv5, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5))); }
#define wML_5_post(cname, conv1, conv2, conv3, conv4, conv5, conv, t, post) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5) \
{ t ret = cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), conv5(arg5)); \
  post; return conv(ret); }
#define wML_6(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5), conv6(arg6))); }
#define wML_6_post(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv, t, post) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6) \
{ t ret = cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		 conv5(arg5), conv6(arg6)); post; return conv(ret); }
#define wML_7(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5), conv6(arg6), conv7(arg7))); }
#define wML_7_post(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv, t, post) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7) \
{ t ret = cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5), conv6(arg6), conv7(arg7)); post; return conv(ret); }


#define wML_bc6(cname) \
CAMLprim value ml_##cname##_bc (value *argv, int argn) \
{ return ml_##cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5]); }
#define wML_bc7(cname) \
CAMLprim value ml_##cname##_bc (value *argv, int argn) \
{ return ml_##cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6]); }

#ifndef W_CHECK_STATUS
# define W_CHECK_STATUS check_cairo_status
#endif
#ifndef W_CONV_CAIRO
# define W_CONV_CAIRO cairo_t_val
#endif

#define wML_0_cairo(cname) \
CAMLprim value ml_cairo_##cname (value v_cr) \
{ cairo_##cname (W_CONV_CAIRO(v_cr)); \
  W_CHECK_STATUS(v_cr); \
  return Val_unit; \
}
#define wML_1_cairo(cname, conv1) \
CAMLprim value ml_cairo_##cname (value v_cr, value arg1) \
{ cairo_##cname (W_CONV_CAIRO(v_cr), conv1 (arg1)); \
  W_CHECK_STATUS(v_cr); \
  return Val_unit; \
}
#define wML_2_cairo(cname, conv1, conv2) \
CAMLprim value ml_cairo_##cname (value v_cr, value arg1, value arg2) \
{ cairo_##cname (W_CONV_CAIRO(v_cr), conv1 (arg1), conv2 (arg2)); \
  W_CHECK_STATUS(v_cr); \
  return Val_unit; \
}
#define wML_3_cairo(cname, conv1, conv2, conv3) \
CAMLprim value ml_cairo_##cname (value v_cr, value arg1, value arg2, value arg3) \
{ cairo_##cname (W_CONV_CAIRO(v_cr), conv1 (arg1), conv2 (arg2), conv3 (arg3)); \
  W_CHECK_STATUS(v_cr); \
  return Val_unit; \
}
#define wML_4_cairo(cname, conv1, conv2, conv3, conv4) \
CAMLprim value ml_cairo_##cname (value v_cr, value arg1, value arg2, value arg3, value arg4) \
{ cairo_##cname (W_CONV_CAIRO(v_cr), conv1 (arg1), conv2 (arg2), conv3 (arg3), conv4 (arg4)); \
  W_CHECK_STATUS(v_cr); \
  return Val_unit; \
}
#define wML_5_cairo(cname, conv1, conv2, conv3, conv4, conv5) \
CAMLprim value ml_cairo_##cname (value v_cr, value arg1, value arg2, value arg3, value arg4, value arg5) \
{ cairo_##cname (W_CONV_CAIRO(v_cr), conv1 (arg1), conv2 (arg2), conv3 (arg3), conv4 (arg4), conv5 (arg5)); \
  W_CHECK_STATUS(v_cr); \
  return Val_unit; \
} \
CAMLprim value ml_cairo_##cname##_bc (value *argv, int argn) \
{ return ml_cairo_##cname (argv[0],argv[1],argv[2],argv[3],argv[4],argv[5]); }
#define wML_6_cairo(cname, conv1, conv2, conv3, conv4, conv5, conv6) \
CAMLprim value ml_cairo_##cname (value v_cr, value arg1, value arg2, value arg3, value arg4, value arg5, value arg6) \
{ cairo_##cname (W_CONV_CAIRO(v_cr), conv1 (arg1), conv2 (arg2), conv3 (arg3), conv4 (arg4), conv5 (arg5), conv6 (arg6)); \
  W_CHECK_STATUS(v_cr); \
  return Val_unit; \
} \
CAMLprim value ml_cairo_##cname##_bc (value *argv, int argn) \
{ return ml_cairo_##cname (argv[0],argv[1],argv[2],argv[3],argv[4],argv[5], argv[6]); }
