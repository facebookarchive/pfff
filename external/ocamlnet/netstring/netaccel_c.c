/* $Id: netaccel_c.c 798 2004-07-08 22:11:07Z stolpmann $
 *
 * Accelerators, especially for bytecode
 */

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/callback.h"

/* Accelerator for Netaux.ArrayAux.int_blit */

value netstring_int_blit_ml (value src, value srcpos, 
			     value dest, value destpos, 
			     value len) {
    long srcpos_c, destpos_c, len_c, i;
    CAMLparam5(src,srcpos,dest,destpos,len);

    srcpos_c = Long_val(srcpos);
    destpos_c = Long_val(destpos);
    len_c = Long_val(len);

    if (len_c < 0 || srcpos_c < 0 || 
	srcpos_c+len_c > Wosize_val(src) ||
	destpos_c < 0 ||
	destpos_c+len_c > Wosize_val(dest))
	invalid_argument("Netaccel.int_blit");
    
    if (src != dest || destpos_c <= srcpos_c) {
	for (i=0; i<len_c; i++) {
	    Field(dest, destpos_c + i) = Field(src, srcpos_c + i);
	}
    }
    else {
	for (i=len_c-1; i>=0; i--) {
	    Field(dest, destpos_c + i) = Field(src, srcpos_c + i);
	}
    }

    CAMLreturn(Val_unit);
}

/* Accelerator for Netaux.ArrayAux.int_series */

value netstring_int_series_ml (value src, value srcpos, 
			       value dest, value destpos, 
			       value len, value n) {
    long srcpos_c, destpos_c, len_c, n_c, i, s;
    CAMLparam5(src,srcpos,dest,destpos,len);
    CAMLxparam1(n);

    srcpos_c = Long_val(srcpos);
    destpos_c = Long_val(destpos);
    len_c = Long_val(len);
    n_c = Long_val(n);

    if (len_c < 0 || srcpos_c < 0 || 
	srcpos_c+len_c > Wosize_val(src) ||
	destpos_c < 0 ||
	destpos_c+len_c > Wosize_val(dest))
	invalid_argument("Netaccel.int_series");

    s = n_c;
    for (i=0; i<len_c; i++) {
	Field(dest, destpos_c + i) = Val_long(s);
	s += Long_val(Field(src, srcpos_c+i));
    }

    CAMLreturn(Val_unit);
}

value netstring_int_series_byte (value *argv, int argn) {
    return netstring_int_series_ml(argv[0], argv[1], argv[2], argv[3],
				   argv[4], argv[5]);
}

/* Accelerator for Netconversion.read_iso88591 */

/* The exception Netconversion.Malformed_code_read must have been
 * registered with Callback.
 */

value netstring_read_iso88591_ml (value maxcode, value enc,
				  value slice_char,
				  value slice_blen,
				  value s_in, value p_in, value l_in) {
    long maxcode_c, p_in_c, l_in_c;
    long slice_char_len, m, k;
    unsigned char ch;
    CAMLparam5(maxcode, enc, slice_char, slice_blen, s_in);
    CAMLxparam2(p_in, l_in);
    CAMLlocal1(r);
    
    maxcode_c = Long_val(maxcode);
    p_in_c = Long_val(p_in);
    l_in_c = Long_val(l_in);
    
    slice_char_len = Wosize_val(slice_char);

    if (slice_char_len != Wosize_val(slice_blen)) 
	invalid_argument("Netaccel.read_iso88591");
    if (p_in_c < 0 || l_in_c < 0 || p_in_c + l_in_c > string_length(s_in)) 
	invalid_argument("Netaccel.read_iso88591");

    m = l_in_c;
    if (slice_char_len < m) m = slice_char_len;

    for (k=0; k<m; k++) {
	ch = Byte_u(s_in, p_in_c+k);
	if (ch > maxcode_c) {
	    Field(slice_char, k) = Val_long(-1);
	    r = alloc_tuple(3);
	    Store_field(r, 0, Val_long(k));
	    Store_field(r, 1, Val_long(k));
	    Store_field(r, 2, enc);
	    raise_with_arg(*caml_named_value("Netconversion.Malformed_code_read"),
			   r);
	};
	Field(slice_char, k) = Val_int((signed int) ch);
    };

    if (m < slice_char_len) {
	Field(slice_char, m) = Val_long(-1);
    };

    r = alloc_tuple(3);
    Store_field(r, 0, Val_long(m));
    Store_field(r, 1, Val_long(m));
    Store_field(r, 2, enc);

    CAMLreturn(r);
}


value netstring_read_iso88591_byte (value *argv, int argn) {
    return netstring_read_iso88591_ml(argv[0], argv[1], argv[2], argv[3],
				      argv[4], argv[5], argv[6]);
}


/* Accelerator for Netconversion.read_utf8 */

/* The exception Netconversion.Malformed_code_read must have been
 * registered with Callback.
 */

value netstring_read_utf8_ml (value is_java,
			      value slice_char,
			      value slice_blen,
			      value s_in, value p_in, value l_in) {
    long is_java_c, p_in_c, l_in_c;
    long slice_char_len, p, p_max, n_ret, n;
    long k_inc, cp;
    unsigned char ch, ch2, ch3, ch4;
    CAMLparam5(is_java, slice_char, slice_blen, s_in, p_in);
    CAMLxparam1(l_in);
    CAMLlocal1(r);
    
    is_java_c = Long_val(is_java);
    p_in_c = Long_val(p_in);
    l_in_c = Long_val(l_in);
    
    slice_char_len = Wosize_val(slice_char);

    if (slice_char_len != Wosize_val(slice_blen)) 
	invalid_argument("Netaccel.read_utf8");
    if (p_in_c < 0 || l_in_c < 0 || p_in_c + l_in_c > string_length(s_in)) 
	invalid_argument("Netaccel.read_utf8");

    p = p_in_c;
    p_max = p_in_c + l_in_c;
    n = 0;
    n_ret = (-1);
    
    while (p < p_max && n < slice_char_len) {
	k_inc = 0;
	ch = Byte_u(s_in, p);
	if (ch == 0) {
	    if (is_java_c) goto malformed_code;
	    Field(slice_char, n) = Val_int(0);
	    k_inc = 1;
	}
	else if (ch <= 127) {
	    Field(slice_char, n) = Val_int((int) ch);
	    k_inc = 1;
	}
	else if (ch <= 223) {
	    if (p+1 < p_max) {
		ch2 = Byte_u(s_in, p+1);
		if (is_java_c && ch == 0x80 && ch2 == 0xc0) {
		    Field(slice_char, n) = Val_int(0);
		    k_inc = 2;
		}
		else {
		    if (ch2 < 0x80 || ch2 >= 0xc0) goto malformed_code;
		    cp = ((ch & 0x1f) << 6) | (ch2 & 0x3f);
		    if (cp < 0x80) goto malformed_code;
		    Field(slice_char, n) = Val_int((int) cp);
		    k_inc = 2;
		}
	    }
	}
	else if (ch <= 239) {
	    if (p+2 < p_max) {
		ch2 = Byte_u(s_in, p+1);
		ch3 = Byte_u(s_in, p+2);
		if (ch2 < 0x80 || ch2 >= 0xc0) goto malformed_code;
		if (ch3 < 0x80 || ch3 >= 0xc0) goto malformed_code;
		cp = ((ch & 0xf) << 12) | ((ch2 & 0x3f) << 6) | (ch3 & 0x3f);
		if (cp < 0x800) goto malformed_code;
		if (cp >= 0xd800 && cp < 0xe000) goto malformed_code;
		if (cp >= 0xfffe && cp <= 0xffff) goto malformed_code;
		Field(slice_char, n) = Val_int((int) cp);
		k_inc = 3;
	    }
	}
	else if (ch <= 247) {
	    if (p+3 < p_max) {
		ch2 = Byte_u(s_in, p+1);
		ch3 = Byte_u(s_in, p+2);
		ch4 = Byte_u(s_in, p+3);
		if (ch2 < 0x80 || ch2 >= 0xc0) goto malformed_code;
		if (ch3 < 0x80 || ch3 >= 0xc0) goto malformed_code;
		if (ch4 < 0x80 || ch4 >= 0xc0) goto malformed_code;
		cp = ((ch & 7) << 18) | ((ch2 & 0x3f) << 12) | 
		    ((ch3 & 0x3f) << 6) | (ch4 & 0x3f);
		if (cp < 0x10000) goto malformed_code;
		if (cp >= 0x110000) goto malformed_code;
		Field(slice_char, n) = Val_int((int) cp);
		k_inc = 4;
	    }
	}
	else goto malformed_code;

	if (k_inc > 0) {
	    Field(slice_blen, n) = Val_int((int) k_inc);
	    p += k_inc;
	    n++;
	}
	else {
	    n_ret = n;
	    n = slice_char_len;
	}
    };

    if (n_ret == (-1)) n_ret = n;
    if (n_ret < slice_char_len) {
	Field(slice_char, n_ret) = Val_long(-1);
    }

    r = alloc_tuple(3);
    Store_field(r, 0, Val_long(n_ret));
    Store_field(r, 1, Val_long(p-p_in_c));
    Store_field(r, 2, hash_variant("Enc_utf8"));

    CAMLreturn(r);

 malformed_code:
    Field(slice_char, n) = Val_long(-1);
    r = alloc_tuple(3);
    Store_field(r, 0, Val_long(n));
    Store_field(r, 1, Val_long(p-p_in_c));
    Store_field(r, 2, hash_variant("Enc_utf8"));
    raise_with_arg(*caml_named_value("Netconversion.Malformed_code_read"),
		   r);

    /* Cannot reach this point! */
    CAMLreturn(Val_unit);
}

value netstring_read_utf8_byte (value *argv, int argn) {
    return netstring_read_utf8_ml(argv[0], argv[1], argv[2], argv[3],
				  argv[4], argv[5]);
}
