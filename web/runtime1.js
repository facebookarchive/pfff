
//Provides: unix_inet_addr_of_string 
function unix_inet_addr_of_string(x) {
  // see unix.ml
  if (x == "0.0.0.0" || x == "127.0.0.1") {
    return x;
  } else {
    caml_failwith("unix_inet_addr_of_string: " + x); 
  }
}

//Provides: unix_gettimeofday
function unix_gettimeofday(x) { caml_failwith("unix_gettimeofday"); }

////Provides: caml_sys_exit
//function caml_sys_exit(x) { caml_failwith("caml_sys_exit"); }

////Provides: caml_ml_output_char
//function caml_ml_output_char(x) { 
//  caml_failwith("caml_ml_output_char");
//}

// Js_of_ocaml toplevel runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2011 Jérôme Vouillon
// Laboratoire PPS - CNRS Université Paris Diderot
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

//Provides: caml_raise_sys_error
//Requires: caml_raise_with_string, caml_global_data
function caml_raise_sys_error (msg) {
  caml_raise_with_string(caml_global_data[2], msg);
}

//Provides: caml_raise_not_found
//Requires: caml_raise_constant, caml_global_data
function caml_raise_not_found () { caml_raise_constant(caml_global_data[7]); }

//Provides: caml_sys_getenv
//Requires: caml_raise_not_found
function caml_sys_getenv () { caml_raise_not_found (); }

//Provides: caml_terminfo_setup
function caml_terminfo_setup () { return 1; } // Bad_term

//////////////////////////////////////////////////////////////////////

// This is to read cmi files, included in the Javascript code

//Provides: caml_sys_file_exists
//Requires: caml_global_data
function caml_sys_file_exists (x) { return (caml_global_data.interfaces[x])?1:0; }

//Provides: caml_sys_open
//Requires: MlString, caml_raise_sys_error, caml_global_data
function caml_sys_open (x) {
  var v = caml_global_data.interfaces[x];
  if (v) {
    var s = new MlString (v);
    s.offset = 0;
    return s;
  } else
    caml_raise_sys_error (x + ": no such file or directory");
}

//Provides: caml_ml_open_descriptor_in
function caml_ml_open_descriptor_in (x) { return x; }

//Provides: caml_ml_input
//Require: caml_blit_string
function caml_ml_input (f, s, i, l) {
  var l2 = f.getLen() - f.offset;
  if (l2 < l) l = l2;
  caml_blit_string(f, f.offset, s, i, l);
  f.offset += l;
  return l;
}

//Provides: caml_input_value
//Requires: caml_marshal_data_size, caml_input_value_from_string
function caml_input_value (s) {
  caml_marshal_data_size (s, s.offset);
  return caml_input_value_from_string(s, s.offset);
}

//Provides: caml_ml_close_channel
function caml_ml_close_channel () { return 0;}

//////////////////////////////////////////////////////////////////////

//Provides: caml_get_global_data
//Requires: caml_global_data
function caml_get_global_data () { return caml_global_data; }

//Provides: caml_get_section_table
//Requires: caml_global_data
function caml_get_section_table () { return caml_global_data.toc; }

//Provides: caml_dynlink_get_current_libs
function caml_dynlink_get_current_libs () { return [0]; }

//Provides: caml_reify_bytecode
//Requires: caml_global_data
function caml_reify_bytecode (code, sz) {
  return eval(caml_global_data.compile(code).toString());
}

//Provides: caml_static_release_bytecode
function caml_static_release_bytecode () { return 0; }

//Provides: caml_static_alloc
//Requires: MlString
function caml_static_alloc (len) { return new MlMakeString (len); }

//Provides: caml_static_free
function caml_static_free () { return 0; }

//Provides: caml_realloc_global
//Requires: caml_global_data
function caml_realloc_global (len) {
  if (len + 1 > caml_global_data.length) caml_global_data.length = len + 1;
  return 0;
}

/////////////////////////////////////////////////////////////////////////

/// In case the user tries to perform some I/Os...

//Provides: caml_sys_exit
// function caml_sys_exit () { return 0; }
function caml_sys_exit () { caml_raise_sys_error("caml_sys_exit not implemented"); }

//Provides: caml_ml_output
function caml_ml_output (x, s, p, l) {
  var o = document.getElementById("output");
  o.appendChild (document.createTextNode(s.toString().slice(p,p+l)));
  return 0;
}

//Provides: caml_ml_output_char
//Requires: caml_ml_output
function caml_ml_output_char (x, c) {
    return caml_ml_output (x, String.fromCharCode (c), 0, 1);
}

//Provides: caml_raise_end_of_file
//Requires: caml_raise_constant, caml_global_data
function caml_raise_end_of_file () {
  caml_raise_constant(caml_global_data[5]);
}

//Provides: caml_ml_input_char
//Requires: caml_raise_end_of_file
function caml_ml_input_char (f) {
  caml_raise_end_of_file ();
}
