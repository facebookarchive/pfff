// Js_of_ocaml library
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2010 Jérôme Vouillon
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

///////////// Jslib: code specific to Js_of_ocaml

//Provides: caml_js_from_bool const
function caml_js_from_bool(x) { return !!x; }
//Provides: caml_js_to_bool const
function caml_js_to_bool(x) { return +x; }
//Provides: caml_js_from_float const
function caml_js_from_float(x) { return x; }
//Provides: caml_js_to_float const
function caml_js_to_float(x) { return x; }
//Provides: caml_js_from_string mutable
function caml_js_from_string(s) { return s.toString(); }
//Provides: caml_js_to_string const
//Requires: MlString
function caml_js_to_string(s) { return new MlWrappedString(s); }
//Provides: caml_js_from_array mutable
function caml_js_from_array(a) { return a.slice(1); }
//Provides: caml_js_to_array mutable
function caml_js_to_array(a) { return [0].concat(a); }

//Provides: caml_js_var mutable
function caml_js_var(x) { return eval(x.toString()); }
//Provides: caml_js_const const
function caml_js_const(x) {
  switch (caml_string_to_js(x)) {
  case "null": return null;
  case "true": return true;
  case "false": return false;
  // case "undefined: return undefined;
  }
}
//Provides: caml_js_call
function caml_js_call(f, o, args) { return f.apply(o, args.slice(1)); }
//Provides: caml_js_fun_call
function caml_js_fun_call(f, args) { return f.apply(null, args.slice(1)); }
//Provides: caml_js_meth_call
function caml_js_meth_call(o, f, args) { return o.f.apply(o, args.slice(1)); }
//Provides: caml_js_new
function caml_js_new(c, a) {
  switch (a.length) {
  case 1: return new c;
  case 2: return new c (a[1]);
  case 3: return new c (a[1],a[2]);
  case 4: return new c (a[1],a[2],a[3]);
  case 5: return new c (a[1],a[2],a[3],a[4]);
  case 6: return new c (a[1],a[2],a[3],a[4],a[5]);
  case 7: return new c (a[1],a[2],a[3],a[4],a[5],a[6]);
  case 8: return new c (a[1],a[2],a[3],a[4],a[5],a[6], a[7]);
  }
  function F() { return c.apply(this, args.slice(1)); }
  F.prototype = c.prototype;
  return new F;
}
//Provides: caml_js_wrap_callback const
//Requires: caml_call_gen
function caml_js_wrap_callback(f) {
  var toArray = Array.prototype.slice;
  return function () {
    var args = (arguments.length > 0)?toArray.call (arguments):[0];
    return caml_call_gen(f, args);
  }
}
//Provides: caml_js_wrap_meth_callback const
//Requires: caml_call_gen
function caml_js_wrap_meth_callback(f) {
  var toArray = Array.prototype.slice;
  return function () {
    var args = (arguments.length > 0)?toArray.call (arguments):[0];
    args.unshift (this);
    return caml_call_gen(f, args);
  }
}
//Provides: caml_js_equals mutable
function caml_js_equals (x, y) { return +(x == y); }
//Provides: caml_js_from_byte_string
function caml_js_from_byte_string (s) {return s.getFullBytes();}
//Provides: caml_js_to_byte_string
function caml_js_to_byte_string (s) {return new MlString (s);}
