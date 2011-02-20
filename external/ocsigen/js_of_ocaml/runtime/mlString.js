// Js_of_ocaml runtime support
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

// Invariants
// ==========
// At any time, at least one property of "string", "bytes" or "array"
// is set; if several are set, then their values must correspond.
// If "bytes" is set, then properties "len" and "last" are also set.
// If "array" is set, properties "len" and "last" are also set.
// Properties "len" and "last" may have different values only when
// "string" and "array" are both null.
//
// We use unusual accessors (getLen/get/set) so that this
// implementation of string differs significantly from Javascript.
// This way, using the wrong object is detected early.

//Provides: MlString
//Requires: caml_array_bound_error
function caml_str_repeat(n, s) {
  if (!n) { return ""; }
  if (n & 1) { return caml_str_repeat(n - 1, s) + s; }
  var r = caml_str_repeat(n >> 1, s);
  return r + r;
}

function MlString(param) {
  if (param != null) {
    this.bytes = this.fullBytes = param;
    this.last = this.len = param.length;
  }
}

MlString.prototype = {
  // JS string
  string:null,
  // byte string
  bytes:null,
  fullBytes:null,
  // byte array
  array:null,
  // length
  len:null,
  // last initialized byte
  last:0,

  toJsString:function() {
    // assumes this.string == null
    return this.string = decodeURIComponent (escape(this.getFullBytes()));
  },

  toBytes:function() {
    // assumes this.bytes == null
    if (this.string != null)
      var b = unescape (encodeURIComponent (this.string));
    else {
      var b = "", a = this.array, l = a.length;
      // FIX should benchmark different conversion functions
      for (var i = 0; i < l; i ++) b += String.fromCharCode (a[i]);
    }
    this.bytes = this.fullBytes = b;
    this.last = this.len = b.length;
    return b;
  },

  getBytes:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes();
    return b;
  },

  getFullBytes:function() {
    var b = this.fullBytes;
    if (b !== null) return b;
    b = this.bytes;
    if (b == null) b = this.toBytes ();
    if (this.last < this.len) {
      this.bytes = (b += caml_str_repeat(this.len - this.last, '\0'));
      this.last = this.len;
    }
    this.fullBytes = b;
    return b;
  },

  toArray:function() {
    // assumes this.array == null
    var b = this.bytes;
    if (b == null) b = this.toBytes ();
    var a = [], l = this.last;
    for (var i = 0; i < l; i++) a[i] = b.charCodeAt(i);
    for (l = this.len; i < l; i++) a[i] = 0;
    this.string = this.bytes = this.fullBytes = null;
    this.last = this.len;
    this.array = a;
    return a;
  },

  getArray:function() {
    var a = this.array;
    if (!a) a = this.toArray();
    return a;
  },

  getLen:function() {
    var len = this.len;
    if (len !== null) return len;
    this.toBytes();
    return this.len;
  },

  toString:function() { var s = this.string; return s?s:this.toJsString(); },

  valueOf:function() { var s = this.string; return s?s:this.toJsString(); },

  blitToArray:function(i1, a2, i2, l) {
    var a1 = this.array;
    if (a1)
      for (var i = 0; i < l; i++) a2 [i2 + i] = a1 [i1 + i];
    else {
      var b = this.bytes;
      if (b == null) b = this.toBytes();
      var l1 = this.last - i1;
      if (l <= l1)
        for (var i = 0; i < l; i++) a2 [i2 + i] = b.charCodeAt(i1 + i);
      else {
        for (var i = 0; i < l1; i++) a2 [i2 + i] = b.charCodeAt(i1 + i);
        for (; i < l; i++) a2 [i2 + i] = 0;
      }
    }
  },

  get:function (i) {
    var a = this.array;
    if (a) return a[i];
    var b = this.bytes;
    if (b == null) b = this.toBytes();
    return (i<this.last)?b.charCodeAt(i):0;
  },

  safeGet:function (i) {
    if (!this.len) this.toBytes();
    if ((i < 0) || (i >= this.len)) caml_array_bound_error ();
    return this.get(i);
  },

  set:function (i, c) {
    var a = this.array;
    if (!a) {
      if (this.last == i) {
        this.bytes += String.fromCharCode (c & 0xff);
        this.last ++;
        return 0;
      }
      a = this.toArray();
    } else if (this.bytes != null) {
      this.bytes = this.fullBytes = this.string = null;
    }
    a[i] = c & 0xff;
    return 0;
  },

  safeSet:function (i, c) {
    if (this.len == null) this.toBytes ();
    if ((i < 0) || (i >= this.len)) caml_array_bound_error ();
    this.set(i, c);
  },

  fill:function (ofs, len, c) {
    if (ofs >= this.last && this.last && c == 0) return;
    var a = this.array;
    if (!a) a = this.toArray();
    else if (this.bytes != null) {
      this.bytes = this.fullBytes = this.string = null;
    }
    var l = ofs + len;
    for (i = ofs; i < l; i++) a[i] = c;
  },

  compare:function (s2) {
    if (this.string != null && s2.string != null) {
      if (this.string < s2.string) return -1;
      if (this.string > s2.string) return 1;
      return 0;
    }
    var b1 = this.getFullBytes ();
    var b2 = s2.getFullBytes ();
    if (b1 < b2) return -1;
    if (b1 > b2) return 1;
    return 0;
  },

  equal:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string == s2.string;
    return this.getFullBytes () == s2.getFullBytes ();
  },
  lessThan:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string < s2.string;
    return this.getFullBytes () < s2.getFullBytes ();
  },
  lessEqual:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string <= s2.string;
    return this.getFullBytes () <= s2.getFullBytes ();
  }
}

// Conversion Javascript -> Caml
function MlWrappedString (s) { this.string = s; }
MlWrappedString.prototype = new MlString();

// Uninitialized Caml string
function MlMakeString (l) { this.bytes = ""; this.len = l; }
MlMakeString.prototype = new MlString ();

// Caml string initialized form an array of bytes
//Provides: MlStringFromArray
//Requires: MlString
function MlStringFromArray (a) {
  var len = a.length; this.array = a; this.len = this.last = len;
}
MlStringFromArray.prototype = new MlString ();

//Provides: caml_create_string const
//Requires: MlString
function caml_create_string(len) { return new MlMakeString(len); }
//Provides: caml_fill_string
//Requires: MlString
function caml_fill_string(s, i, l, c) { s.fill (i, l, c); }
//Provides: caml_string_compare mutable
//Requires: MlString
function caml_string_compare(s1, s2) { return s1.compare(s2); }
//Provides: caml_string_equal mutable
//Requires: MlString
function caml_string_equal(s1, s2) {
  var b1 = s1.fullBytes;
  var b2 = s2.fullBytes;
  if (b1 != null && b2 != null) return (b1 == b2)?1:0;
  return (s1.getFullBytes () == s2.getFullBytes ())?1:0;
}
//Provides: caml_string_notequal mutable
//Requires: caml_string_equal
function caml_string_notequal(s1, s2) { return 1-caml_string_equal(s1, s2); }
//Provides: caml_string_lessequal
//Requires: MlString
function caml_string_lessequal(s1, s2) { return s1.lessEqual(s2); }
//Provides: caml_string_lessthan
//Requires: MlString
function caml_string_lessthan(s1, s2) { return s1.lessThan(s2); }
//Provides: caml_string_greaterthan
//Requires: MlString
function caml_string_greaterthan(s1, s2) { return s2.lessThan(s1); }
//Provides: caml_string_greaterequal
//Requires: MlString
function caml_string_greaterequal(s1, s2) { return s2.lessThan(s1); }
//Provides: caml_blit_string
//Requires: MlString
function caml_blit_string(s1, i1, s2, i2, len) {
  if (len === 0) return;
  if (i2 === s2.last && i1 === 0 && s1.last == len) {
    // s2.string and s2.array are null
    var s = s1.bytes;
    if (s !== null)
      s2.bytes += s1.bytes;
    else
      s2.bytes += s1.getBytes();
    s2.last += len;
    return;
  }
  var a = s2.array;
  if (!a) a = s2.toArray(); else { s2.bytes = s2.string = null; }
  s1.blitToArray (i1, a, i2, len);
}
