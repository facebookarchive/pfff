(* Yoann Padioleau
 * 
 * Copyright (C) 2009-2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

open Common

open Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * This file contains the type definitions for a HTML Document, its AST.
 * The AST of HTML is also called the DOM (Document Object Model) by the W3C.
 * src: http://dev.w3.org/html5/spec/spec.html
 *
 * For the types of the full 'HTML + JS(script) + CSS(style)', 
 * see ast_web.ml.
 * 
 * There are multiple ways to represent an HTML AST:
 *  - just as a tree of 'Element of ... | CdData of ...' (as done in ocamlnet).
 *    This is simple but lack type-checking. Some checking can be done though
 *    by using the spec of a DTD.
 *  - a tree with phantom types
 *  - a real AST, with one different constructor per html element, and
 *    precise types for the set of acceptable attributes. Because
 *    the DTD of HTML is complex, such an AST can be quite tedious to write.
 *  - use ocamlduce/cduce which have a type system specially made to express
 *    the kind of invariants in a DTD.
 * 
 * TODO The solution used in this module is to use a real AST.
 * 
 * alternative implementations: 
 * - xHTML.ml: but poor AST, no parsing, and phantom types are tricky
 * - xml-light: just for strict XML and poor AST
 * - pxp: just for strict XML ?
 * - ocamlnet netstring: looks like a very simple html parser, very (too?)
 *    simple AST (checking is done via an external DTD), also support
 *    ascii only (but can go through an ascii encoding of the input stream)
 * - ocigen: use xHTML.ml or ocamlduce to encode html elements, no parsing
 * - cduce/ocamlduce: they say they rely on pxp/expat/xml-light for input.
 *    They have potentially a better AST to work on, a well typed one.
 *    The one in xHTML.ml use phantom types so it's good for ensuring
 *    the well-typed construction of html, but does not help when
 *    we work on the AST, to do pattern matching on it, to have the
 *    exhaustive check of OCaml, etc.
 * - mirage ?
 * - mmm ? 
 * - hevea ?
 * - efuns has a html mode ? mldonkey ? cameleon ?
 * - ocamlgtk has a small xml lexer (src/xml_lexer.mll)
 * See also http://caml.inria.fr/cgi-bin/hump.fr.cgi?sort=0&browse=49
 * 
 * alternative in other languages:
 *  - the one in firefox, webkit
 *  - a python and php one, http://code.google.com/p/html5lib
 *  - a javascript one, http://ejohn.org/blog/pure-javascript-html-parser/
 * 
 * See also http://en.wikipedia.org/wiki/HTML5
 *)

(*****************************************************************************)
(* Characters, encoding *)
(*****************************************************************************)

(* 
 * On character sets and encodings (src ocamlnet/netstring/netconversion.mli):
 * 
 *   A {b character set} is a set of characters where every character is
 *   identified by a {b code point}. An {b encoding} is a way of 
 *   representing characters from a set in byte strings. For example,
 *   the Unicode character set has more than 96000 characters, and
 *   the code points have values from 0 to 0x10ffff (not all code points
 *   are assigned yet). The UTF-8 encoding represents the code points
 *   by sequences of 1 to 4 bytes. There are also encodings that 
 *   represent code points from several sets, e.g EUC-JP covers four
 *   sets.
 *  
 *   pad: so unicode = char set, utf-8 = encoding (char set -> bytes).
 *   A character is a glyph, and a code point is its index in the set.
 * 
 * On strings and encodings (src ocamlnet/netstring/netconversion.mli):
 * 
 *   In a {b single-byte encoding} every code point is represented by
 *   one byte. This is what many programmers are accustomed at, and
 *   what the O'Caml language specially supports: A [string] is
 *   a sequence of [char]s, where [char] means an 8 bit quantity
 *   interpreted as character.
 * 
 *   In a {b multi-byte encoding} there are code points that are represented
 *   by several bytes. As we still represent such text as [string], the
 *   problem arises that a single [char], actually a byte, often represents 
 *   only a fraction of a full multi-byte character. There are two solutions:
 *   - Give up the principle that text is represented by [string].
 *     This is, for example, the approach chosen by [Camomile], another O'Caml
 *     library dealing with Unicode. Instead, text is represented as
 *     [int array]. This way, the algorithms processing the text can
 *     remain the same.
 *   - Give up the principle that individual characters can be directly
 *     accessed in a text. This is the primary way chosen by Ocamlnet.
 *     This means that there is not any longer the possibility to read
 *     or write the [n]th character of a text. One can, however, still 
 *     compose texts by just concatenating the strings representing
 *     individual characters. Furthermore, it is possible to define
 *     a cursor for a text that moves sequentially along the text.
 *     The consequence is that programmers are restricted to sequential
 *     algorithms. Note that the majority of text processing falls into
 *     this class.
 * 
 *  On ASCII compatible  (src ocamlnet/netstring/netconversion.mli):
 * 
 *    "ASCII compatible" means: The bytes 1 to 127 represent the ASCII
 *    codes 1 to 127, and no other representation of a character contains
 *    the bytes 1 to 127.
 *    
 *    For example, ISO-8859-1 is ASCII-compatible because the byte 1 to
 *    127 mean the same as in ASCII, and all other characters use bytes
 *    greater than 127. UTF-8 is ASCII-compatible for the same reasons,
 *    *it does not matter that there are multi-byte characters*.
 *    EBCDIC is not ASCII-compatible because the bytes 1 to 127 do not mean
 *    the same as in ASCII. UTF-16 is not ASCII-compatible because the bytes
 *    1 to 127 can occur in multi-byte representations of non-ASCII
 *    characters.
 * 
 *)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(*
  type url = Url of string (* actually complicated sublanguage *)
  
  type tag = Tag of string
  
  type attribute = Attr of string (* * value ? *)
  
  type color = Color of string (* ?? *)
  
  
  (* ??? tree ? how be precise ? 
   * see xHtml.ml ? but too complicated to build ... shadow type sucks
   *)
  type html = unit 
  
  (* aka script *)
  type javascript = unit
  
  (* aka style *)
  type css = unit

*)

(* a small wrapper over ocamlnet *)

type html_tree = Nethtml.document list

type html_raw = string (* might want to use channel for efficiency *)

