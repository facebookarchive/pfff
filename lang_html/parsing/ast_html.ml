(* Yoann Padioleau
 * 
 * Copyright (C) 2009,2010,2011 Facebook
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

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * This file contains the type definitions for a HTML Document, its AST,
 * also called the DOM (Document Object Model) by the W3C.
 * src: http://dev.w3.org/html5/spec/spec.html
 *
 * For the types of the full 'HTML + JS(script) + CSS(style)' see ast_web.ml.
 * For information on characters encoding, ascii, utf-8, etc, see encodings.ml
 * 
 * There are multiple ways to represent an HTML AST:
 *  - just as a tree of 'Element of ... | CdData of ...' (as done in ocamlnet).
 *    This is simple but lack type-checking. Some checking can be done though
 *    by using the spec of a DTD and run a validator.
 *  - a tree with phantom types (as done in xHTML)
 *  - a real AST, with one different constructor per html element, and
 *    precise types for the set of acceptable attributes. Because
 *    the DTD of HTML is complex, such an AST can be quite tedious to write.
 *  - an ocamlduce/cduce AST, which have a type system specially made to 
 *    express the kind of invariants of a DTD.
 * 
 * update: I've added token/info in the html tree so we can at least have 
 *  an AST-based highlighter which is needed for coloring urls as in href
 *  for instance.
 * 
 * TODO: The solution used in this module is to use a real AST ?
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
 *  - htcaml: camlp4 on top of xmlm, https://github.com/samoht/htcaml
 *    simple AST
 * - mirage ?
 * - mmm ? 
 * - hevea has a htmllexer.mll, looks quite similar to the one in ocamlnet
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
(* The AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

type pinfo = Parse_info.token
type info = Parse_info.info
and tok = info
(* a shortcut to annotate some information with token/position information *)
and 'a wrap = 'a * info
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* HTML raw version *)
(* ------------------------------------------------------------------------- *)

type html_raw = HtmlRaw of string 

(* ------------------------------------------------------------------------- *)
(* HTML tree version *)
(* ------------------------------------------------------------------------- *)

(* src: ocamlnet/netstring/nethtml.mli *)
(** The type [document] represents parsed HTML documents:
 *
 * {ul
 * {- [Element (name, args, subnodes)] is an element node for an element of
 *   type [name] (i.e. written [<name ...>...</name>]) with arguments [args]
 *   and subnodes [subnodes] (the material within the element). The arguments
 *   are simply name/value pairs. Entity references (something like [&xy;])
 *   occuring in the values are {b not} resolved.
 *
 *   Arguments without values (e.g. [<select name="x" multiple>]: here,
 *   [multiple] is such an argument) are represented as [(name,name)], i.e. the
 *   name is also returned as value.
 *
 *   As argument names are case-insensitive, the names are all lowercase.}
 * {- [Data s] is a character data node. Again, entity references are contained
 *   as such and not as what they mean.}
 * }
 *
 * Character encodings: The parser is restricted to ASCII-compatible
 * encodings (see the function {!Netconversion.is_ascii_compatible} for
 * a definition). In order to read other encodings, the text must be
 * first recoded to an ASCII-compatible encoding (example below).
 * Names of elements and attributes must additionally be ASCII-only.
 *)

(* src: ocamlnet/netstring/nethtml.mli, extended with pad's wrap and newtype *)
type html_tree = 
  | Element of 
      tag *
      (attr_name * attr_value) list *
      html_tree list
  | Data of string wrap

 and tag = Tag of string wrap
 and attr_name  = Attr of string wrap
 and attr_value = Val  of string wrap

 (* with tarzan *)

(* 
 * TODO
 * type url = Url of string (* actually complicated sublanguage *)
 * type color = Color of string (* ?? *)
 * 
 * ??? tree ? how be precise ? 
 * see xHtml.ml ? but too complicated to build ... shadow type sucks
 *
 * (* aka script *)
 * type javascript = unit
 * 
 * (*aka style *)
 * type css = unit
 *)

(* ------------------------------------------------------------------------- *)
(* HTML full AST version *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* a small wrapper over ocamlnet *)
type html_tree2 = Nethtml.document list

(*****************************************************************************)
(* Some constructors *)
(*****************************************************************************)

let fakeInfo ?(next_to=None) ?(str="") () = { 
  PI.token = PI.FakeTokStr (str, next_to);
  comments = ();
  transfo = PI.NoTransfo;
  }

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
