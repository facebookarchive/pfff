
(* 
 * This file contains the type definitions for a HTML Document, its AST.
 * The AST of HTML is also called the DOM (Document Object Model) by the W3C.
 * src: http://dev.w3.org/html5/spec/spec.html
 *
 * For the types of the full 'HTML + JS(script) + CSS(style)', 
 * see ast_web.ml.
 * 
 * alternatives: 
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
 * 
 * - mirage ?
 * - mmm ? 
 * - hevea ?
 * - efuns has a html mode ? mldonkey ? cameleon ?
 * 
 * - ocamlgtk has a small xml lexer (src/xml_lexer.mll)
 * 
 * See also http://caml.inria.fr/cgi-bin/hump.fr.cgi?sort=0&browse=49
 *)

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

