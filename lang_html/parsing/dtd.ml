(* Yoann Padioleau
 *
 * Copyright (C) 2001-2006 Patrick Doane and Gerd Stolpmann
 * Copyright (C) 2011 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * src: most of the code in this file comes from ocamlnet/netstring/.
 * The original CVS ID is:
 * $Id: nethtml.ml 1296 2009-11-18 13:27:41Z ChriS $
 * I've removed the use of open variants and use simple variants. 
 * I've also removed the helper functions for the relax_dtd.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* What is the class of an element? *)
type element_class = 
  [ `Inline
  | `Block
  | `Essential_block
  | `None
  | `Everywhere
  ]

(* The constraint the subelements must fulfill *)
type model_constraint =
  [ `Inline
  | `Block
  | `Flow         (* = `Inline or `Block *)
  | `Empty
  | `Any
  | `Special
  | `Elements of string list  (* Enumeration of allowed elements *)
  | `Or of (model_constraint * model_constraint)
  | `Except of (model_constraint * model_constraint)
  | `Sub_exclusions of (string list * model_constraint)
  ]

type simplified_dtd =
    (string * (element_class * model_constraint)) list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ( |. ) a b = `Or(a,b)
(* let ( -. ) a b = `Except(a,b) *)

(*****************************************************************************)
(* The DTD *)
(*****************************************************************************)

let block_elements =
  (* Only used for exclusions *)
  [ "p"; "dl"; "div"; "center"; "noscript"; "noframes"; "blockquote"; "form";
    "isindex"; "hr"; "table"; "fieldset"; "address"; "h1"; "h2"; "h3"; "h4";
    "h5"; "h6"; "pre"; "ul"; "ol"; "dir"; "menu" ]

let html40_dtd =
  [ (* --------- INLINE ELEMENTS ------------ *)
    (* %fontstyle; *)
    "tt",                 (`Inline, `Inline);
    "i",                  (`Inline, `Inline);
    "b",                  (`Inline, `Inline);
    "big",                (`Inline, `Inline);
    "small",              (`Inline, `Inline);
    (* transitional: *)
    "u",                  (`Inline, `Inline);
    "s",                  (`Inline, `Inline);
    "strike",             (`Inline, `Inline);
    (* %phrase; *)
    "em",                 (`Inline, `Inline);
    "strong",             (`Inline, `Inline);
    "dfn",                (`Inline, `Inline);
    "code",               (`Inline, `Inline);
    "samp",               (`Inline, `Inline);
    "kbd",                (`Inline, `Inline);
    "var",                (`Inline, `Inline);
    "cite",               (`Inline, `Inline);
    "abbr",               (`Inline, `Inline);
    "acronym",            (`Inline, `Inline);
    (* %special; *)
    "sup",                (`Inline, `Inline);
    "sub",                (`Inline, `Inline);
    "span",               (`Inline, `Inline);
    "bdo",                (`Inline, `Inline);
    "br",                 (`Inline, `Empty);
    "a",                  (`Inline, `Sub_exclusions(["a"],`Inline));
    "img",                (`Inline, `Empty);
    "object",             (`Inline, (`Flow |. `Elements ["param"]));
    "script",             (`Inline, `Special);
    "map",                (`Inline, (`Flow |. `Elements ["area"]));
    "q",                  (`Inline, `Inline);
    (* transitional: *)
    "applet",             (`Inline, (`Flow |. `Elements ["param"]));
    "font",               (`Inline, `Inline);
    "basefont",           (`Inline, `Empty);
    "iframe",             (`Inline, `Flow);
    (* %formctrl; *)
    "input",              (`Inline, `Empty);
    "select",             (`Inline, `Elements ["optgroup"; "option"]);
    "textarea",           (`Inline, `Elements []);    (* #PCDATA *)
    "label",              (`Inline, `Sub_exclusions( ["label"],
						    `Inline));
    "button",             (`Inline, `Sub_exclusions( ["a"; "input"; "select";
						     "textarea"; "label";
						     "button"; "form";
						     "fieldset"; "isindex";
						     "iframe"],
						    `Flow));
    (* ------------ BLOCK ELEMENTS ----------*)
    "p",                  (`Block, `Inline);
    (* %heading; *)
    "h1",                 (`Block, `Inline); 
    "h2",                 (`Block, `Inline);
    "h3",                 (`Block, `Inline);
    "h4",                 (`Block, `Inline);
    "h5",                 (`Block, `Inline);
    "h6",                 (`Block, `Inline);
    (* %list; *)
    "ul",                 (`Block, `Elements ["li"]);
    "ol",                 (`Block, `Elements ["li"]);
    (* transitional: *)
    "dir",                (`Block, `Sub_exclusions( block_elements,
						   `Elements ["li"]));
    "menu",               (`Block, `Sub_exclusions( block_elements,
						   `Elements ["li"]));
    (* %preformatted; *)
    "pre",                (`Block, `Sub_exclusions( [ "img"; "object"; "applet";
						      "big"; "small"; "sub"; 
						      "sup"; "font"; "basefont"],
						    `Inline ));
    (* other: *)
    "dl",                 (`Block, `Elements ["dt"; "dd"]);
    "div",                (`Block, `Flow);
    "noscript",           (`Block, `Flow);
    "blockquote",         (`Block, (`Flow |. `Elements ["script"]));
                          (* strict DTD has `Block here *)
    "form",               (`Block, `Sub_exclusions( ["form"],
						    `Flow |. 
						       `Elements ["script"]));
                          (* strict DTD has `Block here *)
    "hr",                 (`Block, `Empty);
    "table",              (`Block, `Elements ["caption"; "col"; "colgroup";
					      "thead"; "tfoot"; "tbody"; "tr"]);
    "fieldset",           (`Block, (`Flow |. `Elements ["legend"]));
    "address",            (`Block, `Inline);
    (* transitional: *)
    "center",             (`Block, `Flow);
    "noframes",           (`Block, `Flow);
    "isindex",            (`Block, `Empty);
    (* ------------ OTHER ELEMENTS ----------*)
    "body",               (`None, (`Flow |. `Elements ["script"]));
                          (* strict DTD has `Block here *)
    "area",               (`None, `Empty);
    "link",               (`None, `Empty);
    "param",              (`None, `Empty);
    "ins",                (`Everywhere, `Flow);
    "del",                (`Everywhere, `Flow);
    "dt",                 (`None, `Inline);
    "dd",                 (`None, `Flow);
    "li",                 (`None, `Flow);
    "optgroup",           (`None, `Elements ["option"]);
    "option",             (`None, `Elements []);   (* #PCDATA *)
    "legend",             (`None, `Inline);
    "caption",            (`None, `Inline);
    "thead",              (`None, `Elements ["tr"]);
    "tbody",              (`None, `Elements ["tr"]);
    "tfoot",              (`None, `Elements ["tr"]);
    "colgroup",           (`None, `Elements ["col"]);
    "col",                (`None, `Empty);
    "tr",                 (`None, `Elements ["th"; "td"]);
    "th",                 (`None, `Flow);
    "td",                 (`None, `Flow);
    "head",               (`None, `Elements ["title"; "base"; "script";
					     "style"; "meta"; "link";
					     "object"]);
    "title",              (`None, `Elements []);   (* #PCDATA *)
    "base",               (`None, `Empty);
    "meta",               (`None, `Empty);
    "style",              (`None, `Special);
    "html",               (`None, (`Flow |. 
				       `Elements ["head"; 
						  "title"; "base"; "script";
						  "style"; "meta"; "link";
						  "object";
						  "body"; "frameset"]));
    (* transitional: *)
    "frameset",           (`None, `Elements ["frameset"; "frame"; "noframes"]);
    "frame",              (`None, `Empty);
  ]
