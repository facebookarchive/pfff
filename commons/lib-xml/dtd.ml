(*
 * Xml Light, an small Xml parser/printer with DTD support.
 * Copyright (C) 2003 Nicolas Cannasse (ncannasse@motion-twin.com)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
  
open Xml_types
open Dtd_types
open Printf

type dtd = Dtd_types.dtd

type dtd_result =
	| DTDNext
	| DTDNotMatched
	| DTDMatched
	| DTDMatchedResult of dtd_child

type error_pos = {
	eline : int;
	eline_start : int;
	emin : int;
	emax : int;
}

type parse_error = parse_error_msg * Xml_types.error_pos


type ('a,'b) hash = ('a,'b) Hashtbl.t


type dtd_state = {
	elements : (string,dtd_element_type) hash;
	attribs : (string,(string,(dtd_attr_type * dtd_attr_default)) hash) hash;
	mutable current : dtd_element_type;
	mutable curtag : string;
	state : (string * dtd_element_type) Stack.t;
}

let file_not_found = ref (fun _ -> assert false)

let _raises e =
	file_not_found := e


let pos source =
	let line, lstart, min, max = Xml_lexer.pos source in
	(Obj.magic {
		eline = line;
		eline_start = lstart;
		emin = min;
		emax = max;
	} : Xml_types.error_pos)

let convert = function
	| Xml_lexer.EInvalidDTDDecl -> InvalidDTDDecl
	| Xml_lexer.EInvalidDTDElement -> InvalidDTDElement
	| Xml_lexer.EInvalidDTDTag -> InvalidDTDTag
	| Xml_lexer.EDTDItemExpected -> DTDItemExpected
	| Xml_lexer.EInvalidDTDAttribute -> InvalidDTDAttribute

let parse source =
	try
		Xml_lexer.init source;
		(* local cast Dtd.dtd -> dtd *)
		let dtd = (Obj.magic Xml_lexer.dtd source : dtd) in
		Xml_lexer.close source;
		dtd
	with
		| Xml_lexer.DTDError e ->
			Xml_lexer.close source;
			raise (Parse_error (convert e,pos source))

let parse_string s = parse (Lexing.from_string s)
let parse_in ch = parse (Lexing.from_channel ch)

let parse_file fname =
	let ch = (try open_in fname with Sys_error _ -> raise (!file_not_found fname)) in
	try
		let x = parse (Lexing.from_channel ch) in
		close_in ch;
		x
	with
		e ->
			close_in ch;
			raise e




(* - for debug only - *)

let to_string_ref = ref (fun _ -> assert false)









let parse_error_msg = function
	| InvalidDTDDecl -> "Invalid DOCTYPE declaration"
	| InvalidDTDElement -> "Invalid DTD element declaration"
	| InvalidDTDAttribute -> "Invalid DTD attribute declaration"
	| InvalidDTDTag -> "Invalid DTD tag"
	| DTDItemExpected -> "DTD item expected"

let parse_error (msg,pos) =
	let pos = (Obj.magic pos : error_pos) in
	if pos.emin = pos.emax then
		sprintf "%s line %d character %d" (parse_error_msg msg) pos.eline (pos.emin - pos.eline_start)
	else
		sprintf "%s line %d characters %d-%d" (parse_error_msg msg) pos.eline (pos.emin - pos.eline_start) (pos.emax - pos.eline_start)

let check_error = function
	| ElementDefinedTwice tag -> sprintf "Element '%s' defined twice" tag
	| AttributeDefinedTwice (tag,aname) -> sprintf "Attribute '%s' of element '%s' defined twice" aname tag
	| ElementEmptyContructor tag -> sprintf "Element '%s' has empty constructor" tag
	| ElementReferenced (tag,from) -> sprintf "Element '%s' referenced by '%s' is not declared" tag from
	| ElementNotDeclared tag -> sprintf "Element '%s' needed but is not declared" tag

let prove_error = function
	| UnexpectedPCData -> "Unexpected PCData"
	| UnexpectedTag tag -> sprintf "Unexpected tag : '%s'" tag
	| UnexpectedAttribute att -> sprintf "Unexpected attribute : '%s'" att
	| InvalidAttributeValue att -> sprintf "Invalid attribute value for '%s'" att
	| RequiredAttribute att -> sprintf "Required attribute not found : '%s'" att
	| ChildExpected cname -> sprintf "Child expected : '%s'" cname
	| EmptyExpected -> "No more children expected"

let to_string = function
	| DTDAttribute (tag,aname,atype,adef) ->
		let atype_to_string = function
			| DTDCData -> "CDATA"
			| DTDNMToken -> "NMTOKEN"
			| DTDEnum l -> sprintf "(%s)" (String.concat "|" l)
		in
		let adefault_to_string = function
			| DTDDefault s -> sprintf "\"%s\"" s
			| DTDRequired -> "#REQUIRED"
			| DTDImplied -> "#IMPLIED"
			| DTDFixed s -> sprintf "#FIXED \"%s\"" s
		in
		sprintf "<!ATTLIST %s %s %s %s>" tag aname (atype_to_string atype) (adefault_to_string adef)
	| DTDElement (tag,etype) ->
		let rec echild_to_string = function
			| DTDTag s -> s
			| DTDPCData -> "#PCDATA"
			| DTDOptional c -> sprintf "%s?" (echild_to_string c)
			| DTDZeroOrMore c -> sprintf "%s*" (echild_to_string c)
			| DTDOneOrMore c -> sprintf "%s+" (echild_to_string c)
			| DTDChoice [c] -> echild_to_string c
			| DTDChoice l -> sprintf "(%s)" (String.concat "|" (List.map echild_to_string l))
			| DTDChildren [c] -> echild_to_string c
			| DTDChildren l -> sprintf "(%s)" (String.concat "," (List.map echild_to_string l))
		in
		let etype_to_string = function
			| DTDEmpty -> "EMPTY"
			| DTDAny -> "ANY"
			| DTDChild x ->
				let rec op_to_string = function
					| DTDOptional c -> sprintf "%s?" (op_to_string c)
					| DTDZeroOrMore c -> sprintf "%s*" (op_to_string c)
					| DTDOneOrMore c -> sprintf "%s+" (op_to_string c)
					| _ -> ""
				in
				let rec root = function
					| DTDOptional c
					| DTDZeroOrMore c 
					| DTDOneOrMore c ->
						root c
					| DTDChoice [_]
					| DTDChildren [_] as x ->
						x, false
					| DTDChoice _
					| DTDChildren _ as x ->
						x, true
					| x -> x, false
				in
				match root x with
				| r, true -> sprintf "%s%s" (echild_to_string r) (op_to_string x)
				| r, false -> sprintf "(%s%s)" (echild_to_string r) (op_to_string x)
		in
		sprintf "<!ELEMENT %s %s>" tag (etype_to_string etype)

;;
to_string_ref := to_string
