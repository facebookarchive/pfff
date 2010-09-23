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



type dtd_child =
	| DTDTag of string
	| DTDPCData
	| DTDOptional of dtd_child
	| DTDZeroOrMore of dtd_child
	| DTDOneOrMore of dtd_child
	| DTDChoice of dtd_child list
	| DTDChildren of dtd_child list

type dtd_element_type =
	| DTDEmpty
	| DTDAny
	| DTDChild of dtd_child

type dtd_attr_default =
	| DTDDefault of string
	| DTDRequired
	| DTDImplied
	| DTDFixed of string

type dtd_attr_type =
	| DTDCData
	| DTDNMToken
	| DTDEnum of string list

type dtd_item =
	| DTDAttribute of string * string * dtd_attr_type * dtd_attr_default
	| DTDElement of string * dtd_element_type

type dtd = dtd_item list

type ('a,'b) hash = ('a,'b) Hashtbl.t
type checked = {
	c_elements : (string,dtd_element_type) hash;
	c_attribs : (string,(string,(dtd_attr_type * dtd_attr_default)) hash) hash;
}



type parse_error_msg =
	| InvalidDTDDecl
	| InvalidDTDElement
	| InvalidDTDAttribute
	| InvalidDTDTag
	| DTDItemExpected

type check_error =
	| ElementDefinedTwice of string
	| AttributeDefinedTwice of string * string
	| ElementEmptyContructor of string
	| ElementReferenced of string * string
	| ElementNotDeclared of string

type prove_error =
	| UnexpectedPCData
	| UnexpectedTag of string
	| UnexpectedAttribute of string
	| InvalidAttributeValue of string
	| RequiredAttribute of string
	| ChildExpected of string
	| EmptyExpected

type parse_error = parse_error_msg * Xml_types.error_pos

exception Parse_error of parse_error
exception Check_error of check_error
exception Prove_error of prove_error


open Xml_types
open Printf

type dtd_result =
	| DTDNext
	| DTDNotMatched
	| DTDMatched
	| DTDMatchedResult of dtd_child


type dtd_state = {
	elements : (string,dtd_element_type) hash;
	attribs : (string,(string,(dtd_attr_type * dtd_attr_default)) hash) hash;
	mutable current : dtd_element_type;
	mutable curtag : string;
	state : (string * dtd_element_type) Stack.t;
}

let empty_hash = Hashtbl.create 0


let start_prove dtd root =
	let d = {
		elements = dtd.c_elements;
		attribs = dtd.c_attribs;
		state = Stack.create();
		current = DTDChild (DTDTag root);
		curtag = "_root";
	} in
	try
		ignore(Hashtbl.find d.elements (String.uppercase root));
		d
	with
		Not_found -> raise (Check_error (ElementNotDeclared root))


let check dtd =
	let attribs = Hashtbl.create 0 in
	let hdone = Hashtbl.create 0 in
	let htodo = Hashtbl.create 0 in
	let ftodo tag from =
		try
			ignore(Hashtbl.find hdone tag);
		with
			Not_found ->
				try
					match Hashtbl.find htodo tag with
					| None -> Hashtbl.replace htodo tag from
					| Some _ -> ()
				with
					Not_found ->
						Hashtbl.add htodo tag from
	in
	let fdone tag edata =
		try 
			ignore(Hashtbl.find hdone tag);
			raise (Check_error (ElementDefinedTwice tag));
		with
			Not_found ->
				Hashtbl.remove htodo tag;
				Hashtbl.add hdone tag edata
	in
	let fattrib tag aname adata =
		let h = (try
				Hashtbl.find attribs tag
			with
				Not_found ->
					let h = Hashtbl.create 1 in
					Hashtbl.add attribs tag h;
					h) in
		try
			ignore(Hashtbl.find h aname);
			raise (Check_error (AttributeDefinedTwice (tag,aname)));
		with
			Not_found ->
				Hashtbl.add h aname adata
	in
	let check_item = function
		| DTDAttribute (tag,aname,atype,adef) ->
			let utag = String.uppercase tag in
			ftodo utag None;
			fattrib utag (String.uppercase aname) (atype,adef)
		| DTDElement (tag,etype) ->
			let utag = String.uppercase tag in
			fdone utag etype;
			let check_type = function
				| DTDEmpty -> ()
				| DTDAny -> ()
				| DTDChild x ->
					let rec check_child = function
						| DTDTag s -> ftodo (String.uppercase s) (Some utag)
						| DTDPCData -> ()
						| DTDOptional c
						| DTDZeroOrMore c
						| DTDOneOrMore c ->
							check_child c
						| DTDChoice []
						| DTDChildren [] ->
							raise (Check_error (ElementEmptyContructor tag))
						| DTDChoice l
						| DTDChildren l ->
							List.iter check_child l
					in
					check_child x
			in
			check_type etype
	in
	List.iter check_item dtd;
	Hashtbl.iter (fun t from ->
		match from with
		| None -> raise (Check_error (ElementNotDeclared t))
		| Some tag -> raise (Check_error (ElementReferenced (t,tag)))
	) htodo;
	{
		c_elements = hdone;
		c_attribs = attribs;
	}


let to_string_ref = ref (fun _ -> assert false)

let trace dtd tag =
	let item = DTDElement ("current",dtd.current) in
	printf "%s : %s\n"
		(match tag with None -> "#PCDATA" | Some t -> t)
		(!to_string_ref item)

exception TmpResult of dtd_result


let is_nmtoken_char = function
	| 'A'..'Z' | 'a'..'z' | '0'..'9' | '.' | '-' | '_' | ':' -> true
	| _ -> false


let prove_child dtd tag = 
	trace dtd tag;
	match dtd.current with
	| DTDEmpty -> raise (Prove_error EmptyExpected)
	| DTDAny -> ()
	| DTDChild elt ->
		let rec update = function
		| DTDTag s ->
			(match tag with
			| None -> DTDNotMatched
			| Some t when t = String.uppercase s -> DTDMatched
			| Some _ -> DTDNotMatched)
		| DTDPCData ->
			(match tag with
			| None -> DTDMatched
			| Some _ -> DTDNotMatched)
		| DTDOptional x ->
			(match update x with
			| DTDNotMatched
			| DTDNext -> DTDNext
			| DTDMatched
			| DTDMatchedResult _ -> DTDMatched)
		| DTDZeroOrMore x ->
			(match update x with
			| DTDNotMatched
			| DTDNext -> DTDNext
			| DTDMatched
			| DTDMatchedResult _ -> DTDMatchedResult (DTDZeroOrMore x))
		| DTDOneOrMore x ->
			(match update x with
			| DTDNotMatched
			| DTDNext -> DTDNotMatched
			| DTDMatched
			| DTDMatchedResult _ -> DTDMatchedResult (DTDZeroOrMore x))
		| DTDChoice l ->
			(try
				(match List.exists (fun x ->
					match update x with
					| DTDMatched -> true
					| DTDMatchedResult _ as r -> raise (TmpResult r)
					| DTDNext | DTDNotMatched -> false) l with
				| true -> DTDMatched
				| false -> DTDNotMatched)
			with
				TmpResult r -> r)	
		| DTDChildren [] -> assert false (* DTD is checked ! *)
		| DTDChildren (h :: t) ->
			(match update h with
			| DTDNext ->
				(match t with
				| [] -> DTDNotMatched
				| _ -> update (DTDChildren t))
			| DTDNotMatched -> DTDNotMatched
			| DTDMatchedResult r ->
				DTDMatchedResult (DTDChildren (r::t))
			| DTDMatched ->
				match t with
				| [] -> DTDMatched
				| _ -> DTDMatchedResult (DTDChildren t))
		in
		match update elt with
		| DTDNext | DTDNotMatched ->
			(match tag with
			| None -> raise (Prove_error UnexpectedPCData)
			| Some t -> raise (Prove_error (UnexpectedTag t)))
		| DTDMatched ->
			dtd.current <- DTDEmpty
		| DTDMatchedResult r ->
			dtd.current <- DTDChild r



let prove_attrib dtd attr aname (atype,adef) accu =
	let aval = (try Some (List.assoc aname attr) with Not_found -> None) in
	(match atype, aval with
	| DTDCData, _ -> ()
	| DTDNMToken, None -> ()
	| DTDNMToken, Some v ->
		for i = 0 to String.length v - 1 do
			if not (is_nmtoken_char v.[i]) then raise (Prove_error (InvalidAttributeValue aname));
		done
	| DTDEnum l, None -> ()
	| DTDEnum l, Some v ->
		if not (List.exists ((=) v) l) then raise (Prove_error (InvalidAttributeValue aname)));
	match adef, aval with
	| DTDRequired, None -> raise (Prove_error (RequiredAttribute aname))
	| DTDFixed v, Some av when v <> av -> raise (Prove_error (InvalidAttributeValue aname))
	| DTDImplied, None -> accu
	| DTDFixed v , None
	| DTDDefault _, Some v
	| DTDDefault v, None
	| DTDRequired,  Some v
	| DTDImplied, Some v
	| DTDFixed _, Some v -> (aname,v) :: accu


let check_attrib ahash (aname,_) =
	try
		ignore(Hashtbl.find ahash aname);
	with
		Not_found -> raise (Prove_error (UnexpectedAttribute aname))

let rec do_prove dtd = function
	| PCData s ->
		prove_child dtd None;
		PCData s
	| Element (tag,attr,childs) ->
		let utag = String.uppercase tag in
		let uattr = List.map (fun (aname,aval) -> String.uppercase aname , aval) attr in
		prove_child dtd (Some utag);
		Stack.push (dtd.curtag,dtd.current) dtd.state;
		let elt = (try Hashtbl.find dtd.elements utag with Not_found -> raise (Prove_error (UnexpectedTag tag))) in
		let ahash = (try Hashtbl.find dtd.attribs utag with Not_found -> empty_hash) in
		dtd.curtag <- tag;
		dtd.current <- elt;
		List.iter (check_attrib ahash) uattr;
		let attr = Hashtbl.fold (prove_attrib dtd uattr) ahash [] in
		let childs = ref (List.map (do_prove dtd) childs) in
		(match dtd.current with
		| DTDAny
		| DTDEmpty -> ()
		| DTDChild elt ->
			let name = ref "" in
			let rec check = function
				| DTDTag t -> 
					name := t;
					false
				| DTDPCData when !childs = [] ->
					childs := [PCData ""];
					true
				| DTDPCData ->
					name := "#PCDATA";
					false
				| DTDOptional _ -> true
				| DTDZeroOrMore _ -> true
				| DTDOneOrMore e ->
					ignore(check e);
					false
				| DTDChoice l -> List.exists check l
				| DTDChildren l -> List.for_all check l
			in
			match check elt with
			| true -> ()
			| false -> raise (Prove_error (ChildExpected !name)));
		let ctag, cur = Stack.pop dtd.state in
		dtd.curtag <- tag;
		dtd.current <- cur;
		Element (tag,attr,!childs)

let prove dtd root xml =
	do_prove (start_prove dtd root) xml
