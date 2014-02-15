
open Common
open Parse_info
open Ast_js
open Module_js

(*********************************************************************)
(* temporary list of global stuff on which we currently punt 		 *)
(* we should move items out of this list as we model their semantics *)
(*********************************************************************)

let global_variables = [

				(* mixInto *)
				(* emptyFunction *)

	"window";
	"document";
	"global";
	"navigator";

	"Math";
	"Date";
	"Array";
	"FileReader";
	"Object";
	"RegExp";
	"Function";
	"String";
	"JX";
	"JSON";
	"FB";
	"Error";

	"Infinity";
	"undefined";

	"parseInt";
	"setInterval";
	"setTimeout";

	"define";
	"require";
	"requireLazy";
	"requireDynamic";

	"Arbiter"; 		(* @requires *)
	"jQuery";  		(* third-party *)
]

let global_methods = [
	"createClass"; 	(* React, ... *)

	"newClass";		(* UKI, ... *)

	"getInstance"; 	(* LogHistory, MessagingManager *)

	"extend"; 		(* Object.extend, jQuery.extend, ... *)
	"clone"; 		(* jQuery.clone *)
]




(**********************)
(* various statistics *)
(**********************)

let local = ref {
	local_requires = [];
	module_ = "";
	local_bindings = SMap.singleton "exports" (new_object SMap.empty);
}

(**************************)
(* manipulation of locals *)
(**************************)

let set_local s shape =
	local := {!local with local_bindings = !local.local_bindings +> SMap.add s shape }

let get_local s =
	try
		SMap.find s !local.local_bindings
	with _ ->
		if (List.mem s global_variables) then UnknownShape s
		else failwith ("Unable to find " ^ s)

(********************)
(* property set/get *)
(********************)

let get_property mapr x =
	let (_,map,_maps) = !mapr in
	try 
		SMap.find x map 
	with _ -> 
		PropertyShape (ObjectShape mapr, x)

let set_property mapr x shape =
	let (id,map,maps) = !mapr in
	mapr := (id,map +> SMap.add x shape, maps)

let set_object_property o x shape =
	match o with

	| ObjectShape mapr -> set_property mapr x shape
	| _ -> () (* TODO *)

let new_function() = FunctionShape (ref SMap.empty, ClassShape (new_object SMap.empty, new_class()))	

(***************************)
(* analysis of expressions *)
(***************************)

let rec analyze_expression = function

	| V(x,_) -> get_local x

	| Period(V("module",_),_,("exports",_)) -> get_local "exports"

	| Period(e,_,(x,_)) ->
		(match analyze_expression e with

			| ObjectShape mapr -> get_property mapr x

			| ClassShape (_, ObjectShape mapr) -> get_property mapr x

			| FunctionShape (_,ClassShape (_, ObjectShape mapr)) -> get_property mapr x

			| shape -> PropertyShape(shape,x) (* TODO *)
		)

	| Apply (V("require",_), _module) ->
		(match _module +> unparen +> uncomma with

			| [L (String (m,_))] when not(String.contains m '/') ->
				local := { !local with local_requires = m::!local.local_requires };
				RequireShape m

			| _ -> UnknownShape "unrecognized require" (* TODO *)
		)

	| Apply(V("mixin",_),mixins) ->
		MixinShape(
			ObjectShape (ref (
				fresh_id(),
				SMap.empty,
				(mixins +> unparen +> uncomma +> List.map analyze_expression)
			))	
		)

	| Apply(V("classWithMixins",_),classWithMixin) ->
		(match classWithMixin +> unparen +> uncomma with
			| [_class;mixin] ->
				ClassWithMixinShape(analyze_expression _class, analyze_expression mixin)
			| _ -> failwith "unsupported mixin"
		)

	| Apply(V("copyProperties",_), args) ->
		let args = args +> unparen +> uncomma in
		let e = analyze_expression(List.hd args) in
		(match e with

			| ObjectShape mapr 
			| ClassShape (_, ObjectShape mapr) 
			| FunctionShape (_,ClassShape (_, ObjectShape mapr)) ->
				let (id,map,maps) = !mapr in
				let shapes = List.tl args +> List.map analyze_expression in
				mapr := shapes +> List.fold_left
					(fun (id,map,maps) -> fun shape -> 
						match shape with
							| ObjectShape {contents = (_,map2,maps2)} -> 
								(id, SMap.fold SMap.add map map2, List.rev_append maps maps2)
							| _ -> (id, map, shape::maps)	
					) 
					(id, map,maps)
					(* (map, List.rev_append shapes maps) *)

			| _ -> () (* TODO *)
		);
		e

	| Apply(V("mixInto",_), args) ->
		let args = args +> unparen +> uncomma in
		let e = analyze_expression(List.hd args) in
		(match e with

			| ClassShape (_, ObjectShape mapr)
			| FunctionShape (_,ClassShape (_, ObjectShape mapr)) ->
				(match get_property mapr "prototype" with
					| ObjectShape mapr ->
						let (id,map,maps) = !mapr in
						let shapes = List.tl args +> List.map analyze_expression in
						mapr := (id,map, List.rev_append shapes maps)
					| _ -> failwith "prototype is not object"
				)

			| _ -> () (* TODO *)
		);
		e

	| Apply(V("merge",_), args) ->
		let args = args +> unparen +> uncomma in
		ObjectShape (ref (
			fresh_id(),
			SMap.empty,
			args +> List.map analyze_expression
		))

	| Apply(V("keyMirror",_), args) ->
		let args = args +> unparen +> uncomma in
		(match args with
			| [o] ->
				(match analyze_expression o with
					| ObjectShape {contents = (_,map,_)} -> new_object(map)
					| _ -> UnknownShape "unable to mirror"
				)
			| _ -> failwith "unable to mirror")

	| Apply(Period(V("Object",_),_,("create",_)), prototype) ->
		let prototype = prototype +> unparen +> uncomma in
		(match prototype with

			| [e] -> NewShape (ClassShape (
				new_object(SMap.empty),
				new_object(SMap.singleton "prototype" (analyze_expression e))
			  ))

			| _ -> failwith "unable to create"
		)

	| Apply(Period(_o,_,(s,_)),_) when List.mem s global_methods ->
		new_class() (* TODO *)

	| Apply(U((U_new,_),c),_) ->
		NewShape (analyze_expression c) (* TODO *)

	| Apply(f,_) ->
		ApplyShape (analyze_expression f)

	| Object props ->
		new_object (shapes_of_props (props +> unbrace +> uncomma))

	| Array _ ->
		ArrayShape

	| Function _finfo -> 
		new_function()

	| L _ ->
		LiteralShape

	| Assign(r,(A_eq,_), e) ->
		let shape = analyze_expression e in
		(match r with

			| V(x,_) -> set_local x shape

			| Period(V("global",_),_,_) -> ()

			| Period(V("module",_),_,("exports",_)) -> set_local "exports" shape

			| Period(o,_,(x,_)) ->
				let e = analyze_expression o in
				(match e with

					| ObjectShape mapr -> set_property mapr x shape

					| ClassShape (_, ObjectShape mapr) ->
						set_property mapr x shape

					| FunctionShape (_,ClassShape (_, ObjectShape mapr)) ->
						set_property mapr x shape

					| _ -> () (* TODO *)
				)

			| _ -> () (* TODO *)
		);
		shape

	| Bracket(a,_) ->
		ElementShape (analyze_expression a) (* TODO *)

	| Paren e -> analyze_expression (unparen e)

	| Seq(e1,_,e2) ->
		ignore(analyze_expression e1); analyze_expression e2

	| XhpHtml _ -> LiteralShape (* TODO *)

	| U(_,_) -> LiteralShape (* TODO *)

	| B(_,_,_) -> LiteralShape (* TODO *)

	| Conditional(_,_,_,_,_) -> LiteralShape (* TODO *)

	| This _ ->
		new_object SMap.empty (* TODO *)

	| e ->
		failwith (Utils_js.string_of_any (Expr e))



and shapes_of_props props =
	List.fold_left
		(fun shapes -> function
			| (PN_String (y,_),_,e) ->
				shapes +> SMap.add y (analyze_expression e)

			| _ -> shapes (* TODO: number-indexed property *)
		)
		SMap.empty props


let extract_fields imapr = function
	| St (ExprStmt(
		Assign(Period(This _,_,(field,_)),(A_eq,_),_)
		,_)) ->
		set_property imapr field (UnknownShape "missing annotation")
	| _ -> ()


let extract_extends = function
	| None -> rootclass
	| Some (_,e) -> analyze_expression e


(*************************)
(* analysis of toplevels *)
(*************************)

let analyze_toplevel_decls t = match t with

	| FunDecl finfo ->
		(match finfo.f_name with

			| Some (f,_) ->
				set_local f (new_function())

			| _ -> failwith (Utils_js.string_of_any (Toplevel t))
		)

	| ClassDecl cinfo ->
		let (c,_) = cinfo.c_name in
		set_local c (ClassShape (
			new_object SMap.empty,
			new_class()
		))

	| _ -> ()

let analyze_toplevel t = match t with

	| ClassDecl cinfo ->
		let (c,_) = cinfo.c_name in
		(match get_local c with

			| ClassShape (ObjectShape imapr, ObjectShape smapr) ->
				cinfo.c_body +> unbrace +> List.iter
					(function

						| Method(None, {f_name = Some ("constructor",_); f_body = body; _}) ->
							set_property smapr "constructor" (new_function());
							body +> unbrace +> List.iter (extract_fields imapr);

						| Method(None, {f_name = Some (f,_); _}) ->
							set_object_property (get_property smapr "prototype")
								f (new_function())

						| _ -> ()
					);
				set_property smapr "extends" (extract_extends cinfo.c_extends)

			| _ -> failwith "class not found"
		)

	| St (Variable(_,vdecls,_)) ->
		uncomma(vdecls) +> List.iter (
			fun vdecl ->
				let (x,_) = vdecl.v_name in
				match vdecl.v_init with

					| Some (_,e) ->
						set_local x
						(analyze_expression e)

					| None ->
						set_local x (UnknownShape "uninitialized variable")
		)

	| FunDecl _finfo -> ()

	| St (ExprStmt (e,_)) ->
		ignore(analyze_expression e)

	| St _ -> ()

	| FinalDef _ -> ()

	| _ -> failwith (Utils_js.string_of_any (Toplevel t))


(**************************)
(* parsing Haste comments *)
(**************************)

let rec parse_attributes =
	let rec helper value_and_words value = match value_and_words with
		| [] -> (value,[])
		| str :: _ when str.[0] = '@' -> (value, value_and_words)
		| str :: value_and_words -> helper value_and_words (str::value)

	in
	function
		| [] -> []
		| key :: value_and_words when key.[0] = '@' ->
			let (value,words) = helper value_and_words [] in
			(key,value)::(parse_attributes words)
		| _ :: words -> parse_attributes words


let parse_comment comment =
	let words = Str.split (Str.regexp "[ \t\n\\*/]+") comment in
	let pairs = parse_attributes words in
	pairs +> List.iter (function
		| ("@providesModule", [m]) ->
			local := { !local with module_ = m }
		| ("@providesLegacy", [m]) ->
			local := { !local with module_ = ("LEGACY::" ^ m) }
		| (_key,_value) ->
			()
	)

let analyze_tok = function
	| Parser_js.TComment comment ->
		(match comment.token with
			| OriginTok pinfo -> parse_comment (pinfo.str)
			| _ -> ()
		)
	| _ -> ()

(********************)
(* analysis of ASTs *)
(********************)

let extract_module_name file =
	Filename.chop_suffix (Filename.basename file) ".js"

let analyze file (ast,toks) =
	local := {
		module_ = "";
		local_bindings = SMap.singleton "exports" (new_object SMap.empty);
		local_requires = []
	};
	toks +> List.iter analyze_tok;
	(if (!local.module_ = "")
	 then local := { !local with module_ = extract_module_name file }
	 else () 
	);
	ast +> List.iter analyze_toplevel_decls;
	ast +> List.iter analyze_toplevel;
	get_local "exports"

let export_module _shape modules =
	modules +> SMap.add !local.module_ !local


