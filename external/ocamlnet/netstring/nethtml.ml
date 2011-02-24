(* $Id: nethtml.ml 1296 2009-11-18 13:27:41Z ChriS $
 * ----------------------------------------------------------------------
 *
 *)

open Nethtml_scanner;;

type document =
    Element of (string  *  (string*string) list  *  document list)
  | Data of string
;;


exception End_of_scan;;
exception Found;;


type element_class =         (* What is the class of an element? *)
  [ `Inline
  | `Block
  | `Essential_block
  | `None
  | `Everywhere
  ]
;;

type model_constraint =      (* The constraint the subelements must fulfill *)
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
;;

type simplified_dtd =
    (string * (element_class * model_constraint)) list


let ( |. ) a b = `Or(a,b);;
let ( -. ) a b = `Except(a,b);;


let block_elements =
  (* Only used for exclusions *)
  [ "p"; "dl"; "div"; "center"; "noscript"; "noframes"; "blockquote"; "form";
    "isindex"; "hr"; "table"; "fieldset"; "address"; "h1"; "h2"; "h3"; "h4";
    "h5"; "h6"; "pre"; "ul"; "ol"; "dir"; "menu" ];;

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
;;


let relax_dtd dtd =
  (* Changes (`Inline, `Inline) constraints into (`Inline, `Flow). *)
  let rec relax_model m =
    match m with
	`Inline -> `Flow
      | `Sub_exclusions(l,m') -> `Sub_exclusions(l,relax_model m')
      | other -> other
  in
  List.map
    (fun (name, (elclass, elconstr)) ->
       match elclass with
	   `Inline ->
	     (name, (elclass, relax_model elconstr))
	 | other ->
	     (name, (elclass, elconstr))
    )
    dtd
;;


let essential_blocks dtd elements =
  (* Changes the passed block elements into essential block elements *)
  List.map
    (fun (name, (elclass, elconstr)) ->
       match elclass with
	   `Block when List.mem name elements ->
	     (name, ( `Essential_block, elconstr))
         | other ->
	     (name, (elclass, elconstr))
    )
    dtd
;;


let relaxed_html40_dtd =
  essential_blocks
    (relax_dtd html40_dtd)
    [ "body"; "table"; "ol"; "ul"; "dl" ]
;;


let rec parse_comment buf =
  let t = scan_comment buf in
  match t with
      Mcomment ->
	let s = Lexing.lexeme buf in
	s ^ parse_comment buf
    | Eof ->
	raise End_of_scan
    | _ ->
	(* must be Rcomment *)
	""
;;


let rec parse_doctype buf =
  let t = scan_doctype buf in
  match t with
      Mdoctype ->
	let s = Lexing.lexeme buf in
	s ^ parse_doctype buf
    | Eof ->
	raise End_of_scan
    | _ ->
	(* must be Rdoctype *)
	""
;;


let rec parse_pi buf =
  let t = scan_pi buf in
  match t with
      Mpi ->
	let s = Lexing.lexeme buf in
	s ^ parse_pi buf
    | Eof ->
	raise End_of_scan
    | _ ->
	(* must be Rpi *)
	""
;;


let hashtbl_from_alist l =
  let ht = Hashtbl.create (List.length l) in
  List.iter
    (fun (k, v) ->
       Hashtbl.add ht k v)
    l;
  ht
;;


module S = struct
  type t = string
  let compare = (Pervasives.compare : string -> string -> int)
end

module Strset = Set.Make(S);;


let parse_document ?(dtd = html40_dtd) 
                   ?(return_declarations = false) 
                   ?(return_pis = false)
                   ?(return_comments = false) buf =
  let current_name = ref "" in
  let current_atts = ref [] in
  let current_subs = ref [] in
  let current_excl = ref Strset.empty in      (* current exclusions *)
  let stack = Stack.create() in
  let dtd_hash = hashtbl_from_alist dtd in

  let model_of element_name =
    if element_name = "" then
      (`Everywhere, `Any)
    else
      let extract =
	function
	    (eclass, `Sub_exclusions(_,m)) -> eclass, m
	  | m -> m
      in
      try
	extract(Hashtbl.find dtd_hash element_name)
      with
	  Not_found -> (`Everywhere, `Any)
  in

  let exclusions_of element_name =
    if element_name = "" then
      []
    else
      let extract =
	function
	    (eclass, `Sub_exclusions(l,_)) -> l
	  | _ -> []
      in
      try
	extract(Hashtbl.find dtd_hash element_name)
      with
	  Not_found -> []
  in

  let is_possible_subelement parent_element parent_exclusions sub_element =
    let (sub_class, _) = model_of sub_element in
    let rec eval m =
      match m with
	  `Inline     -> sub_class = `Inline
	| `Block      -> sub_class = `Block  || sub_class = `Essential_block
	| `Flow       -> sub_class = `Inline || sub_class = `Block ||
		         sub_class = `Essential_block
	| `Elements l -> List.mem sub_element l
	| `Any        -> true
	| `Or(m1,m2)  -> eval m1 || eval m2
	| `Except(m1,m2) -> eval m1 && not (eval m2)
	| `Empty      -> false
	| `Special    -> false
	| `Sub_exclusions(_,_) -> assert false
    in
    (sub_class = `Everywhere) || (
	      (not (Strset.mem sub_element parent_exclusions)) &&
	      let (_, parent_model) = model_of parent_element in
	      eval parent_model
	    )
  in

  let unwind_stack sub_name =
    (* If the current element is not a possible parent element for sub_name,
     * search the parent element in the stack.
     * Either the new current element is the parent, or there was no
     * possible parent. In the latter case, the current element is the
     * same element as before.
     *)
    let backup = Stack.create() in
    let backup_name = !current_name in
    let backup_atts = !current_atts in
    let backup_subs = !current_subs in
    let backup_excl = !current_excl in
    try
      while not (is_possible_subelement !current_name !current_excl sub_name) do
	(* Maybe we are not allowed to end the current element: *)
	let (current_class, _) = model_of !current_name in
	if current_class = `Essential_block then raise Stack.Empty;
	(* End the current element and remove it from the stack: *)
	let grant_parent = Stack.pop stack in
	Stack.push grant_parent backup;        (* Save it; may we need it *)
	let (gp_name, gp_atts, gp_subs, gp_excl) = grant_parent in
	(* If gp_name is an essential element, we are not allowed to close
	 * it implicitly, even if that violates the DTD.
	 *)
	let current = Element (!current_name, !current_atts, 
			       List.rev !current_subs) in
	current_name := gp_name;
	current_atts := gp_atts;
	current_excl := gp_excl;
	current_subs := current :: gp_subs
      done;
    with
	Stack.Empty ->
	  (* It did not work! Push everything back to the stack, and
	   * resume the old state.
	   *)
	  while Stack.length backup > 0 do
	    Stack.push (Stack.pop backup) stack
	  done;
	  current_name := backup_name;
	  current_atts := backup_atts;
	  current_subs := backup_subs;
	  current_excl := backup_excl
  in

  let parse_atts() =
    let rec next_no_space p_string =
      (* p_string: whether string literals in quotation marks are allowed *)
      let tok =
	if p_string then
	  scan_element_after_Is buf
	else
	  scan_element buf in
      match tok with
	  Space _ -> next_no_space p_string
	| t -> t
    in

    let rec parse_atts_lookahead next =
      match next with
	| Relement  -> ( [], false )
	| Relement_empty  -> ( [], true )
      	| Name n ->
	    ( match next_no_space false with
	      	Is ->
		  ( match next_no_space true with
		      Name v ->
			let toks, is_empty =
			  parse_atts_lookahead (next_no_space false) in
		      	( (String.lowercase n, v) :: toks, is_empty )
		    | Literal v ->
			let toks, is_empty =
			  parse_atts_lookahead (next_no_space false) in
		      	( (String.lowercase n,v) :: toks, is_empty )
		    | Eof ->
		      	raise End_of_scan
		    | Relement ->
		      	(* Illegal *)
		      	( [], false )
		    | Relement_empty ->
		      	(* Illegal *)
		      	( [], true )
		    | _ ->
		      	(* Illegal *)
		      	parse_atts_lookahead (next_no_space false)
		  )
	      | Eof ->
		  raise End_of_scan
	      | Relement ->
		  (* <tag name> <==> <tag name="name"> *)
		  ( [ String.lowercase n, String.lowercase n ], false)
	      | Relement_empty ->
		  (* <tag name> <==> <tag name="name"> *)
		  ( [ String.lowercase n, String.lowercase n ], true)
	      | next' ->
		  (* assume <tag name ... > <==> <tag name="name" ...> *)
		  let toks, is_empty = 
		    parse_atts_lookahead next' in
		  ( ( String.lowercase n, String.lowercase n ) :: toks,
		    is_empty)
	    )
      	| Eof ->
	    raise End_of_scan
      	| _ ->
	    (* Illegal *)
	    parse_atts_lookahead (next_no_space false)
    in
    parse_atts_lookahead (next_no_space false)
  in

  let rec parse_special name =
    (* Parse until </name> *)
    match scan_special buf with
      | Lelementend n ->
	  if String.lowercase n = name then
	    ""
	  else
	    "</" ^ n ^ parse_special name
      | Eof ->
	  raise End_of_scan
      | Cdata s ->
	  s ^ parse_special name
      | _ ->
	  (* Illegal *)
	  parse_special name
  in

  let rec skip_element() =
    (* Skip until ">" (or "/>") *)
    match scan_element buf with
      | Relement | Relement_empty ->
	  ()
      | Eof ->
	  raise End_of_scan
      | _ ->
	  skip_element()
  in

  let rec parse_next() =
    let t = scan_document buf in
    match t with
      | Lcomment ->
	  let comment = parse_comment buf in
	  if return_comments then
	    current_subs := (Element("--",["contents",comment],[])) :: !current_subs;
	  parse_next()
      | Ldoctype ->
	  let decl = parse_doctype buf in
	  if return_declarations then
	    current_subs := (Element("!",["contents",decl],[])) :: !current_subs;
	  parse_next()
      | Lpi ->
	  let pi = parse_pi buf in
	  if return_pis then
	    current_subs := (Element("?",["contents",pi],[])) :: !current_subs;
	  parse_next()
      | Lelement name ->
	  let name = String.lowercase name in
	  let (_, model) = model_of name in
	  ( match model with
		`Empty ->
		  let atts, _ = parse_atts() in
		  unwind_stack name;
		  current_subs := (Element(name, atts, [])) :: !current_subs;
		  parse_next()
	      | `Special ->
		  let atts, is_empty = parse_atts() in
		  unwind_stack name;
		  let data = 
		    if is_empty then 
		      ""
		    else (
		      let d = parse_special name in
		      (* Read until ">" *)
		      skip_element();
		      d
		    ) in
		  current_subs := (Element(name, atts, [Data data])) :: !current_subs;
		  parse_next()
	      | _ ->
		  let atts, is_empty = parse_atts() in
		  (* Unwind the stack until we find an element which can be
		   * the parent of the new element:
		   *)
		  unwind_stack name;
		  if is_empty then (
		    (* Simple case *)
		    current_subs := (Element(name, atts, [])) :: !current_subs;
		  )
		  else (
		    (* Push the current element on the stack, and this element
		     * becomes the new current element:
		     *)
		    let new_excl = exclusions_of name in
		    Stack.push 
		      (!current_name, 
		       !current_atts, !current_subs, !current_excl)
		      stack;
		    current_name := name;
		    current_atts := atts;
		    current_subs := [];
		    List.iter
		      (fun xel -> current_excl := Strset.add xel !current_excl)
		      new_excl;
		  );
		  parse_next()
	  )
      | Cdata data ->
	  current_subs := (Data data) :: !current_subs;
	  parse_next()
      | Lelementend name ->
	  let name = String.lowercase name in
	  (* Read until ">" *)
	  skip_element();
	  (* Search the element to close on the stack: *)
	  let found = 
	    (name = !current_name) ||
	    try
	      Stack.iter
		(fun (old_name, _, _, _) ->
		   if name = old_name then raise Found;
		   match model_of old_name with
		       `Essential_block, _ -> raise Not_found;
			 (* Don't close essential blocks implicitly *)
		     | _ -> ())
		stack;
	      false
	    with
		Found -> true
	      | Not_found -> false
	  in
	  (* If not found, the end tag is wrong. Simply ignore it. *)
	  if not found then
	    parse_next()
	  else begin
	    (* If found: Remove the elements from the stack, and append
	     * them to the previous element as sub elements
	     *)
	    while !current_name <> name do
	      let old_name, old_atts, old_subs, old_excl = Stack.pop stack in
	      current_subs := (Element (!current_name, !current_atts,
					List.rev !current_subs)) :: old_subs;
	      current_name := old_name;
	      current_atts := old_atts;
	      current_excl := old_excl
	    done;
	    (* Remove one more element: the element containing the element
	     * currently being closed.
	     *)
	    let old_name, old_atts, old_subs, old_excl = Stack.pop stack in
	    current_subs := (Element (!current_name, !current_atts,
				      List.rev !current_subs)) :: old_subs;
	    current_name := old_name;
	    current_atts := old_atts;
	    current_excl := old_excl;
	    (* Go on *)
	    parse_next()
	  end
      | Eof ->
	  raise End_of_scan
      | _ ->
	  parse_next()
  in
  try
    parse_next();  (* never returns. Will get a warning X *)
    assert false
  with
      End_of_scan ->
	(* Close all remaining elements: *)
	while Stack.length stack > 0 do
	  let old_name, old_atts, old_subs, old_excl = Stack.pop stack in
	  current_subs := Element (!current_name,
				   !current_atts,
				   List.rev !current_subs) :: old_subs;
	  current_name := old_name;
	  current_atts := old_atts;
	  current_excl := old_excl
	done;
	List.rev !current_subs
;;

let parse ?dtd ?return_declarations ?return_pis ?return_comments ch =
  let buf = Netchannels.lexbuf_of_in_obj_channel ch in
  parse_document ?dtd ?return_declarations ?return_comments ?return_pis buf
;;  


type xmap_value =
  | Xmap_attribute of string * string * string (* elname, attname, attval *)
  | Xmap_data of string option * string        (* elname, pcdata *)

let rec xmap f surelem doc =
  (* surdoc: surrounding element *)
  match doc with
    | Element(name,atts,subdocs) ->
	(match name with
	   | "!"
	   | "?"
	   | "--" ->
	       Element(name,atts,xmap_list f None subdocs)
	   | _ ->
	       let atts' =
		 List.map
		   (fun (aname,aval) ->
		      aname, f (Xmap_attribute(name, aname, aval))
		   )
		   atts
	       in
	       let subdocs' =  xmap_list f (Some name) subdocs in
	       Element(name,atts',subdocs')
	)
    | Data s ->
	Data(f (Xmap_data(surelem,s)))
and xmap_list f surelem l = List.map (xmap f surelem) l;;

let map_list f l =
  xmap_list
    (function
       | Xmap_attribute(_, _, v) -> f v
       | Xmap_data(_, v) -> f v
    )
    None
    l


let encode ?(enc = `Enc_iso88591) ?(prefer_name = true) ?(dtd = html40_dtd)
           dl = 
  let enc_string =
    Netencoding.Html.encode 
      ~in_enc:enc ~out_enc:`Enc_usascii ~prefer_name () in
  let dtd_hash = hashtbl_from_alist dtd in
  let enc_node = 
    function
      | Xmap_attribute(_, _, v) -> enc_string v
      | Xmap_data(None, v) -> enc_string v
      | Xmap_data(Some el, v) ->
	  let is_special =
	    try snd(Hashtbl.find dtd_hash el) = `Special
	    with Not_found -> false in
	  if is_special then
	    v
	  else
	    enc_string v in
  xmap_list enc_node None dl
;;

let decode ?(enc = `Enc_iso88591) ?subst ?entity_base ?lookup 
           ?(dtd = html40_dtd)
           dl = 
  let dec_string =
    Netencoding.Html.decode 
      ~in_enc:enc ~out_enc:enc ?subst ?entity_base ?lookup () in
  let dtd_hash = hashtbl_from_alist dtd in
  let dec_node = 
    function
      | Xmap_attribute(_, _, v) -> dec_string v
      | Xmap_data(None, v) -> dec_string v
      | Xmap_data(Some el, v) ->
	  let is_special =
	    try snd(Hashtbl.find dtd_hash el) = `Special
	    with Not_found -> false in
	  if is_special then
	    v
	  else
	    dec_string v in
  xmap_list dec_node None dl
;;


let quote_quot_re = Netstring_str.regexp "\"";;

let write_ ~dtd ~xhtml write_os doc =
  let quote_quot s =
    Netstring_str.global_substitute quote_quot_re 
      (fun _ _ -> "&quot;")
      s
  in
  let rec trav doc =
    match doc with
	Element(name,atts,subdocs) ->
	  ( match name with
		"!" ->
		  write_os "<!";
		  write_os (List.assoc "contents" atts);
		  write_os ">";
	      | "?" ->
		  write_os "<?";
		  write_os (List.assoc "contents" atts);
		  write_os ">";
	      | "--" ->
		  write_os "<!--";
		  write_os (List.assoc "contents" atts);
		  write_os "-->";
	      | _ ->
		  let is_empty =
		    try 
		      let _, constr = List.assoc name dtd in
		      constr = `Empty
		    with
			Not_found -> false
		  in
		  write_os "<";
		  write_os name;
		  List.iter
		    (fun (aname,aval) ->
		       write_os " ";
		       write_os aname;
		       write_os "=\"";
		       write_os (quote_quot aval);
		       write_os "\"";
		    )
		    atts;
		  if is_empty then
		    (* Ignore subdocs (even if <> []) because they should
		       not be there. *)
		    write_os (if xhtml then "/>" else ">")
                  else begin
		    write_os ">";
		    List.iter trav subdocs;
		    write_os "</";
		    write_os name;
		    write_os ">";
		  end
	  )
      | Data s ->
	  write_os s
  in
  try
    List.iter trav doc
  with
      Not_found -> failwith "write"
;;

let write ?(dtd = html40_dtd) ?(xhtml = true) ch doc =
  write_ ~dtd ~xhtml (ch # output_string) doc
