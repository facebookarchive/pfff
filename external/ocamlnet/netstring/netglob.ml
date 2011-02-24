(* $Id: netglob.ml 1520 2010-12-22 23:52:22Z gerd $ *)

open Netglob_lex
open Printf

type glob_expr = glob_expr_atom list

and glob_expr_atom =
    [ `Literal of string
    | `Star
    | `Qmark
    | `Bracket of (bool * glob_set)
    | `Brace of glob_expr list
    | `Tilde of string 
    ]

and glob_set = < set : (int * int) list >


type valid_glob_expr =
    { pat : glob_expr;
      encoding : Netconversion.encoding;
    }

exception Bad_glob_expr of string

exception Unsupported_expr of string

class type user_info =
object
  method path_encoding : Netconversion.encoding option
  method home_directory : string -> string
end

class type glob_fsys =
object
  method path_encoding : Netconversion.encoding option
  method read_dir : string -> string list
  method file_is_dir : string -> bool
  method file_exists : string -> bool
end

type glob_mode = [ `Existing_paths
                 | `All_paths
                 | `All_words
                 ]

type pattern = [ `String of string | `Expr of valid_glob_expr ]


let literal_glob_expr enc s =
  { pat = [ `Literal s ];
    encoding = enc
  }


let reparse_bracket_expr enc l =
  (* In order to support multi-byte encodings, reparse the expression
     now. For simplifying this, we require that ranges (like c-d) are
     purely ASCII. So only the chars outside ranges need to be reparsed
   *)
  let rec collect buf toks =
    match toks with
      | Bracket_char c :: toks' ->
	  Buffer.add_char buf c;
	  collect buf toks'
      | Bracket_range(c1,c2) as tok :: toks' ->
	  let new_toks = reparse buf in
	  new_toks @ [tok] @ collect (Buffer.create 80) toks'
      | Bracket_code _ :: _ ->
	  assert false
      | Bracket_end :: _
      | [] ->
	  reparse buf

  and reparse buf =
    let s = Buffer.contents buf in
    let codes = ref [] in
    ( try
	Netconversion.ustring_iter enc (fun i -> codes := i :: !codes) s
      with _ -> raise Lexing_Error
    );
    List.rev_map
      (fun i -> Bracket_code i)
      !codes
  in

  collect (Buffer.create 80) l

    
let parse_glob_expr
      ?(encoding = `Enc_iso88591)
      ?(enable_star = true)
      ?(enable_qmark = true)
      ?(enable_brackets = true)
      ?(enable_braces = true)
      ?(enable_tilde = true)
      ?(enable_escape = true)
      s =

  if not (Netconversion.is_ascii_compatible encoding) then
    failwith
      "Netglob.parse_glob_expr: the encoding is not ASCII-compatible";
  
  let feat =
    { enable_star = enable_star;
      enable_qmark = enable_qmark;
      enable_brackets = enable_brackets;
      enable_braces = enable_braces;
      enable_tilde = enable_tilde;
      enable_escape = enable_escape;
      escaped = false;
    } in

  let rec collect_until lexbuf =
    let tok = glob_expr feat lexbuf in
    if tok = Glob_end then
      []
    else
      tok :: (collect_until lexbuf)
  in

  let rec process_brace_list current list =
    match list with
      | Brace_literal s :: list' ->
          let gl = collect_until (Lexing.from_string s) in
          process_brace_list (current @ gl) list'
      | Brace_braces l :: list' ->
          process_brace_list (current @ [Glob_braces l]) list'
      | Brace_comma :: list' ->
          let ge = process_glob_list [] current in
          ge :: process_brace_list [] list'
      | Brace_end :: _ ->
          assert false
      | [] ->
          let ge = process_glob_list [] current in
          [ ge ]

  and process_glob_list acc list =
    match list with
      | Glob_star :: list' ->
          ( match acc with
              | `Star :: acc' ->
                  (* Ignore the second star! *)
                  process_glob_list acc list'
              | _ ->
                  process_glob_list (`Star :: acc) list'
          )
      | Glob_qmark :: list' ->
          process_glob_list (`Qmark :: acc) list'
      | Glob_brackets (neg,btoks) :: list' ->
          let set =
            List.map
              (function
                 | Bracket_char c ->
		     assert false
                 | Bracket_range (c1,c2) -> (* c1, c2 are ASCII *)
                     (Char.code c1, Char.code c2)
		 | Bracket_code i ->
		     (i, i)
                 | Bracket_end ->
                     assert false
              )
              (reparse_bracket_expr encoding btoks) in
	  let set_obj = ( object method set = set end ) in
          process_glob_list (`Bracket(neg,set_obj) :: acc) list'
      | Glob_braces btoks :: list' ->
          let alts = process_brace_list [] btoks in
          process_glob_list (`Brace alts :: acc) list'
      | Glob_literal s :: list' ->
          if s <> "" then
            ( match acc with
                | `Literal s' :: acc' ->
                    process_glob_list (`Literal(s' ^ s) :: acc') list'
                | _ ->
                    process_glob_list (`Literal s :: acc) list'
            )
          else
            process_glob_list acc list'
      | Glob_tilde(s,slash) :: list' ->
	  let atoms =
	    if slash then [ `Literal "/"; `Tilde s ] else [ `Tilde s ] in
	  process_glob_list ( atoms @ acc ) list'
      | Glob_end :: _ ->
          assert false
      | [] ->
          List.rev acc
  in

  try
    let glob_list = 
      collect_until (Lexing.from_string s) in
    
    let glob_expr =
      process_glob_list [] glob_list in

    { pat = glob_expr;
      encoding = encoding
    }

  with
    | Bracket_Unsupported ->
        raise (Unsupported_expr s)
    | Lexing_Error ->
        raise (Bad_glob_expr s)


let validate_glob_expr enc expr =
  let checkenc s =
    try Netconversion.verify enc s
    with _ ->
      failwith "Netglob.validate_glob_expr: literal does not conform \
                to selected pattern encoding" in

  let rec validate ge =
    match ge with
      | `Literal s :: ge' ->
	  if s = "" then
	    failwith "Netglob.validate_glob_expr: empty literal";
	  checkenc s;
	  validate ge'
      | `Bracket(_,set) :: ge' ->
	  List.iter
	    (fun (j,k) ->
	       if j < 0 || k < 0 || j > k then
		 failwith "Netglob.validate_glob_expr: bad bracket set";
	    )
	    set#set
      | `Brace l :: ge' ->
	  List.iter validate l;
	  validate ge'
      | `Tilde s :: ge' ->
	  checkenc s;
	  validate ge'
      | _ :: ge' ->
	  validate ge'
      | [] ->
	  () in
  if not (Netconversion.is_ascii_compatible enc) then
    failwith
      "Netglob.validate_glob_expr: the encoding is not ASCII-compatible";
  validate expr;
  { pat = expr;
    encoding = enc
  }

let recover_glob_expr expr =
  expr.pat

let encoding_of_glob_expr expr =
  expr.encoding


(* A more efficient representation for sets: *)

type eff_set =
    { ascii : bool array;
      non_ascii : (int, unit) Hashtbl.t
    }

let to_eset set =
  let ascii = Array.create 128 false in
  let non_ascii = Hashtbl.create 13 in
  List.iter
    (fun (k0,k1) ->
       assert(k0 <= k1);
       for p = k0 to k1 do
	 if p < 128 then
	   ascii.(p) <- true
	 else
	   Hashtbl.replace non_ascii p ()
       done
    )
    set;
  { ascii = ascii; non_ascii = non_ascii }


let rec mem_eset code eset =
  if code >= 0 && code < 128 then
    eset.ascii.(code)
  else
    Hashtbl.mem eset.non_ascii code


let size_eset eset =
  let n = ref 0 in
  for k = 0 to 127 do
    if eset.ascii.(k) then incr n
  done;
  !n + Hashtbl.length eset.non_ascii


let ascii_ranges eset =
  let ranges = ref [] in
  let inrange = ref None in
  for k = 0 to 127 do
    let p = eset.ascii.(k) in
    match !inrange with
      | None ->
	  if p then inrange := Some k
      | Some q ->
	  if not p then (
	    ranges := (q, k-1) :: !ranges;
	    inrange := None;
	  )
  done;
  ( match !inrange with
      | None -> ()
      | Some q -> ranges := (q, 127) :: !ranges
  );
  List.rev !ranges


let rec exclude_set codes set =
  match set with
      [] -> []
    | (x,y) :: set' ->
        let x' = if List.mem x codes then x+1 else x in
        let y' = if List.mem y codes then y-1 else y in
        if x = x' && y = y' && x <= y then
          (x,y) :: exclude_set codes set'
        else if x' <= y' then 
          exclude_set codes ( (x',y') :: set')
        else
          exclude_set codes set'


let print_set buf encoding neg_char negated set =
  (* Always produce a portable expression: *)
  let eset = to_eset set in

  (* Check for special characters: *)
  let want_minus    = mem_eset (Char.code '-') eset in
  let want_rbracket = mem_eset (Char.code ']') eset in
  let want_circum   = mem_eset (Char.code '^') eset in
  let want_exclam   = mem_eset (Char.code '!') eset in
  let size = size_eset eset in
  
  (* Check for very special sets: *)
  if not negated && want_circum && size = 1 then
    Buffer.add_string buf "^"  (* "[^]" would not be portable enough *)
  else if not negated && want_exclam && size = 1 then
    Buffer.add_string buf "!"  (* "[!]" would not be portable enough *)
  else if not negated && want_circum && want_exclam && size = 2 then
    failwith "print_glob_expr"
      (* There is no portable representation *)
  else (
    (* First create a set expression where the special characters
     * '-', ']', '^', and '!' do not occur literally.
     *)
    let empty = ref true in
    let buf' = Buffer.create 200 in
    let ascii_part = ascii_ranges eset in
    let ascii_part' = 
      exclude_set (List.map Char.code ['-'; ']'; '^'; '!']) ascii_part in
    let ascii_part'_eset = to_eset ascii_part' in
    List.iter
      (fun (x0,x1) ->
         if x0 = x1 then (
           Buffer.add_char buf' (Char.chr x0);
           empty := false;
         ) 
          else if x0 <= x1 then (
           Buffer.add_char buf' (Char.chr x0);
           Buffer.add_char buf' '-';
           Buffer.add_char buf' (Char.chr x1);
           empty := false;
         )
      )
      ascii_part';
    (* The non-ascii part is easy: *)
    Hashtbl.iter
      (fun code _ ->
	 let encoded =
	   Netconversion.ustring_of_uarray encoding [| code |] in
	 Buffer.add_string buf' encoded
      )
      eset.non_ascii;
    (* Check which of the special characters are already covered
     * by ranges:
     *)
    let done_minus    = mem_eset (Char.code '-') ascii_part'_eset in
    let done_rbracket = mem_eset (Char.code ']') ascii_part'_eset in
    let done_circum   = mem_eset (Char.code '^') ascii_part'_eset in
    let done_exclam   = mem_eset (Char.code '!') ascii_part'_eset in
    (* Begin with printing *)
    Buffer.add_string
      buf 
      (if negated then "[" ^ String.make 1 neg_char else "[");
    (* ']' must always be the first character of the set: *)
    if want_rbracket && not done_rbracket then (
      Buffer.add_string buf "]";
      empty := false;
    );
    Buffer.add_buffer buf buf';
    (* '-' must be the first or the last character; '^' and '!' must
     * not be the first character. So we usually print these
     * characters in the order "^!-". One case is special: We have
     * not yet printed any character. Then, "-" must be printed
     * first (if member of the set), or we have one of the very
     * special cases already tested above.
     *)
    if !empty then (
      if want_minus && not done_minus then Buffer.add_char buf '-';
      if want_circum && not done_circum then Buffer.add_char buf '^';
      if want_exclam && not done_exclam then Buffer.add_char buf '!';
    ) else (
      if want_circum && not done_circum then Buffer.add_char buf '^';
      if want_exclam && not done_exclam then Buffer.add_char buf '!';
      if want_minus && not done_minus then Buffer.add_char buf '-';
    );
    Buffer.add_char buf ']';
  )


let esc_re = Netstring_pcre.regexp "[][*?{},\\\\~]";;

let esc_subst m s =
  "\\" ^ Netstring_pcre.matched_group m 0 s

let print_glob_expr ?(escape_in_literals=true) expr =
  let buf = Buffer.create 200 in
  let rec print gl =
    match gl with
      | `Literal s :: gl' ->
          Buffer.add_string buf
	    (if escape_in_literals then
	       Netstring_pcre.global_substitute esc_re esc_subst s
	     else
	       s
	    );
          print gl'
      | `Star :: gl' ->
          Buffer.add_string buf "*";
          print gl'
      | `Qmark :: gl' ->
          Buffer.add_string buf "?";
          print gl'
      | `Bracket (negated,set) :: gl' ->
          print_set buf expr.encoding '!' negated set#set;
          print gl'
      | `Brace ge_list :: gl' ->
          Buffer.add_string buf "{";
          let first = ref true in
          List.iter 
            (fun ge ->
               if not !first then Buffer.add_string buf ",";
               print ge;
            )
            ge_list;
          Buffer.add_string buf "}";
          print gl'
      | `Tilde s :: gl' ->
	  Buffer.add_char buf '~';
	  Buffer.add_string buf s;
	  print gl'
      | [] ->
          ()
  in
  print expr.pat;
  Buffer.contents buf


class local_user_info() =
  let pe =
    match Sys.os_type with
      | "Win32" ->
	  Netconversion.user_encoding()
      | _ -> None in
object
  method path_encoding = pe

  method home_directory name =
    (* Win32: only the HOME method works *)
    try
      if name = "" then (
	try Sys.getenv "HOME"
	with Not_found ->
	  let pw = Unix.getpwuid(Unix.getuid()) in
	  pw.Unix.pw_dir
      ) else
	(Unix.getpwnam name).Unix.pw_dir
    with
      | _ -> raise Not_found
end


let local_user_info = new local_user_info


let rec product f l1 l2 =
  match l1 with
      [] ->
        []
    | x1 :: l1' ->
        List.map (fun x2 -> f x1 x2) l2 @ product f l1' l2


let rec expand_braces ge =
  match ge with
    | [] ->
        [ [] ]
    | `Brace gelist :: ge' ->
        let gelist' = 
          List.flatten (List.map expand_braces gelist) in
        let ge_alts' = expand_braces ge' in
        product ( @ ) gelist' ge_alts'
	  
    | any :: ge' ->
        let ge_alts' = expand_braces ge' in
        List.map (fun ge_alt' -> any :: ge_alt') ge_alts'


let rec expand_tildes encoding user_info ge =
  match ge with
    | [] ->
        []
    | `Tilde name :: ge' ->
	let atom =
	  try
	    let dir = user_info#home_directory name in
	    if dir="" then raise Not_found; (* empty literals not allowed *)
	    ( match user_info#path_encoding with
		| None -> `Literal dir
		| Some ui_enc ->
		    if ui_enc = encoding then
		      `Literal dir
		    else
		      `Literal
			(Netconversion.convert
			   ~in_enc:ui_enc ~out_enc:encoding dir)
	    )
	  with Not_found ->
	    `Literal ("~" ^ name) in
	atom :: expand_tildes encoding user_info ge'
    | any :: ge' ->
	any :: expand_tildes encoding user_info ge'


let expand_glob_expr ?(user_info=local_user_info())
                     ?(expand_brace=true) ?(expand_tilde=true) expr =
  let pat' =
    if expand_tilde then
      expand_tildes expr.encoding user_info expr.pat
    else
      expr.pat in
  let pat_l =
    if expand_brace then
      expand_braces pat'
    else
      [pat'] in
  List.map (fun p -> { expr with pat = p }) pat_l


let period = Char.code '.'
let slash = Char.code '/'

let match_glob_expr ?(protect_period=true) ?(protect_slash=true)
                    ?encoding
                    expr s =
  let esets = Hashtbl.create 5 in
  let get_eset set =
    try Hashtbl.find esets set
    with Not_found ->
      let eset = to_eset set#set in
      Hashtbl.add esets set eset;
      eset in

  let u = 
    Netconversion.uarray_of_ustring 
      ( match encoding with
	  | None -> expr.encoding
	  | Some e -> e
      )
      s in
  let n = Array.length u in

  let leading_period p =
    u.(p) = period && 
      (p = 0 || (protect_slash && u.(p - 1) = slash)) in

  let rec match_at c ge =
    match ge with
      | `Literal lit :: ge' ->
	  let lit_u = Netconversion.uarray_of_ustring expr.encoding lit in
	  let lit_n = Array.length lit_u in
	  let ok =
	    try
	      for k = 0 to lit_n - 1 do
		if c+k >= n then raise Not_found;
		let code = u.(c+k) in
		if code <> lit_u.(k) then raise Not_found;
	      done;
	      true
	    with 
	      | Not_found -> false in
	  ok && match_at (c+lit_n) ge'	  
      | `Star :: ge' ->
	  let k = ref 0 in
	  let cont = ref true in
	  let found = ref false in
	  while c + !k <= n && not !found && !cont do
	    found := match_at (c + !k) ge';
	    if c + !k < n then
	      cont :=
		(not protect_period || not (leading_period (c + !k))) &&
		   (not protect_slash || u.(c + !k) <> slash);
	    incr k;
	  done;
	  !found
      | `Qmark :: ge' ->
	  let ok =
	    c < n && 
	      (not protect_period || not (leading_period c)) &&
	      (not protect_slash || u.(c) <> slash) in
	  ok && match_at (c+1) ge'	      
      | `Bracket(neg,set) :: ge' ->
	  let ok =
	    c < n && (
	      let code = u.(c) in
	      (not protect_slash || code <> slash) && 
		(not protect_period || not (leading_period c)) && (
		  let eset = get_eset set in
		  let is_mem = mem_eset code eset in
		  (neg <> is_mem)
		)
	    ) in
	  ok &&
	    match_at (c+1) ge'
      | `Brace _ :: _ ->
	  failwith "Netglob.match_glob_expr: found `Brace subpattern"
      | `Tilde _ :: _ ->
	  failwith "Netglob.match_glob_expr: found `Tilde subpattern"
      | [] ->
	  c = n in

  match_at 0 expr.pat


let skip_slashes s k =
  let l = String.length s in
  let j = ref k in
  while !j < l && s.[!j] = '/' do incr j done;
  !j

let rev_skip_slashes s k =
  let j = ref k in
  while !j >= 0 && s.[!j] = '/' do decr j done;
  !j

let search_slash s =
  let k = String.index s '/' in
  let j = skip_slashes s (k+1) in
  (k, j)


let split_glob_expr expr =

  let rec split_loop is_first acc ge =
    (* acc: accumulates the current component *)
    match ge with
      | [] ->
          [ List.rev acc ]
      | (`Literal s as atom) :: ge' ->
          assert(s <> "");
          ( try 
	      let (k,j) = search_slash s in    (* or Not_found *)
              let l = String.length s in
              let s1 = String.sub s 0 k in         (* part before '/' *)
              let s2 = String.sub s j (l - j) in   (* part after '/' *)
              if is_first && k = 0 then (
                (* Case: rooted expression *)
                let ge'' =
                  if s2 <> "" then (`Literal s2) :: ge' else ge' in
                let comps = split_loop false [] ge'' in
		(* N.B. comps is a list of lists... *)
                match comps with
                  | ( (`Literal s3) :: r ) :: l ->
                      ( `Literal("/" ^ s3) :: r) :: l
                  | r :: l ->
                      (`Literal "/" :: r) :: l
                  | [] ->
                      [ [ `Literal "/" ] ]
              )
              else
                if ge' = [] && s2 = "" then (
                  (* Case: component matches only directory *)
                  [ List.rev (`Literal (s1 ^ "/") :: acc) ]
                )
                else (
                  let acc' = 
                    if s1 <> "" then (`Literal s1)::acc else acc in
                  let ge'' =
                    if s2 <> "" then (`Literal s2) :: ge' else ge' in
                  (List.rev acc') :: split_loop false [] ge''
                )
            with
              | Not_found ->
                  split_loop false (atom::acc) ge'
          )
      | (`Star | `Qmark | `Bracket(_,_) as atom) :: ge' ->
          split_loop false (atom::acc) ge'

      | `Brace _ :: _ ->
          failwith "Netglob.split_glob_expr: brace expression found"

      | `Tilde _ :: _ ->
          failwith "Netglob.split_glob_expr: tilde expression found"
  in

  List.map
    (fun p -> { expr with pat = p })
    (split_loop true [] expr.pat)


let check_rooted_glob_expr expr =
  match expr.pat with
    | (`Literal s) :: r ->
	assert(s <> "");
	if s.[0] = '/' then (
          let j = skip_slashes s 1 in
          let l = String.length s in
          let s' = String.sub s j (l - j) in   (* part after '/' *)
          if s' = "" then 
	    Some { expr with pat = r }
          else
            Some { expr with pat = `Literal s' :: r }
	)
        else
          None
    | _ ->
        None


let check_directory_glob_expr expr =
  match List.rev expr.pat with
    | (`Literal s) :: r ->
	assert(s <> "");
	( try
            let l = String.length s in
	    if s.[l-1] <> '/' then raise Not_found;
            let k = rev_skip_slashes s (l-1) + 1 in
            let s' = String.sub s 0 k in   (* the part before '/' *)
            if s' = "" then
	      Some { expr with pat = List.rev r }
            else
	      Some { expr with pat = List.rev (`Literal s' :: r) }
          with
              Not_found -> None
        )
    | _ ->
        None

class of_dual_stream_fs (abs_fs:Netfs.stream_fs) rel_fs =
  let is_abs name = name <> "" && name.[0] = '/' in
  let fix name =
    if is_abs name then
      (abs_fs, name)
    else
      (rel_fs, "/" ^ name) in
object
  method path_encoding = abs_fs#path_encoding
  method read_dir name =
    let (fs,name) = fix name in
    try fs#readdir [] name with _ -> []
  method file_is_dir name =
    let (fs,name) = fix name in
    try fs#test [] name `D with _ -> false
  method file_exists name =
    let (fs,name) = fix name in
    try fs#test [] name `E with _ -> false
end


class of_stream_fs (fs:Netfs.stream_fs) =
  of_dual_stream_fs fs fs

let of_stream_fs = new of_stream_fs


class local_fsys ?encoding () =
  let abs_fs = Netfs.local_fs ?encoding () in
  let rel_fs = Netfs.local_fs ?encoding ~root:"." () in
  of_dual_stream_fs abs_fs rel_fs

let local_fsys = new local_fsys




let fn_concat d f =
  let l = String.length d in
  if l = 0 || d.[l-1] = '/' then
    d ^ f
  else
    d ^ "/" ^ f


let glob1 ?base_dir 
          ?(protect_period=true) 
          ?(fsys = local_fsys())
	  ?user_info
          ?(mode = `Existing_paths)
          expr =

  (* File names and paths are encoded as [fsys] demands it.
     The encoding of the pattern can be different!
   *)

  let rec collect_and_match base_dir generated_prefix components =
    match components with
      | [] ->
          if generated_prefix <> "" then [ generated_prefix ] else []
      | comp :: components' ->
          let full_path file =
            match base_dir with
              | Some d -> fn_concat d file
              | None   -> file in

          let dir_ge = check_directory_glob_expr comp in
          let comp' = 
            match dir_ge with 
              | Some ge' -> ge'
              | None -> comp in

          let check_for_match only_dirs e file =
	    (* file is encoded in fsys#path_encoding. For matching, we
	       need to convert it to the encoding of the pattern.
	     *)
	    try
	      let pe =
		match fsys#path_encoding with
		  | None -> `Enc_iso88591 (* so no conv errors possible *)
		  | Some pe -> pe in
	      match_glob_expr ~protect_period ~encoding:pe e file &&
		(not only_dirs || fsys#file_is_dir (full_path file))
	    with
	      | Netconversion.Cannot_represent _ -> false
          in
          
          let files =
            match comp'.pat with
              | [ `Literal s ] ->
		  (* s is encoded in expr.encoding. We need it here
		     in the fsys#encoding
		   *)
                  ( try
		      let s' =
			match fsys#path_encoding with
			  | None -> s
			  | Some pe -> 
			      Netconversion.convert 
				~in_enc:expr.encoding ~out_enc:pe s in
		      match mode with
			| `Existing_paths ->
			    let path = full_path s' in
                            if fsys # file_exists path then
			      [ s' ]
                            else
			      []
			| _ ->
                            [ s' ]
		    with Netconversion.Cannot_represent _ 
			 when mode = `Existing_paths -> []
                  )
              | _ ->
                  let only_dirs = components' <> [] || dir_ge <> None in
                  let file_list = fsys#read_dir (full_path ".") in
(*eprintf "Files in %s: %s\n%!" (full_path ".") (String.concat "," file_list);*)
                  List.filter (check_for_match only_dirs comp') file_list 
          in
          List.flatten
            (List.map
               (fun file -> 
                  let prefixed_file = 
                    fn_concat generated_prefix file 
                    ^ (if dir_ge <> None then "/" else "") in
                  
                  collect_and_match 
                    (Some(full_path file))
                    prefixed_file
                    components'
               )
               files
            )

  in

  let collect_and_match_0 components =
    match components with
      | comp :: components' ->
          ( match check_rooted_glob_expr comp with
              | None ->
                  collect_and_match base_dir "" components
              | Some comp' ->
                  if comp'.pat = [] then
                    (* Special case "/" *)
                    [ "/" ]
                  else
                    collect_and_match (Some "/") "/" (comp' :: components')
          )
      | [] ->
          []
  in

  let e_list = expand_glob_expr ?user_info expr in
  List.flatten
    (List.map
       (fun e' ->
          let l = collect_and_match_0 (split_glob_expr e') in
          if mode = `All_words && l = [] && e'.pat <> [] then
            [print_glob_expr e']
          else
            l
       )
       e_list
    )


let glob ?encoding ?base_dir ?protect_period ?fsys ?user_info ?mode pat =
  match pat with
    | `Expr e ->
	glob1 ?base_dir ?protect_period ?fsys ?user_info ?mode e
    | `String s ->
	let e =
	  parse_glob_expr ?encoding s in
	glob1 ?base_dir ?protect_period ?fsys ?user_info ?mode e
