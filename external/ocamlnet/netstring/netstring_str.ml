(* $Id: netstring_str.ml 1003 2006-09-24 15:17:15Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(* This implementation of Netstring_str uses the PCRE engine. The
 * syntax for regular expressions is compatible with previous versions.
 *)


(**********************************************************************)
(* Parsing types *)

type setatom =
  | Schar of char
  | Srange of (char * char)

and set = setatom list
;;


type re_term =
    Texact of string            (* literal characters *)
  | Tany                        (* . but no newline *)
  | Tnull
  | Tconcat of re_term list
  | Tstar of re_term            (* x* *)
  | Tplus of re_term            (* x+ *)
  | Toption of re_term          (* x? *)
  | Tset of set                 (* [...] *)
  | Tnegset of set              (* [^...] *)
  | Tbegline                    (* ^ *)
  | Tendline                    (* $ *)
  | Talt of re_term list        (* x\|y *)
  | Tgroup of (int * re_term)   (* \(...\) *)
  | Trefer of int               (* \i *)
  | Tinterval of (re_term * int * int)    (* x{n,m}. m=-1 means infinite *)
  | Twordchar                   (* \w *)
  | Tnowordchar                 (* \W *)
  | Twordbeg                    (* \< *)
  | Twordend                    (* \> *)
  | Twordbound                  (* \b *)
  | Tnowordbound                (* \B *)
  | Tbegbuf                     (* \` *)
  | Tendbuf                     (* \' *)
;;


(**********************************************************************)
(* Final types *)

type regexp = Pcre.regexp;;
type split_result = Str.split_result = Text of string | Delim of string;;

type result = Netstring_pcre.result ;;

(**********************************************************************)
(* Parse Str-style regexps, and convert to Pcre-style regexps *)

let scan_str_regexp re_string =

  let l = String.length re_string in
  let k = ref (-1) in
  let c = ref ' ' in
  let esc = ref false in
  let group = ref 1 in
  let n_open_groups = ref 0 in
  let closed_groups = Array.create 10 false in

  let next() =
    incr k;
    if ( !k < l ) then begin
      let c1 = re_string.[ !k ] in
      if c1 = '\\' then begin
	if !k < l then begin
	  incr k;
	  c := re_string.[ !k ];
	  esc := true
	end
	else
	  failwith "regexp: bad backslash"
      end
      else begin
	esc := false;
	c := c1
      end
    end
  in

  let next_noesc() =
    incr k;
    if ( !k < l ) then begin
      c := re_string.[ !k ];
      esc := false
    end
  in

  let rec scan_alternative () =
    let t1 = scan_concatenation () in
    if !k < l then begin
      if !esc & !c = '|' then begin
	next();
	match scan_alternative() with
	  Talt alist -> Talt (t1::alist)
	| t          -> Talt [ t1; t]
      end else t1
    end else t1

  and scan_concatenation () =
    let t1 = scan_repetition () in
    if t1 = Tnull then
      t1
    else
      let t2 = scan_concatenation() in
      match t2 with
	Tnull         -> t1
      |	Texact s2     -> begin
	                   match t1 with
			     Texact s1 -> Texact (s1 ^ s2)
			   | _         -> Tconcat [t1;t2]
			 end
      |	Tconcat clist -> Tconcat (t1::clist)
      |	_             -> Tconcat [ t1; t2 ]

  and scan_repetition () =
    let t1 = ref (scan_literal_or_group ()) in
    let continue = ref true in
    while !continue do
      if !k < l & not !esc then begin
      	match !c with
	  '*' -> next();
	         t1 := Tstar !t1
      	| '+' -> next();
	         t1 := Tplus !t1
      	| '?' -> next();
	         t1 := Toption !t1
      	| '{' -> next_noesc();
	         let n1 = ref None in
	         let n2 = ref None in

		 let j = ref 0 in
		 if !k < l & !c >= '0' & !c <= '9' then begin
		   while !k < l & !c >= '0' & !c <= '9' do
		     j := 10* !j + (Char.code !c - Char.code '0');
		     next_noesc()
		   done;
		   n1 := Some !j
                 end;
		 
		 if !k < l & !n1 <> None & !c = '}' then begin
		   next();
		   t1 := Tinterval (!t1, !j, !j)
		 end
		 else begin
		   
		   if !k >= l or !c <> ',' then
		     failwith "regexp: error in {...} phrase";

		   next_noesc();
		   j := 0;
		   
		   if !k < l & !c >= '0' & !c <= '9' then begin
		     while !k < l & !c >= '0' & !c <= '9' do
		       j := 10* !j + (Char.code !c - Char.code '0');
		       next_noesc()
		     done;
		     n2 := Some !j
                   end;
		   
		   if !k >= l || !c <> '}' then
		     failwith "regexp: error in {...} phrase";

		   next();
		   ( match !n1 with
		     None ->
		       ( match !n2 with
		           None ->
			     failwith "regexp: error in {...} phrase";
		         | Some m2 ->
			     t1 := Tinterval (!t1, 0, m2)
		       )
		   | Some m1 ->
		       ( match !n2 with
		           None ->
			     t1 := Tinterval (!t1, m1, -1)
		         | Some m2 ->
			     t1 := Tinterval (!t1, m1, m2)
		       )
                   )
		 end

	| _   -> continue := false
      end
      else continue := false
    done;
    !t1

  and scan_literal_or_group () =
    if !k >= l then
      Tnull
    else
    if !esc then begin
      match !c with
	'(' -> next();
	       let n = !group in
	       incr group;
	       incr n_open_groups;
               let t = scan_alternative() in
	       decr n_open_groups;
	       if !k < l & !esc & !c = ')' then begin
		 next();
		 closed_groups.(n) <- true;
		 Tgroup (n, t)
	       end
	       else
		 failwith "regexp: closing paranthesis \\) not found"
      |	('1'..'9') -> let n = (Char.code !c - Char.code '0') in 
	              if closed_groups.(n) then begin
	                 next(); 
			 Trefer n
		      end else 
			failwith "regexp: bad reference to group"
      |	'w' -> next(); Twordchar
      |	'W' -> next(); Tnowordchar
      |	'b' -> next(); Twordbound
      |	'B' -> next(); Tnowordbound
      |	'<' -> next(); Twordbeg
      |	'>' -> next(); Twordend
      |	'`' -> next(); Tbegbuf
      |	'\'' -> next(); Tendbuf
      |	'\\' -> next(); Texact (String.make 1 '\\')
      |	'|' -> Tnull
      |	')' -> if !n_open_groups > 0 then
	         Tnull
	       else
	         failwith "regexp: unmatched closing parenthesis"
      |	ch -> next(); Texact (String.make 1 ch)
    end 
    else begin
      match !c with
	'*' -> Tnull
      |	'+' -> Tnull
      |	'?' -> Tnull
      |	'{' -> Tnull
      |	'^' -> next(); Tbegline
      |	'$' -> next(); Tendline
      |	'.' -> next(); Tany

      |	'[' -> next_noesc();
	       if !k < l then begin
		 let negated = ref false in
		 let set = ref [] in

		 let add_char c =
		   set := Schar c :: !set
		 in

		 let add_range c1 c2 =
		   set := Srange(c1,c2) :: !set
		 in

		 if !c = '^' then begin
		   next_noesc();
		   negated := true
		 end;
		 
		 let continue = ref true in
		 let first = ref true in  (* the character after [ or [^ ? *)

		 while !continue & !k < l do
		   match () with
		     () when !c = '[' & !k + 1 < l & re_string.[!k + 1] = ':' ->
		       failwith "regexp: Character classes such as [[:digit:]] not implemented";

                     (* TODO: check for predefined sets *)

		   | () when !c = ']' & not !first ->
		       next();
		       continue := false

		   | () when (!k + 2 < l) & (re_string.[!k + 1] = '-') &
		             (re_string.[!k + 2] <> ']') ->

		     (* range *)

		       add_range !c (re_string.[!k + 2]);
		       next_noesc();
		       next_noesc();
		       next_noesc();
		       first := false;

		   | () ->
		       add_char !c;
		       next_noesc();
		       first := false;
		 done;
		 
		 if !continue then
		   failwith "regexp: closing bracket ] not found";

		 if !negated then
		   Tnegset !set
		 else
		   Tset !set
	       end
	       else
		 failwith "regexp: closing bracket ] not found"
		     
      |	ch  -> next(); Texact (String.make 1 ch )
    end

  in

  next();
  scan_alternative ()
;;


let pcre_safe_quote c =
  match c with
      'a'..'z'|'A'..'Z'|'0'..'9'|'_' -> String.make 1 c
    | _ -> "\\" ^ String.make 1 c
;;


let rec print_pcre_regexp ret =
  match ret with
      Texact s ->
	Pcre.quote s
    | Tany ->
	"."
    | Tnull ->
	"(?:)"
    | Tconcat l ->
	String.concat "" (List.map print_pcre_regexp l)
    | Tstar ret' ->
	print_pcre_subregexp ret' ^ "*"
    | Tplus ret' ->
	print_pcre_subregexp ret' ^ "+"
    | Toption ret' ->
	print_pcre_subregexp ret' ^ "?"
    | Tset s ->
	"[" ^ print_set s ^ "]"
    | Tnegset s ->
	"[^" ^ print_set s ^ "]"
    | Talt l ->
	String.concat "|" (List.map print_pcre_subregexp l)
    | Tgroup(_,ret') ->
	"(" ^ print_pcre_regexp ret' ^ ")"
    | Trefer n ->
	(* Put parentheses around \n to disambiguate from \nn *)
	"(?:\\" ^ string_of_int n ^ ")"
    | Tinterval(ret',m,n) ->
	print_pcre_subregexp ret' ^ "{" ^ string_of_int m ^ "," ^ 
	(if n >= 0 then string_of_int n else "") ^ "}"
    | Tbegline ->
	"^"
    | Tendline ->
	"(?:$)"
    | Twordchar ->
	"\\w"
    | Tnowordchar ->
	"\\W"
    | Twordbeg ->
	"\\b(?=\\w)"
    | Twordend ->
	"(?<=\\w)\\b"
    | Twordbound ->
	"\\b"
    | Tnowordbound ->
	"\\B"
    | Tbegbuf ->
	"\\A"
    | Tendbuf ->
	"\\z"

and print_pcre_subregexp ret =
  (* Print ret, but put parentheses around ret *)
  match ret with
      Tset _ 
    | Tnegset _ 
    | Tgroup(_,_) ->
	(* No additional parentheses needed *)
	print_pcre_regexp ret
    | _ ->
	(* Print (?:ret). This is the "neutral" form of grouping that only
	 * changes precedence
	 *)
	"(?:" ^ print_pcre_regexp ret ^ ")"

and print_set s =
  String.concat ""
    (List.map
       (function
	    Schar c -> pcre_safe_quote c
	  | Srange(c1,c2) -> pcre_safe_quote c1 ^ "-" ^ pcre_safe_quote c2
       )
       s
    )
;;

(**********************************************************************)
(* Emulation *)

let regexp s =
  let ret = scan_str_regexp s in
  let s' = print_pcre_regexp ret in
  (* DEBUG: prerr_endline s'; *)
  Pcre.regexp ~flags:[`MULTILINE] s'
;;

let regexp_case_fold s =
  let ret = scan_str_regexp s in
  let s' = print_pcre_regexp ret in
  (* DEBUG: prerr_endline s'; *)
  Pcre.regexp ~flags:[`MULTILINE; `CASELESS] s'
;;

let quote s =
  Pcre.quote s
;;

let regexp_string s =
  Pcre.regexp ~flags:[`MULTILINE] (Pcre.quote s)
;;

let regexp_string_case_fold s =
  Pcre.regexp ~flags:[`MULTILINE; `CASELESS] (Pcre.quote s)
;;

let string_match = Netstring_pcre.string_match ;;

(* let string_partial_match = Netstring_pcre.string_partial_match ;; *)
(* N/A *)

let search_forward = Netstring_pcre.search_forward ;;
let search_backward = Netstring_pcre.search_backward ;;

let matched_string = Netstring_pcre.matched_string ;;
let match_beginning = Netstring_pcre.match_beginning ;;
let match_end = Netstring_pcre.match_end ;;
let matched_group = Netstring_pcre.matched_group ;;
let group_beginning = Netstring_pcre.group_beginning ;;
let group_end = Netstring_pcre.group_end ;;

let templ_re = Pcre.regexp "(?:\\\\\\d)|[\\$\\\\]" ;;
  (* matches a backslash and a digit, or a single dollar or a single
   * backslash.
   *)

let tr_templ s =
  (* Convert \n to $n etc. *)
  (* Unfortunately we cannot just replace \ by $. *)
  let rec tr l =
    match l with
	Pcre.Delim "$" :: l' -> "$$" :: tr l'
      | Pcre.Delim "\\" :: Pcre.Delim "$" :: l'  -> "$$" :: tr l'
      | Pcre.Delim "\\" :: Pcre.Delim s :: l' -> s :: tr l'
      | Pcre.Delim "\\" :: Pcre.Text s :: l' -> s :: tr l'
      | [ Pcre.Delim "\\" ] -> failwith "trailing backslash"
      | Pcre.Delim d :: l' ->
	  assert(d.[0] = '\\');
	  let n = Char.code d.[1] - Char.code '0' in
	  if n = 0 then
	    "$&" :: tr l'
	  else
	    ("$" ^ string_of_int n ^ "$!") :: tr l'
      | Pcre.Text t :: l' -> t :: tr l'
      | Pcre.Group(_,_) :: _ -> assert false
      | Pcre.NoGroup :: _ -> assert false
      | [] -> []
  in
  let l = Pcre.full_split ~rex:templ_re ~max:(-1) s in
  String.concat "" (tr l)
;;


let global_replace pat templ s = 
  Netstring_pcre.global_replace pat (tr_templ templ) s;;
let replace_first pat templ s = 
  Netstring_pcre.replace_first pat (tr_templ templ) s ;;

let global_substitute = Netstring_pcre.global_substitute ;;
let substitute_first = Netstring_pcre.substitute_first ;;

(* replace_matched: n/a *)

let split = Netstring_pcre.split ;;
let bounded_split = Netstring_pcre.bounded_split ;;
let split_delim = Netstring_pcre.split_delim ;;
let bounded_split_delim = Netstring_pcre.bounded_split_delim ;;

let tr_split_result r =
  List.map
    (function 
	 Pcre.Text t   -> Text t
       | Pcre.Delim d  -> Delim d
       | _ -> assert false
    )
    (List.filter
       (function 
	    Pcre.Group(_,_)
	  | Pcre.NoGroup    -> false
	  | _               -> true
       )
       r
    )
;;


let full_split sep s = 
  tr_split_result (Netstring_pcre.full_split sep s);;
let bounded_full_split sep s max = 
  tr_split_result (Netstring_pcre.bounded_full_split sep s max);;

let string_before = Netstring_pcre.string_before ;;
let string_after = Netstring_pcre.string_after ;;
let first_chars = Netstring_pcre.first_chars ;;
let last_chars = Netstring_pcre.last_chars ;;
