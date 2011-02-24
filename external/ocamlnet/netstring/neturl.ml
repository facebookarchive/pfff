(* $Id: neturl.ml 1062 2006-12-17 20:17:36Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

open Printf

exception Malformed_URL

type url_syntax_option =
    Url_part_not_recognized
  | Url_part_allowed
  | Url_part_required


type url_syntax =
    { url_enable_scheme    : url_syntax_option;
      url_enable_user      : url_syntax_option;
      url_enable_user_param : url_syntax_option;
      url_enable_password  : url_syntax_option;
      url_enable_host      : url_syntax_option;
      url_enable_port      : url_syntax_option;
      url_enable_path      : url_syntax_option;
      url_enable_param     : url_syntax_option;
      url_enable_query     : url_syntax_option;
      url_enable_fragment  : url_syntax_option;
      url_enable_other     : url_syntax_option;
      url_accepts_8bits    : bool;
      url_is_valid         : url -> bool;
      url_enable_relative  : bool;
    }

and url =
    { 
      url_syntax   : url_syntax;
      mutable url_validity : bool;
      url_scheme   : string option;
      url_user     : string option;
      url_user_param : string list;
      url_password : string option;
      url_host     : string option;
      url_port     : int option;
      url_path     : string list;
      url_param    : string list;
      url_query    : string option;
      url_fragment : string option;
      url_other    : string option;
    }
;;


type char_category =
    Accepted
  | Rejected
  | Separator



let scan_url_part s k_from k_to cats accept_8bits =
  (* Scans the longest word of accepted characters from position 'k_from'
   * in 's' until at most position 'k_to'. The character following the
   * word (if any) must be a separator character.
   * On success, the function returns the position of the last character
   * of the word + 1.
   * If there is any rejected character before the separator or the end
   * of the string (i.e. position 'k_to') is reached, the exception
   * Malformed_URL is raised.
   * Furthermore, if the character '%' is accepted it is checked whether
   * two hexadecimal digits follow (which must be accepted, too). If this
   * is not true, the exception Malformed_URL is raised, too.
   * 'cats': contains for every character code (0 to 255) the category
   * of the character.
   *)
  let check_hex c =
    if cats.( Char.code c ) <> Accepted then raise Malformed_URL;
    match c with
	('0'..'9'|'A'..'F'|'a'..'f') -> ()
      | _ -> raise Malformed_URL
  in

  let rec scan k =
    if k >= k_to then
      k
    else begin
      let c = s.[k] in
      let cat = cats.(Char.code c) in
      match cat with
	  Accepted -> 
	    if c = '%' then begin
	      if k+2 >= k_to then raise Malformed_URL;
	      let c1 = s.[k+1] in
	      let c2 = s.[k+2] in
	      check_hex c1;
	      check_hex c2;
	      scan (k+3)
	    end
	    else
	      scan (k+1)
	| Separator -> k
	| Rejected -> 
	    if accept_8bits && c >= '\128' 
	    then scan (k+1)
	    else raise Malformed_URL
    end
  in

  assert (Array.length cats = 256);
  assert (k_from >= 0);
  assert (k_from <= k_to);
  assert (k_to <= String.length s);
  
  scan k_from
;;

  
(* Create a categorization: *)

let lalpha = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
	       'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z' ]

let ualpha = [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M';
	       'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z' ]

let digit = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]

let safe = [ '$'; '-'; '_'; '.'; '+' ]

let extra = [ '!'; '*'; '\''; '('; ')'; ',' ]

let make_cats accepted separators =
  (* create a categorization:
   * - All characters listed in 'separators' are separators.
   * - All characters listed in 'accepted' and which do not occur in
   *   'separators' are accepted characters.
   * - All other characters are rejected.
   *)
  let cats = Array.make 256 Rejected in
  List.iter
    (fun c ->
       cats.(Char.code c) <- Accepted
    )
    accepted;

  List.iter
    (fun c ->
       cats.(Char.code c) <- Separator
    )
    separators;
  cats
;;


let scheme_cats =
  make_cats (lalpha @ ualpha @ ['+'; '-'; '.']) [':'] ;;

    (* scheme_cats: character categorization to _extract_ the URL scheme *)


let login_cats =
  make_cats 
    (lalpha @ ualpha @ digit @ safe @ extra @ [';'; '&'; '='; '%'])  
    [':'; '@'; '/'; '#'; '?' ]
;;

    (* login_cats: character categorization to _extract_ user name, password,
     * host name, and port.
     *
     * Note: user_params are extracted in a second step.
     *
     * Note: '?' is now a separator, as described in RFC 2396.
     *)

let host_cats =
  make_cats
    (lalpha @ ualpha @ digit @ ['.'; '-'])
    []
;;

    (* host_cats: character categorization to _check_ whether the host name
     * is formed only by legal characters.
     * Especially '%' is not allowed here!
     *
     * CHECK: IPv6
     *)

let port_cats =
  make_cats
    digit
    []
;;

    (* port_cats: character categorization to _check_ whether the port number
     * is formed only by legal characters.
     * Especially '%' is not allowed here!
     *)

let path_cats separators =
  make_cats
    (lalpha @ ualpha @ digit @ safe @ extra @ 
              ['?'; ':'; '@'; '&'; '='; ';'; '%'; '/'; '~'])
    separators
;;


let separators_from_syntax syn =
  let include_if syn_option clist =
    if syn_option <> Url_part_not_recognized then
      clist
    else
      []
  in
  (include_if syn.url_enable_param [';']) @
  (include_if syn.url_enable_query ['?']) @
  (include_if syn.url_enable_fragment ['#'])
;;


let path_cats_from_syntax syn extraseps =
  let separators = separators_from_syntax syn in
  path_cats (separators @ extraseps)
;;

(* path_cats_from_syntax:
 * Computes a character categorization to extract the path from an URL.
 * This depends on the syntax because the list of possible separators
 * contains the characters that may begin the next URL clause.
 *
 * Notes:
 * - The '#' is rejected unless fragments are enabled. 
 * - The '~' is accepted although this violates RFC 1738 (but it is ok
 *   according to RFC 2396)
 *)


let other_cats_from_syntax syn =
  let include_if syn_option clist =
    if syn_option <> Url_part_not_recognized then
      clist
    else
      []
  in
  let separators =
    (include_if syn.url_enable_param [';']) @
    (include_if syn.url_enable_query ['?']) @
    (include_if syn.url_enable_fragment ['#'])
  in

  make_cats
    (lalpha @ ualpha @ digit @ safe @ extra @ 
              (separators @ ['?'; ':'; '@'; '&'; '='; ';'; '%'; '/']))
    []
;;

    (* other_cats: character categorization to extract or check the
     * "other" part of the URL.
     *)



let extract_url_scheme s = 
  let l = String.length s in
  let k = scan_url_part s 0 l scheme_cats false in
          (* or raise Malformed_URL *)
  if k = l then raise Malformed_URL;
  assert (s.[k] = ':');
  String.lowercase(String.sub s 0 k)
;;


let ( => ) a b = not a or b;;   (* implication *)

let ( <=> ) (a:bool) b = ( a = b );;  (* equivalence *)

let url_syntax_is_valid syn =
  let recognized x = x <> Url_part_not_recognized in
  let _not_recognized x = x = Url_part_not_recognized in
  (recognized syn.url_enable_password   => recognized syn.url_enable_user) &&
  (recognized syn.url_enable_user_param => recognized syn.url_enable_user) &&
  (recognized syn.url_enable_port       => recognized syn.url_enable_host) &&
  (recognized syn.url_enable_user       => recognized syn.url_enable_host) &&
  not ( (recognized syn.url_enable_user ||
	 recognized syn.url_enable_password ||
	 recognized syn.url_enable_host ||
	 recognized syn.url_enable_port ||
	 recognized syn.url_enable_path) &&
	(recognized syn.url_enable_other))
;;


let partial_url_syntax syn =
  let weaken =
    function
	Url_part_not_recognized -> Url_part_not_recognized
      | Url_part_allowed        -> Url_part_allowed
      | Url_part_required       -> Url_part_allowed
  in
  if not syn.url_enable_relative then
    failwith "Neturl.partial_url_syntax: This syntax does not support relative URLs";
  { url_enable_scheme    = weaken syn.url_enable_scheme;
    url_enable_user      = weaken syn.url_enable_user;
    url_enable_user_param= weaken syn.url_enable_user_param;
    url_enable_password  = weaken syn.url_enable_password;
    url_enable_host      = weaken syn.url_enable_host;
    url_enable_port      = weaken syn.url_enable_port;
    url_enable_path      = weaken syn.url_enable_path;
    url_enable_param     = weaken syn.url_enable_param;
    url_enable_query     = weaken syn.url_enable_query;
    url_enable_fragment  = weaken syn.url_enable_fragment;
    url_enable_other     = weaken syn.url_enable_other;
    url_accepts_8bits    = syn.url_accepts_8bits;
    url_is_valid         = syn.url_is_valid;
    url_enable_relative  = true;
  }
;;



let file_url_syntax =
  { url_enable_scheme    = Url_part_required;
    url_enable_user      = Url_part_not_recognized;
    url_enable_user_param= Url_part_not_recognized;
    url_enable_password  = Url_part_not_recognized;
    url_enable_host      = Url_part_allowed;
    url_enable_port      = Url_part_not_recognized;
    url_enable_path      = Url_part_required;
    url_enable_param     = Url_part_not_recognized;
    url_enable_query     = Url_part_not_recognized;
    url_enable_fragment  = Url_part_not_recognized;
    url_enable_other     = Url_part_not_recognized;
    url_accepts_8bits    = false;
    url_is_valid         = (fun _ -> true);
    url_enable_relative  = true;
  }
;;


let ftp_url_syntax =
  { url_enable_scheme    = Url_part_required;
    url_enable_user      = Url_part_allowed;
    url_enable_user_param= Url_part_not_recognized;
    url_enable_password  = Url_part_allowed;
    url_enable_host      = Url_part_required;
    url_enable_port      = Url_part_allowed;
    url_enable_path      = Url_part_allowed;
    url_enable_param     = Url_part_allowed;
    url_enable_query     = Url_part_not_recognized;
    url_enable_fragment  = Url_part_not_recognized;
    url_enable_other     = Url_part_not_recognized;
    url_accepts_8bits    = false;
    url_is_valid         = (fun _ -> true);
    url_enable_relative  = true;
  }
;;


let http_url_syntax =
  { url_enable_scheme    = Url_part_required;
    url_enable_user      = Url_part_allowed;
    url_enable_user_param= Url_part_not_recognized;
    url_enable_password  = Url_part_allowed;
    url_enable_host      = Url_part_required;
    url_enable_port      = Url_part_allowed;
    url_enable_path      = Url_part_allowed;
    url_enable_param     = Url_part_not_recognized;
    url_enable_query     = Url_part_allowed;
    url_enable_fragment  = Url_part_not_recognized;
    url_enable_other     = Url_part_not_recognized;
    url_accepts_8bits    = false;
    url_is_valid         = (fun _ -> true);
    url_enable_relative  = true;
  }
;;


let pop_url_syntax =
  { url_enable_scheme    = Url_part_required;
    url_enable_user      = Url_part_allowed;
    url_enable_user_param= Url_part_allowed;
    url_enable_password  = Url_part_allowed;
    url_enable_host      = Url_part_required;
    url_enable_port      = Url_part_allowed;
    url_enable_path      = Url_part_not_recognized;
    url_enable_param     = Url_part_not_recognized;
    url_enable_query     = Url_part_not_recognized;
    url_enable_fragment  = Url_part_not_recognized;
    url_enable_other     = Url_part_not_recognized;
    url_accepts_8bits    = false;
    url_is_valid         = (fun _ -> true);
    url_enable_relative  = false;
  }
;;


let imap_url_syntax =
  { url_enable_scheme    = Url_part_required;
    url_enable_user      = Url_part_allowed;
    url_enable_user_param= Url_part_allowed;
    url_enable_password  = Url_part_allowed;
    url_enable_host      = Url_part_required;
    url_enable_port      = Url_part_allowed;
    url_enable_path      = Url_part_allowed;
    url_enable_param     = Url_part_not_recognized;
    url_enable_query     = Url_part_allowed;
    url_enable_fragment  = Url_part_not_recognized;
    url_enable_other     = Url_part_not_recognized;
    url_accepts_8bits    = false;
    url_is_valid         = (fun _ -> true);
    url_enable_relative  = true;
  }
;;


let mailto_url_syntax =
  { url_enable_scheme    = Url_part_required;
    url_enable_user      = Url_part_not_recognized;
    url_enable_user_param= Url_part_not_recognized;
    url_enable_password  = Url_part_not_recognized;
    url_enable_host      = Url_part_not_recognized;
    url_enable_port      = Url_part_not_recognized;
    url_enable_path      = Url_part_not_recognized;
    url_enable_param     = Url_part_not_recognized;
    url_enable_query     = Url_part_allowed;
    url_enable_fragment  = Url_part_not_recognized;
    url_enable_other     = Url_part_required;
    url_accepts_8bits    = false;
    url_is_valid         = (fun _ -> true);
    url_enable_relative  = false;
  }
;;


let news_url_syntax =
  { url_enable_scheme    = Url_part_required;
    url_enable_user      = Url_part_not_recognized;
    url_enable_user_param= Url_part_not_recognized;
    url_enable_password  = Url_part_not_recognized;
    url_enable_host      = Url_part_not_recognized;
    url_enable_port      = Url_part_not_recognized;
    url_enable_path      = Url_part_not_recognized;
    url_enable_param     = Url_part_not_recognized;
    url_enable_query     = Url_part_not_recognized;
    url_enable_fragment  = Url_part_not_recognized;
    url_enable_other     = Url_part_required;
    url_accepts_8bits    = false;
    url_is_valid         = (fun _ -> true);
    url_enable_relative  = false;
  }
;;
let cid_url_syntax = news_url_syntax;;
let data_url_syntax = news_url_syntax;;

let nntp_url_syntax =
  { url_enable_scheme    = Url_part_required;
    url_enable_user      = Url_part_not_recognized;
    url_enable_user_param= Url_part_not_recognized;
    url_enable_password  = Url_part_not_recognized;
    url_enable_host      = Url_part_required;
    url_enable_port      = Url_part_allowed;
    url_enable_path      = Url_part_required;
    url_enable_param     = Url_part_not_recognized;
    url_enable_query     = Url_part_not_recognized;
    url_enable_fragment  = Url_part_not_recognized;
    url_enable_other     = Url_part_not_recognized;
    url_accepts_8bits    = false;
    url_is_valid         = (fun url -> 
			      List.length url.url_path = 3
			   );
    url_enable_relative  = true;
  }
;;


let ipp_url_syntax =
  { url_enable_scheme    = Url_part_required;
    url_enable_user      = Url_part_not_recognized;
    url_enable_user_param= Url_part_not_recognized;
    url_enable_password  = Url_part_not_recognized;
    url_enable_host      = Url_part_required;
    url_enable_port      = Url_part_allowed;
    url_enable_path      = Url_part_allowed;
    url_enable_param     = Url_part_not_recognized;
    url_enable_query     = Url_part_allowed;
    url_enable_fragment  = Url_part_not_recognized;
    url_enable_other     = Url_part_not_recognized;
    url_accepts_8bits    = false;
    url_is_valid         = (fun _ -> true);
    url_enable_relative  = true;
  }
;;


let null_url_syntax =
  { url_enable_scheme    = Url_part_not_recognized;
    url_enable_user      = Url_part_not_recognized;
    url_enable_user_param= Url_part_not_recognized;
    url_enable_password  = Url_part_not_recognized;
    url_enable_host      = Url_part_not_recognized;
    url_enable_port      = Url_part_not_recognized;
    url_enable_path      = Url_part_not_recognized;
    url_enable_param     = Url_part_not_recognized;
    url_enable_query     = Url_part_not_recognized;
    url_enable_fragment  = Url_part_not_recognized;
    url_enable_other     = Url_part_not_recognized;
    url_accepts_8bits    = false;
    url_is_valid         = (fun _ -> true);
    url_enable_relative  = false;
  }
;;


let ip_url_syntax =
  { url_enable_scheme    = Url_part_allowed;
    url_enable_user      = Url_part_allowed;
    url_enable_user_param= Url_part_not_recognized;
                           (* so user parameters are parsed as part
			    * of the user string! (Most generic.)
			    *)
    url_enable_password  = Url_part_allowed;
    url_enable_host      = Url_part_allowed;
    url_enable_port      = Url_part_allowed;
    url_enable_path      = Url_part_allowed;
    url_enable_param     = Url_part_allowed;
    url_enable_query     = Url_part_allowed;
    url_enable_fragment  = Url_part_allowed;
    url_enable_other     = Url_part_not_recognized;
    url_accepts_8bits    = false;
    url_is_valid         = (fun _ -> true);
    url_enable_relative  = true;
  }
;;


let common_url_syntax =
  let h = Hashtbl.create 10 in
  Hashtbl.add h "file"   file_url_syntax;
  Hashtbl.add h "ftp"    ftp_url_syntax;
  Hashtbl.add h "http"   http_url_syntax;
  Hashtbl.add h "https"  http_url_syntax;
  Hashtbl.add h "pop"    pop_url_syntax;
  Hashtbl.add h "pops"   pop_url_syntax;
  Hashtbl.add h "imap"   imap_url_syntax;
  Hashtbl.add h "imaps"  imap_url_syntax;
  Hashtbl.add h "mailto" mailto_url_syntax;
  Hashtbl.add h "news"   news_url_syntax;
  Hashtbl.add h "nntp"   nntp_url_syntax;
  Hashtbl.add h "nntps"  nntp_url_syntax;
  Hashtbl.add h "cid"    cid_url_syntax;
  Hashtbl.add h "mid"    cid_url_syntax;
  Hashtbl.add h "data"   data_url_syntax;
  Hashtbl.add h "ipp"    ipp_url_syntax;
  Hashtbl.add h "ipps"   ipp_url_syntax;
  h
;;


let url_conforms_to_syntax url =
  let recognized x = x <> Url_part_not_recognized in
  let required x = x = Url_part_required in
  let present x    = x <> None in
  let syn = url.url_syntax in
  (present url.url_scheme   => recognized syn.url_enable_scheme)   &&
  (present url.url_user     => recognized syn.url_enable_user)     &&
  ((url.url_user_param <> []) => recognized syn.url_enable_user_param) &&
  (present url.url_password => recognized syn.url_enable_password) &&
  (present url.url_host     => recognized syn.url_enable_host)     &&
  (present url.url_port     => recognized syn.url_enable_port)     &&
  ((url.url_path <> [])     => recognized syn.url_enable_path)     &&
  ((url.url_param <> [])    => recognized syn.url_enable_param)    &&
  (present url.url_query    => recognized syn.url_enable_query)    &&
  (present url.url_fragment => recognized syn.url_enable_fragment) &&
  (present url.url_other    => recognized syn.url_enable_other)    &&
  (required syn.url_enable_scheme   => present url.url_scheme)     &&
  (required syn.url_enable_user     => present url.url_user)       &&
  (required syn.url_enable_user_param => (url.url_user_param <> [])) &&
  (required syn.url_enable_password => present url.url_password)   &&
  (required syn.url_enable_host     => present url.url_host)       &&
  (required syn.url_enable_port     => present url.url_port)       &&
  (required syn.url_enable_path     => (url.url_path <> []))       &&
  (required syn.url_enable_param    => (url.url_param <> []))      &&
  (required syn.url_enable_query    => present url.url_query)      &&
  (required syn.url_enable_fragment => present url.url_fragment)   &&
  (required syn.url_enable_other    => present url.url_other)      &&
  (url.url_validity || syn.url_is_valid url)
;;


let url_syntax_of_url url = url.url_syntax
;;


let modify_url
      ?syntax
      ?(encoded = false)
      ?scheme
      ?user
      ?user_param
      ?password
      ?host
      ?port
      ?path
      ?param
      ?query
      ?fragment
      ?other
      url 
  =

  let enc ?(plus=false) x =
    if encoded then
      x
    else
      match x with
	  None -> None
	| Some x' -> Some (Netencoding.Url.encode ~plus x')
  in
  let enc_list ?(plus=false) l = 
    if encoded then
      l
    else
      List.map (Netencoding.Url.encode ~plus) l 
  in

  let new_syntax =
    match syntax with
	None -> url.url_syntax
      | Some syn -> syn
  in

  let check_string s_opt cats =
    match s_opt with
	None   -> ()
      | Some s ->
	  let l = String.length s in
	  let k = scan_url_part s 0 l cats new_syntax.url_accepts_8bits in
	          (* or raise Malformed_URL *)
	  if k <> l then raise Malformed_URL
  in

  let check_string_list p cats sep =
    List.iter
      (fun p_component ->
	 let l = String.length p_component in
	 let k = 
	   scan_url_part p_component 0 l cats new_syntax.url_accepts_8bits in
	   (* or raise Malformed_URL *)
	 if k <> l then raise Malformed_URL;
	 if String.contains p_component sep then raise Malformed_URL;
      )
      p
  in

  (* Create the modified record: *)
  let url' =
    { 
      url_syntax   = new_syntax;
      url_validity = false;
      url_scheme   = if scheme   = None then url.url_scheme   else scheme;
      url_user     = if user     = None then url.url_user     else enc user;
      url_user_param = ( match user_param with 
			     None -> url.url_user_param
			   | Some p -> enc_list p);
      url_password = if password = None then url.url_password else enc password;
      url_host     = if host     = None then url.url_host     else host;
      url_port     = if port     = None then url.url_port     else port;
      url_path     = (match path with
			  None -> url.url_path
			| Some p -> enc_list p);
      url_param    = (match param with
			  None -> url.url_param
			| Some p -> enc_list p);
      url_query    = if query    = None then url.url_query    else enc ~plus:true query;
      url_fragment = if fragment = None then url.url_fragment else enc fragment;
      url_other    = if other    = None then url.url_other    else enc other;
    }
  in
  (* Check whether the URL conforms to the syntax:
   *)
  if not (url_conforms_to_syntax url') then raise Malformed_URL;
  if url'.url_password <> None && url'.url_user = None then raise Malformed_URL;
  if url'.url_user_param <> [] && url'.url_user = None then raise Malformed_URL;
  if url'.url_user <> None && url'.url_host = None then raise Malformed_URL;
  if url'.url_port <> None && url'.url_host = None then raise Malformed_URL;
  (* Check every part: *)
  check_string url'.url_scheme          scheme_cats;
  check_string url'.url_user            login_cats;
  check_string_list url'.url_user_param login_cats ';';
  check_string url'.url_password        login_cats;
  check_string url'.url_host            host_cats;
  (match url'.url_port with 
       None -> ()
     | Some p -> if p < 0 || p > 65535 then raise Malformed_URL
  );
  let path_cats  = path_cats_from_syntax  new_syntax [] in
  let other_cats = other_cats_from_syntax new_syntax in
  check_string url'.url_query    path_cats;
  check_string url'.url_fragment path_cats;
  check_string url'.url_other    other_cats;
  (* Check the lists: *)
  check_string_list url'.url_param path_cats ';';
  check_string_list url'.url_path  path_cats '/';
  (* Further path checks: *)
  begin match url'.url_path with
      [] ->
	(* The path is empty: There must not be 'param' *)
	if url'.url_host <> None then begin
	  if url'.url_param <> [] then raise Malformed_URL;
	  (* if url'.url_query <> None then raise Malformed_URL; 
	   * Allowed since RFC 2396
	   *)
	end
    | ["";""] ->
	(* This is illegal. *)
	raise Malformed_URL;
    | "" :: p' ->
	(* The path is absolute: ensure there is no double slash with host *)
	( match url'.url_path with
	    | "" :: "" :: _ ->
		(* Double slash at beginning of path: Only allowed we have
                 * a host name!
                 *)
		if host = None then raise Malformed_URL
	    | _ -> ()
	)
    | _ ->
	(* The path is relative: there must not be a host *)
	if url'.url_host <> None then raise Malformed_URL;
  end;
  (* Cache that the URL is valid: *)
  url'.url_validity <- true;

  url'
;;


let null_url =
  { 
    url_syntax   = null_url_syntax;
    url_validity = true;
    url_scheme   = None;
    url_user     = None;
    url_user_param = [];
    url_password = None;
    url_host     = None;
    url_port     = None;
    url_path     = [];
    url_param    = [];
    url_query    = None;
    url_fragment = None;
    url_other    = None;
  }
;;


let make_url
      ?(encoded = false)
      ?scheme
      ?user
      ?user_param
      ?password
      ?host
      ?port
      ?path
      ?param
      ?query
      ?fragment
      ?other
      syntax
  =

  if not (url_syntax_is_valid syntax) then
    invalid_arg "Neturl.make_url";

  modify_url
    ~encoded
    ~syntax
    ?scheme
    ?user
    ?user_param
    ?password
    ?host
    ?port
    ?path
    ?param
    ?query
    ?fragment
    ?other
    null_url
;;


let remove_from_url
      ?(scheme = false)
      ?(user = false)
      ?(user_param = false)
      ?(password = false)
      ?(host = false)
      ?(port = false)
      ?(path = false)
      ?(param = false)
      ?(query = false)
      ?(fragment = false)
      ?(other = false)
      url
  =

  make_url
    ~encoded:   true
    ?scheme:    (if scheme     then None else url.url_scheme)
    ?user:      (if user       then None else url.url_user)
    ?user_param:(if user_param then None else Some url.url_user_param)
    ?password:  (if password   then None else url.url_password)
    ?host:      (if host       then None else url.url_host)
    ?port:      (if port       then None else url.url_port)
    ?path:      (if path       then None else Some url.url_path)
    ?param:     (if param      then None else Some url.url_param)
    ?query:     (if query      then None else url.url_query)
    ?fragment:  (if fragment   then None else url.url_fragment)
    ?other:     (if other      then None else url.url_other)
    url.url_syntax
;;


let default_url
      ?(encoded = false)
      ?scheme
      ?user
      ?(user_param = [])
      ?password
      ?host
      ?port
      ?(path = [])
      ?(param = [])
      ?query
      ?fragment
      ?other
      url
  =

  let encode = Netencoding.Url.encode ~plus:true in

  let enc x =
    if encoded then
      x
    else
      match x with
	  None -> None
	| Some x' -> Some (encode x')
  in

  let enc_list l = 
    if encoded then
      l
    else
      List.map encode l 
  in

  let pass_if_missing current arg =
    match current with
	None -> arg
      | _    -> current
  in

  make_url
    ~encoded:   true
    ?scheme:    (pass_if_missing url.url_scheme   scheme)
    ?user:      (pass_if_missing url.url_user     (enc user))
    ~user_param:(if url.url_user_param = [] then enc_list user_param else url.url_user_param)
    ?password:  (pass_if_missing url.url_password (enc password))
    ?host:      (pass_if_missing url.url_host     host)
    ?port:      (pass_if_missing url.url_port     port)
    ~path:      (if url.url_path  = [] then enc_list path  else url.url_path)
    ~param:     (if url.url_param = [] then enc_list param else url.url_param)
    ?query:     (pass_if_missing url.url_query    (enc query))
    ?fragment:  (pass_if_missing url.url_fragment (enc fragment))
    ?other:     (pass_if_missing url.url_other    (enc other))
    url.url_syntax
;;


let undefault_url
      ?scheme
      ?user
      ?user_param
      ?password
      ?host
      ?port
      ?path
      ?param
      ?query
      ?fragment
      ?other
      url
  =

  let remove_if_matching current arg =
    match current with
	None -> None
      | Some x -> 
	  (match arg with
	       None -> current
	     | Some x' ->
		 if x=x' then
		   None
		 else
		   current)
  in

  let remove_list_if_matching current arg =
    match arg with
	None -> current
      | Some x ->
	  if x = current then [] else current
  in

  make_url
    ~encoded:   true
    ?scheme:    (remove_if_matching url.url_scheme   scheme)
    ?user:      (remove_if_matching url.url_user     user)
    ~user_param:(remove_list_if_matching url.url_user_param user_param)
    ?password:  (remove_if_matching url.url_password password)
    ?host:      (remove_if_matching url.url_host     host)
    ?port:      (remove_if_matching url.url_port     port)
    ~path:      (remove_list_if_matching url.url_path path)
    ~param:     (remove_list_if_matching url.url_param param)
    ?query:     (remove_if_matching url.url_query    query)
    ?fragment:  (remove_if_matching url.url_fragment fragment)
    ?other:     (remove_if_matching url.url_other    other)
    url.url_syntax
;;


let url_provides 
      ?(scheme = false)
      ?(user = false)
      ?(user_param = false)
      ?(password = false)
      ?(host = false)
      ?(port = false)
      ?(path = false)
      ?(param = false)
      ?(query = false)
      ?(fragment = false)
      ?(other = false)
      url
  =
  
  (scheme     => (url.url_scheme   <> None)) &&
  (user       => (url.url_user     <> None)) &&
  (user_param => (url.url_param    <> [])) &&
  (password   => (url.url_password <> None)) &&
  (host       => (url.url_host     <> None)) &&
  (port       => (url.url_port     <> None)) &&
  (path       => (url.url_path     <> [])) &&
  (param      => (url.url_param    <> [])) &&
  (query      => (url.url_query    <> None)) &&
  (fragment   => (url.url_fragment <> None)) &&
  (other      => (url.url_other    <> None))
;;
  

let return_if value =
  match value with
      None -> raise Not_found
    | Some x -> x
;;


let decode_if ?(plus=false) want_encoded value =
  let value' = return_if value in
  if want_encoded then
    value'
  else
    Netencoding.Url.decode ~plus value'
;;


let decode_path_if ?(plus=false) want_encoded value =
  if want_encoded then
    value
  else
    List.map (Netencoding.Url.decode ~plus) value 
;;


let url_scheme                      url = return_if url.url_scheme;;
let url_user       ?(encoded=false) url = decode_if encoded url.url_user;;
let url_user_param ?(encoded=false) url = decode_path_if encoded url.url_user_param;;
let url_password   ?(encoded=false) url = decode_if encoded url.url_password;;
let url_host                        url = return_if url.url_host;;
let url_port                        url = return_if url.url_port;;
let url_path       ?(encoded=false) url = decode_path_if encoded url.url_path;;
let url_param      ?(encoded=false) url = decode_path_if encoded url.url_param;;
let url_query      ?(encoded=false) url = decode_if ~plus:true encoded url.url_query;;
let url_fragment   ?(encoded=false) url = decode_if encoded url.url_fragment;;
let url_other      ?(encoded=false) url = decode_if encoded url.url_other;;


let string_of_url url =
  if not (url.url_validity) then
    failwith "Neturl.string_of_url: URL not flagged as valid";
  (match url.url_scheme with
       None -> ""
     | Some s -> s ^ ":") ^ 
  (match url.url_host with
       None -> ""
     | Some host ->
	 "//" ^ 
	 (match url.url_user with
	      None -> "" 
	    | Some user -> 
		user ^ 
		(String.concat ""
		   (List.map
		      (fun p -> ";" ^ p)
		      url.url_user_param)) ^
		(match url.url_password with
		     None -> ""
		   | Some password ->
		       ":" ^ password 
		) ^ 
		"@") ^ 
	 host ^ 
	 (match url.url_port with
	      None -> ""
	    | Some port ->
		":" ^ string_of_int port)) ^ 
  (match url.url_path with
     | [""] ->
	 "/"
     | x :: p  when  url.url_scheme = None &&
                     url.url_host = None &&
	             String.contains x ':' 
	->
	  (* Really a special case: The colon contained in 'x' may cause
	   * that a prefix of 'x' is interpreted as URL scheme. In this
	   * case, "./" is prepended (as recommended in RFC 1808, 5.3).
	   *)
	  "./"
     | _ ->
	 ""
  ) ^
  String.concat "/" url.url_path ^ 
  (match url.url_other with
       None -> ""
     | Some other ->
	 other) ^ 
  String.concat ""  (List.map (fun s -> ";" ^ s) url.url_param) ^ 
  (match url.url_query with
       None -> ""
     | Some query ->
	 "?" ^ query) ^ 
  (match url.url_fragment with
       None -> ""
     | Some fragment ->
	 "#" ^ fragment)
;;


let semi_re = Netstring_str.regexp ";";;


let url_of_string url_syntax s =
  let l = String.length s in
  let recognized x = x <> Url_part_not_recognized in

  let rec collect_words terminators eof_char cats k =
    (* Collect words as recognized by 'cats', starting at position 'k' in
     * 's'. Collection stops if one the characters listed in 'terminators'
     * is found. If the end of the string is reached, it is treated as
     * 'eof_char'.
     *)
    let k' = scan_url_part s k l cats url_syntax.url_accepts_8bits in  
             (* or raise Malformed_URL *)
    let word, sep =
      String.sub s k (k'-k), (if k'<l then s.[k'] else eof_char) in
    if List.mem sep terminators then
      [word, sep], k'
    else
      let word_sep_list', k'' = 
	collect_words terminators eof_char cats (k'+1) in
      ((word, sep) :: word_sep_list'), k''
  in

  (* Try to extract the scheme name: *)
  let scheme, k1 =
    if recognized url_syntax.url_enable_scheme then
      try
	let k = scan_url_part s 0 l scheme_cats false in
        (* or raise Malformed_URL *)
	if k = l then raise Malformed_URL;
	assert (s.[k] = ':');
	Some (String.sub s 0 k), (k+1)
      with
	  Malformed_URL -> None, 0
    else
      None, 0
  in

  (* If there is a "//", a host will follow: *)
  let host, port, userinfo, password, k2 =
    if recognized url_syntax.url_enable_host  &&
       k1 + 2 <= l  &&  s.[k1]='/'  && s.[k1+1]='/' then begin

      let word_sep_list, k' = collect_words [ '/'; '?'; '#' ] '/' login_cats (k1+2) 
      in
          (* or raise Malformed_URL *)

      let int x =
	try int_of_string x with _ -> raise Malformed_URL in

      match word_sep_list with
	  [ host, ('/'|'?'|'#') ] ->
	    Some host, None, None, None, k'
	| [ host, ':'; port, ('/'|'?'|'#') ] ->
	    Some host, Some (int port), None, None, k'
	| [ user, '@'; host, ('/'|'?'|'#') ] ->
	    Some host, None, Some user, None, k'
	| [ user, '@'; host, ':'; port, ('/'|'?'|'#') ] ->
	    Some host, Some (int port), Some user, None, k'
	| [ user, ':'; password, '@'; host, ('/'|'?'|'#') ] ->
	    Some host, None, Some user, Some password, k'
	| [ user, ':'; password, '@'; host, ':'; port, ('/'|'?'|'#') ] ->
	    Some host, Some (int port), Some user, Some password, k'
	| _ ->
	    raise Malformed_URL
    end
    else
      None, None, None, None, k1
  in

  (* Separate user from user_param: *)
  let user, user_param =
    match userinfo with
	None -> (None, [])
      | Some u ->
	  if recognized url_syntax.url_enable_user_param then (
	    let l = Netstring_str.split_delim semi_re u in
	    match l with
		[] -> (Some "", [])
	      | user :: user_param -> (Some user, user_param)
	  )
	  else (Some u, [])
  in

  let path, k3 =
    if recognized url_syntax.url_enable_path  &&
       k2 < l  (*  &&  s.[k2]='/'  *)
    then begin
      let cats = path_cats_from_syntax url_syntax [ '/' ] in
      let seps = separators_from_syntax url_syntax in

      (* Note: '>' is not allowed within URLs; because of this we can use
       * it as end-of-string character.
       *)

      let word_sep_list, k' = collect_words ('>'::seps) '>' cats k2 in
          (* or raise Malformed_URL *)
      match word_sep_list with
	  [ "", '/'; "", _ ] ->
	    [ "" ], k'
	| [ "", _ ] ->
	    [], k'
	| _ ->
	    List.map fst word_sep_list, k'
    end
    else begin
      (* If there is a single '/': skip it *)
      if not (recognized url_syntax.url_enable_other) &&
	 k2 < l  &&  s.[k2]='/'
      then
	[], (k2+1)
      else
	[], k2
    end
  in

  let other, k4 =
    if recognized url_syntax.url_enable_other  &&
       k3 < l 
    then begin
      
      let cats = other_cats_from_syntax url_syntax in

      (* Note: '>' is not allowed within URLs; because of this we can use
       * it as end-of-string character.
       *)

      let word_sep_list, k' = collect_words ['>';'#'] '>' cats k3 in
          (* or raise Malformed_URL *)

      match word_sep_list with
	  [ other, _ ] -> Some other, k'
	| _ -> assert false
    end
    else
      None, k3
  in

  let param, k5 =
    if recognized url_syntax.url_enable_param  &&
       k4 < l  &&  s.[k4]=';' 
    then begin
      let cats  = path_cats_from_syntax url_syntax [] in
      let seps  = separators_from_syntax url_syntax in
      let seps' = List.filter (fun c -> c <> ';') seps in

      (* Note: '>' is not allowed within URLs; because of this we can use
       * it as end-of-string character.
       *)

      let word_sep_list, k' = collect_words ('>'::seps') '>' cats (k4+1) in
          (* or raise Malformed_URL *)
      
      List.map fst word_sep_list, k'
    end
    else
      [], k4
  in

  let query, k6 =
    if recognized url_syntax.url_enable_query  &&
       k5 < l  &&  s.[k5]='?'
    then begin
      let cats  = path_cats_from_syntax url_syntax [] in
      let seps  = separators_from_syntax url_syntax in
      
      (* Note: '>' is not allowed within URLs; because of this we can use
       * it as end-of-string character.
       *)

      let word_sep_list, k' = collect_words ('>'::seps) '>' cats (k5+1) in
          (* or raise Malformed_URL *)

      match word_sep_list with
	  [ query, _ ] -> Some query, k'
	| _ -> assert false
    end
    else
      None, k5
  in

  let fragment, k7 =
    if recognized url_syntax.url_enable_fragment  &&
       k6 < l  &&  s.[k6]='#'
    then begin
      let cats  = path_cats_from_syntax url_syntax [] in
      let seps  = separators_from_syntax url_syntax in
      
      (* Note: '>' is not allowed within URLs; because of this we can use
       * it as end-of-string character.
       *)

      let word_sep_list, k' = collect_words ('>'::seps) '>' cats (k6+1) in
          (* or raise Malformed_URL *)

      match word_sep_list with
	  [ fragment, _ ] -> Some fragment, k'
	| _ -> assert false
    end
    else
      None, k6
  in

  if k7 <> l then raise Malformed_URL;

  make_url
    ~encoded:true
    ?scheme
    ?user
    ~user_param
    ?password
    ?host
    ?port
    ~path
    ~param
    ?query
    ?fragment
    ?other
    url_syntax
;;



let problem_re = Pcre.regexp "[ <>\"{}|\\\\^\\[\\]`]"

let fixup_url_string =
  Netstring_pcre.global_substitute 
    problem_re
    (fun m s ->
       sprintf "%%%02x" (Char.code s.[Netstring_pcre.match_beginning m]))
;;

  



let parse_url ?(schemes = common_url_syntax)
              ?base_syntax
	      ?(accept_8bits = false)
	      ?(enable_fragment = false)
	      s =
  let scheme =
    try
      Some (extract_url_scheme s)
    with
	Malformed_URL -> None in
  let syntax =
    match scheme with
	None ->
	  ( match base_syntax with
		None -> 
		  raise Malformed_URL
	      | Some syn -> 
		  partial_url_syntax syn
	  )
      | Some sch ->
	  try
	    Hashtbl.find schemes sch
	  with
	      Not_found -> raise Malformed_URL
  in
  let syntax' =
    if accept_8bits then
      { syntax with url_accepts_8bits = true } 
    else
      syntax in
  let syntax'' =
    if enable_fragment && 
       syntax.url_enable_fragment = Url_part_not_recognized then
      { syntax' with url_enable_fragment = Url_part_allowed } 
    else
      syntax' in
  url_of_string syntax'' s
;;


let split_path s =
  let l = String.length s in
  let rec collect_words k =
    let k' = 
      try
	String.index_from s k '/'
      with
	  Not_found -> l
    in
    let word = String.sub s k (k'-k) in
    if k' >= l then
      [word]
    else
      word :: collect_words (k'+1)
  in
  match collect_words 0 with
      [ "" ] -> []
    | [ "";"" ] -> [ "" ]
    | other -> other
;;


let join_path l = 
  match l with
      [ "" ] -> "/"
    | _      -> String.concat "/" l;;


let norm_path l = 

  let rec remove_slash_slash l first =
    match l with
      | [ "" ] ->
	  [ "" ]
      | [ ""; "" ] when first ->
	  [ "" ]
      | "" :: l' when not first ->
	  remove_slash_slash l' false
      | x :: l' ->
	  x :: remove_slash_slash l' false
      | [] ->
	  []
  in

  let rec remove_dot l first =
    match l with
      | ([ "." ] | ["."; ""]) ->
	  if first then [] else [ "" ]
      |	"." :: x :: l' ->
	  remove_dot (x :: l') false
      | x :: l' ->
	  x :: remove_dot l' false
      | [] ->
	  []
  in

  let rec remove_dot_dot_once l first =
    match l with
	x :: ".." :: [] when x <> "" && x <> ".." && not first ->
	  [ "" ]
      |	x :: ".." :: l' when x <> "" && x <> ".." ->
	  l'
      | x :: l' ->
	  x :: remove_dot_dot_once l' false
      | [] ->
	  raise Not_found
  in

  let rec remove_dot_dot l =
    try
      let l' = remove_dot_dot_once l true in
      remove_dot_dot l'
    with
	Not_found -> l
  in

  let l' = remove_dot_dot (remove_dot (remove_slash_slash l true) true) in
  match l' with
      [".."] -> [".."; ""]
    | ["";""] -> [ "" ]
    | _      -> l'
;;


let apply_relative_url baseurl relurl =
  if not (baseurl.url_validity) or not (relurl.url_validity) then
    failwith "Neturl.apply_relative_url: URL not flagged as valid";

  if relurl.url_scheme <> None then
    modify_url 
      ~syntax:baseurl.url_syntax           (* inherit syntax *)
      relurl
  else
    if relurl.url_host <> None then
      modify_url 
	~syntax:baseurl.url_syntax         (* inherit syntax and scheme *)
	?scheme:baseurl.url_scheme
	relurl
    else
      match relurl.url_path with
	  "" :: other ->
	    (* An absolute path *)
	    modify_url 
	      ~syntax:baseurl.url_syntax   (* inherit syntax, scheme, and *)
	      ~encoded:true
	      ?scheme:baseurl.url_scheme   (* login info *)
	      ?host:baseurl.url_host
	      ?port:baseurl.url_port
	      ?user:baseurl.url_user
	      ~user_param:baseurl.url_user_param
	      ?password:baseurl.url_password
	      relurl
	| [] ->
	    (* Empty: Inherit also path, params, query, and fragment *)
	    let new_params, new_query, new_fragment =
	      match relurl.url_param, relurl.url_query, relurl.url_fragment
	      with
		  [], None, None ->
		    (* Inherit all three *)
		    baseurl.url_param, baseurl.url_query, baseurl.url_fragment
		| [], None, f ->
		    (* Inherit params and query *)
		    baseurl.url_param, baseurl.url_query, f
		| [], q, f ->
		    (* Inherit params *)
		    baseurl.url_param, q, f
		| p, q, f ->
		    (* Inherit none of them *)
		    p, q, f
	    in
	    modify_url 
	      ~syntax:baseurl.url_syntax
	      ~encoded:true
	      ?scheme:baseurl.url_scheme
	      ?host:baseurl.url_host
	      ?port:baseurl.url_port
	      ?user:baseurl.url_user
	      ~user_param:baseurl.url_user_param
	      ?password:baseurl.url_password
	      ~path:baseurl.url_path
	      ~param:new_params
	      ?query:new_query
	      ?fragment:new_fragment
	      relurl
	| relpath ->
	    (* A relative path *)
	    let rec change_path basepath =
	      match basepath with
		| [] ->
		    if baseurl.url_host = None then
		      relpath
		    else
		      "" :: relpath
		| [ "" ] ->
		    "" :: relpath
		| [ x ] ->
		    relpath
		| x :: basepath' ->
		    x :: change_path basepath'
	    in
	    let new_path = norm_path (change_path baseurl.url_path) in
	    modify_url 
	      ~syntax:baseurl.url_syntax   (* inherit syntax, scheme, and *)
	      ~encoded:true
	      ?scheme:baseurl.url_scheme   (* login info *)
	      ?host:baseurl.url_host
	      ?port:baseurl.url_port
	      ?user:baseurl.url_user
	      ~user_param:baseurl.url_user_param
	      ?password:baseurl.url_password
	      ~path:new_path               (* and change path *)
	      relurl

;;


let ensure_absolute_url ?base u =
  if u.url_scheme = None then (
    match base with
	None -> raise Malformed_URL
      | Some b -> apply_relative_url b u
  )
  else
    u
;;
  

let print_url url =
  Format.print_string ("<URL:" ^ string_of_url url ^ ">")
;;


let backslash_re = Netstring_pcre.regexp "\\\\";;
let drive_letter_re = Netstring_pcre.regexp "^([A-Za-z]):/";;
let drive_letter_re' = Netstring_pcre.regexp "^/([A-Za-z]):/";;
let unc_path_re = Netstring_pcre.regexp "^//([^/]+)(/|$)";;


let os_type = Sys.os_type;;

let classify_path p =
  match os_type with
      "Unix" ->
	if p <> "" && p.[0] = '/' then
	  `Absolute_local p
	else
	  `Relative p
    | "Win32" ->
	let p' = Netstring_pcre.global_replace backslash_re "/" p in
	( match Netstring_pcre.string_match drive_letter_re p' 0 with
	      Some m ->
		`Absolute_local ("/" ^ p')
	    | None ->
		( match Netstring_pcre.string_match unc_path_re p' 0 with
		      Some m ->
			let host = Netstring_pcre.matched_group m 1 p' in
			let host_e = Netstring_pcre.group_end m 1 in
			let path = String.sub p' host_e (String.length p' - host_e) in
			let path = if path = "" then "/" else path in
			`Absolute_remote(host,path)
		    | None ->
			if p' <> "" && p'.[0] = '/' then
			  `Relative_drive p'
			else
			  `Relative p'
		)
	)
    | "Cygwin" ->
	let p' = Netstring_pcre.global_replace backslash_re "/" p in
	( match Netstring_pcre.string_match drive_letter_re p' 0 with
	      Some m ->
		let letter = Netstring_pcre.matched_group m 1 p' in
		let rest = String.sub p' 2 (String.length p' - 2) in
		`Absolute_local("/cygdrive/" ^ letter ^ rest)
	    | None ->
		( match Netstring_pcre.string_match unc_path_re p' 0 with
		      Some m ->
			let host = Netstring_pcre.matched_group m 1 p' in
			let host_e = Netstring_pcre.group_end m 1 in
			let path = String.sub p' host_e (String.length p' - host_e) in
			let path = if path = "" then "/" else path in
			`Absolute_remote(host,path)
		    | None ->
			if p' <> "" && p'.[0] = '/' then
			  `Absolute_local p'
			else
			  `Relative p'
		)
	)
    | _ ->
	assert false
;;

let file_url_of_local_path ?(getcwd = Sys.getcwd) p =
  (* Classify p, and make it absolute: *)
  let p_class = classify_path p in
  let p_abs_class =
    match p_class with
	`Relative r ->
	  ( match classify_path (getcwd()) with
		`Absolute_local l ->
		  if l = "/" then 
		    `Absolute_local("/" ^ r)
		  else
		    `Absolute_local(l ^ "/" ^ r)
	      | `Absolute_remote(h,l) ->
		  if l = "/" then 
		    `Absolute_remote(h,"/" ^ r)
		  else
		    `Absolute_remote(h,l ^ "/" ^ r)
	      | _ ->
		  failwith "Neturl.file_url_of_local_path: cwd is not absolute"
	  )
      | `Relative_drive r ->
	  ( match classify_path (getcwd()) with
		`Absolute_local l ->
		  ( match Netstring_pcre.string_match drive_letter_re' l 0 with
			Some m ->
			  let letter = Netstring_pcre.matched_group m 1 l in
			  `Absolute_local("/" ^ letter ^ ":" ^ r)
		      | None ->
			  assert false
		  )
	      | `Absolute_remote(h,l) ->
		  `Absolute_remote(h,r)
	      | _ ->
		  failwith "Neturl.file_url_of_local_path: cwd is not absolute"
	  )
      | other -> other
  in
  (* Generate the URL: *)
  let syntax = { file_url_syntax with url_accepts_8bits = true } in
  match p_abs_class with
      `Absolute_local l ->
	let path = split_path l in
	make_url ~scheme:"file" ~host:"localhost" ~path syntax
    | `Absolute_remote(host,l) ->
	let path = split_path l in
	make_url ~scheme:"file" ~host ~path syntax
    | _ ->
	assert false
;;


let drive_letter_comp_re = Netstring_pcre.regexp "^([A-Za-z])(:|\\|)";;

let local_path_of_file_url u =
  let local_path p =
    if p = [] || List.hd p <> "" then
      failwith "Neturl.local_path_of_file_url: URL is not absolute";
    match os_type with
	"Unix" ->
	  join_path p
      | "Win32" ->
	  (* There must be a drive letter: *)
	  ( match p with
		("" :: drive :: rest) ->
		  (match Netstring_pcre.string_match drive_letter_comp_re drive 0 with
		       Some m ->
			 let letter = Netstring_pcre.matched_group m 1 drive in
			 let rest = if rest = [] then [""] else rest in
			 join_path((letter ^ ":") :: rest)
		     | None ->
			 failwith "Neturl.local_path_of_file_url: URL is not absolute"; 
		  )
	      | _ ->
		  failwith "Neturl.local_path_of_file_url: URL is not absolute"; 
	  )
      | "Cygwin" ->
	  (* Recognize drive letters: *)
	  ( match p with
		("" :: drive :: rest) ->
		  (match Netstring_pcre.string_match drive_letter_comp_re drive 0 with
		       Some m ->
			 let letter = Netstring_pcre.matched_group m 1 drive in
			 join_path("" :: "cygdrive" :: letter :: rest)
		     | None ->
			 join_path p
		  )
	      | _ ->
		  join_path p
	  )
      | _ ->
	  assert false
  in
  let remote_path host p =
    if p = [] || List.hd p <> "" then
      failwith "Neturl.local_path_of_file_url: URL is not absolute";
    match os_type with
	"Unix" ->
	  failwith "Neturl.local_path_of_file_url: Cannot process non-local file URLs"
      | "Win32" | "Cygwin" ->
	  join_path( "" :: "" :: host :: List.tl p)
      | _ ->
	  assert false
  in
  let opt f =
    try Some(f u) with Not_found -> None
  in
  match (opt url_scheme), (opt url_host), (url_path u) with
      (Some "file", (Some("localhost"|"")|None), []) ->
	local_path [""]
    | (Some "file", (Some("localhost"|"")|None), p) ->
	local_path p
    | (Some "file", Some host, []) ->
	remote_path host [""]
    | (Some "file", Some host, p) ->
	remote_path host p
    | (Some _, _, _) ->
	failwith "Neturl.local_path_of_file_url: Unexpected scheme"
    | (None, _, _) ->
	failwith "Neturl.local_path_of_file_url: Missing scheme (relative URL?)"
;;
