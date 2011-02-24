(* $Id: mimestring.ml 1062 2006-12-17 20:17:36Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

module S = Netstring_pcre;;

let cr_or_lf_re = S.regexp "[\013\n]";;

let header_stripped_re =
  S.regexp "([^ \t\r\n:]+):[ \t]*((.*[^ \t\r\n])?([ \t\r]*\n[ \t](.*[^ \t\r\n])?)*)[ \t\r]*\n";;
(* This expression extracts the name and the stripped value (whitespace is
 * removed at the beginning and at the end). Explanations:
 * ([^ \t\r\n:]+)             The name of the header field, all but whitespace
 * :                          The colon seperating the name from the value
 * [ \t]*                     Optional whitespace after the colon. It does not
 *                            count.
 * (.*[^ \t\r\n])?            A string not containing \n (LF) at all. It may 
 *                            contain whitespace including \r (CR) but not as
 *                            last character. The intention is: match 
 *                            everything until the next LF or CRLF, but don't
 *                            include whitespace at the end of the string.
 * ([ \t\r]*\n[ \t](.*[^ \t\r\n])?)*  
 *                            Any number of continuation lines. These lines
 *                            begin after skipping the whitespace at the end
 *                            of the last line, skipping a LF or CRLF 
 *                            terminating the line followed by exactly
 *                            one whitespace. This whitespace indicates that
 *                            a continuation line follows. The rule for
 *                            the continuation line itself is essentially the
 *                            same as the rule for the first line.
 * [ \t\r]*\n                 Finally, skip the whitespace at the end of the
 *                            last line, following by LF or CRLF terminating
 *                            the line.
 *
 * Note that the first group matches the name of the field, and that the
 * second group matches the value of the field. The value does neither include
 * whitespace at its beginning nor whitespace at its end.
 *)

let header_unstripped_re =
  S.regexp "([^ \t\r\n:]+):([ \t]*.*\n([ \t].*\n)*)";;
(* This much simpler expression returnes the name and the unstripped 
 * value.
 *)


let empty_line_re =
  S.regexp "\013?\n";;

let end_of_header_re =
  S.regexp "\n\013?\n";;


let scan_header ?(downcase=true) 
                ?(unfold=true) 
		?(strip=false)
		parstr ~start_pos:i0 ~end_pos:i1 =
  let header_re =
    if unfold || strip then header_stripped_re else header_unstripped_re in
  let rec parse_header i l =
    match S.string_match header_re parstr i with
	Some r ->
	  let i' = S.match_end r in
	  if i' > i1 then
	    failwith "Mimestring.scan_header";
	  let name = 
	    if downcase then
	      String.lowercase(S.matched_group r 1 parstr) 
	    else
	      S.matched_group r 1 parstr
	  in
	  let value_with_crlf =
	    S.matched_group r 2 parstr in
	  let value =
	    if unfold then
	      S.global_replace cr_or_lf_re "" value_with_crlf
	    else
	      value_with_crlf
	  in
	  parse_header i' ( (name,value) :: l)
      | None ->
	  (* The header must end with an empty line *)
	  begin match S.string_match empty_line_re parstr i with
	      Some r' ->
		List.rev l, S.match_end r'
	    | None ->
		failwith "Mimestring.scan_header"
	  end
  in
  parse_header i0 []
;;


let read_header ?downcase ?unfold ?strip (s : Netstream.in_obj_stream) =
  let rec find_end_of_header() =
    try
      let b = Netbuffer.unsafe_buffer s#window in
      (* Maybe the header is empty. In this case, there is an empty line
       * right at the beginning
       *)
      begin match S.string_match empty_line_re b 0 with
	  Some r ->
	    S.match_end r
	| None ->
	    (* Search the empty line: *)
	    let _, r = S.search_forward end_of_header_re b 0 in (*or Not_found*)
	    if S.match_end r > s#window_length then raise Not_found;
	    S.match_end r
      end
    with
	Not_found ->
	  if s#window_at_eof then (
	    failwith "Mimestring.read_header";
	  );
	  s#want_another_block();
	  find_end_of_header()
  in
  let end_pos = find_end_of_header() in
  let b = Netbuffer.unsafe_buffer s#window in
  let header, _ = scan_header ?downcase ?unfold ?strip b ~start_pos:0 ~end_pos 
  in
  s # skip end_pos;
  header
;;


let write_re =
  S.regexp "[ \t\r]*(\n|\\z)([ \t\r]*)" ;;
(* Note: \z matches the end of the buffer (PCRE) *)

let ws_re =
  S.regexp "^[ \t\r\n]*" ;;

let write_header ?(soft_eol = "\r\n") ?(eol = "\r\n") ch h =
  let hd = Buffer.create 120 in
  List.iter
    (fun (n,v) ->
       Buffer.add_string hd (n ^ ": ");
       let l = String.length v in
       let pos = ref 0 in
       ( match S.string_match ws_re v 0 with
	     Some r ->
	       pos := S.match_end r
	   | None -> 
	       assert false
       );
       try
	 while !pos < l do
	   let (k, r) = S.search_forward write_re v !pos in  
	                (* might raise Not_found *)
	   Buffer.add_substring hd v !pos (k - !pos);
	   let pos' = S.match_end r in
	   if pos' < l then begin 
	     (* Before printing linear whitespace, ensure that the line
	      * would not be empty
	      *)
	     if v.[pos'] <> '\n' then begin
	       (* The line would not be empty. Print eol and all found
		* spaces and TABs, but no CRs. Ensure that there is at least
		* one space.
		*)
	       Buffer.add_string hd soft_eol;
	       let found_space = ref false in
	       for i = S.group_beginning r 2 to pos' - 1 do
		 let c = v.[i] in
		 if c = ' ' || c = '\t' then begin
		   Buffer.add_char hd c;
		   found_space := true;
		 end
	       done;
	       if not !found_space then
		 Buffer.add_char hd ' ';
	     end
	       (* else: The line would consist only of whitespace. We simply
		* drop all of this whitespace.
		*)
	   end;
	   pos := pos'
	 done;
	 Buffer.add_string hd eol;
       with
	   Not_found ->
	     assert false  (* The reg exp matches always *)
    )
    h;
  Buffer.add_string hd eol;
  ch # output_buffer hd
;;


type s_token =
    Atom of string
  | EncodedWord of ((string * string) * string * string)
  | QString of string
  | Control of char
  | Special of char
  | DomainLiteral of string
  | Comment
  | End
;;

type s_option =
    No_backslash_escaping
  | Return_comments
  | Recognize_encoded_words
;;

type s_extended_token =
    { token      : s_token;
      token_pos  : int;
      token_line : int;
      token_linepos : int;   (* Position of the beginning of the line *)
      token_len  : int;
      mutable token_sep : bool; (* separates adjacent encoded words *)
    }
;;

let get_token et  = et.token;;
let get_pos et    = et.token_pos;;
let get_line et   = et.token_line;;
let get_column et = et.token_pos - et.token_linepos;;
let get_length et = et.token_len;;
let separates_adjacent_encoded_words et = et.token_sep;;

let get_language et =
  match et.token with
      EncodedWord((_,lang),_,_) -> lang
    | _ -> ""
;;

let get_decoded_word et =
  match et.token with
      Atom s -> s
    | QString s -> s
    | Control c -> String.make 1 c
    | Special c -> String.make 1 c
    | DomainLiteral s -> s
    | Comment -> ""
    | EncodedWord (_, encoding, content) ->
	( match encoding with
	      ("Q"|"q") ->
		Netencoding.Q.decode content
	    | ("B"|"b") -> 
		Netencoding.Base64.decode 
		  ~url_variant:false
		  ~accept_spaces:false
		  content
	    | _ -> failwith "get_decoded_word"
	)
    | End -> 
	failwith "get_decoded_word"
;;

let get_charset et =
  match et.token with
      EncodedWord ((charset,_), _, _) -> charset
    | End -> failwith "get_charset"
    | _ -> "US-ASCII"
;;

type scanner_spec =
    { (* What the user specifies: *)
      scanner_specials : char list;
      scanner_options : s_option list;
      (* Derived from that: *)
      mutable opt_no_backslash_escaping : bool;
      mutable opt_return_comments : bool;
      mutable opt_recognize_encoded_words : bool;

      mutable is_special : bool array;
      mutable space_is_special : bool;
    }
;;

type scanner_target =
    { scanned_string : string;
      mutable scanner_pos : int;
      mutable scanner_line : int;
      mutable scanner_linepos : int; 
      (* Position of the beginning of the line *)
      mutable scanned_tokens : s_extended_token Queue.t;
      (* A queue of already scanned tokens in order to look ahead *)
      mutable last_token : s_token;
      (* The last returned token. It is only important whether it is
       * EncodedWord or not.
       *)
    }
;;

type mime_scanner = scanner_spec * scanner_target
;;

let get_pos_of_scanner (spec, target) = 
  if spec.opt_recognize_encoded_words then
    failwith "get_pos_of_scanner"
  else
    target.scanner_pos
;;

let get_line_of_scanner (spec, target) = 
  if spec.opt_recognize_encoded_words then
    failwith "get_line_of_scanner"
  else
    target.scanner_line
;;

let get_column_of_scanner (spec, target) = 
  if spec.opt_recognize_encoded_words then
    failwith "get_column_of_scanner"
  else
    target.scanner_pos - target.scanner_linepos 
;;

let create_mime_scanner ~specials ~scan_options =
  let is_spcl = Array.create 256 false in
  List.iter
    (fun c -> is_spcl.( Char.code c ) <- true)
    specials;
  let spec =
    { scanner_specials = specials;
      scanner_options = scan_options;
      opt_no_backslash_escaping = 
	List.mem No_backslash_escaping scan_options;
      opt_return_comments = 
	List.mem Return_comments scan_options;
      opt_recognize_encoded_words = 
	List.mem Recognize_encoded_words scan_options;
      is_special = is_spcl;
      space_is_special = is_spcl.(32);
    }
  in
  (* Grab the remaining arguments: *)
  fun ?(pos=0) ?(line=1) ?(column=0) s ->
    let target =
      { scanned_string = s;
	scanner_pos = pos;
	scanner_line = line;
	scanner_linepos = pos - column;
	scanned_tokens = Queue.create();
	last_token = Comment;   (* Must not be initialized with EncodedWord *)
      }
    in
    spec, target
;;


let encoded_word_re =
  S.regexp "=\\?([^?]+)\\?([^?]+)\\?([^?]+)\\?=";;

let scan_next_token (spec,target) =
  let mk_pair t len =
    { token = t;
      token_pos = target.scanner_pos;
      token_line = target.scanner_line;
      token_linepos = target.scanner_linepos;
      token_len = len;
      token_sep = false;
    },
    t
  in

  (* Note: mk_pair creates a new token pair, and it assumes that 
   * target.scanner_pos (and also scanner_line and scanner_linepos)
   * still contain the position of the beginning of the token.
   *)

  let s = target.scanned_string in
  let l = String.length s in
  let rec scan i =
    if i < l then begin
      let c = s.[i] in
      if spec.is_special.( Char.code c ) then begin
	let pair = mk_pair (Special c) 1 in
	target.scanner_pos <- target.scanner_pos + 1;
	(match c with
	     '\n' -> 
	       target.scanner_line    <- target.scanner_line + 1;
	       target.scanner_linepos <- target.scanner_pos;
	   | _ -> ()
	);
	pair
      end
      else
	match c with
	    '"' -> 
	      (* Quoted string: *)
	      scan_qstring (i+1) (i+1) 0
	  | '(' ->
	      (* Comment: *)
	      let i', line, linepos = 
		scan_comment (i+1) 0 target.scanner_line target.scanner_linepos
	      in
	      let advance() =
		target.scanner_pos <- i';
		target.scanner_line <- line;
		target.scanner_linepos <- linepos
	      in
	      if spec.opt_return_comments then begin
		let pair = mk_pair Comment (i' - i) in
		advance();
		pair
	      end
	      else 
		if spec.space_is_special then begin
		  let pair = mk_pair (Special ' ') (i' - i) in
		  advance();
		  pair
		end
		else begin
		  advance();
		  scan i'
		end
	  | (' '|'\t'|'\r') ->
	      (* Ignore whitespace by default: *)
	      target.scanner_pos <- target.scanner_pos + 1;
	      scan (i+1)
	  | '\n' ->
	      (* Ignore whitespace by default: *)
	      target.scanner_pos     <- target.scanner_pos + 1;
	      target.scanner_line    <- target.scanner_line + 1;
	      target.scanner_linepos <- target.scanner_pos;
	      scan (i+1)
	  | ('\000'..'\031'|'\127'..'\255') ->
	      let pair = mk_pair (Control c) 1 in
	      target.scanner_pos <- target.scanner_pos + 1;
	      pair
	  | '[' ->
	      (* Domain literal: *)
	      scan_dliteral (i+1) (i+1) 0
	  | _ ->
	      scan_atom i i
    end
    else 
      mk_pair End 0

  and scan_atom i0 i =
    let return_atom() =
      let astring = String.sub s i0 (i-i0) in
      let r =
	if spec.opt_recognize_encoded_words then
	  S.string_match ~groups:4 encoded_word_re astring 0
	else
	  None
      in
      match r with
	  None ->
	    (* An atom contains never a linefeed character, so we can ignore
	     * scanner_line here.
	     *)
	    let pair = mk_pair (Atom astring) (i-i0) in
	    target.scanner_pos <- i;
	    pair
	| Some mr ->
	    (* Found an encoded word. *)
	    let charset_lang = S.matched_group mr 1 astring in
	    let encoding = S.matched_group mr 2 astring in
	    let content  = S.matched_group mr 3 astring in
	    (* Check if it is an extended encoded word (RFC 2231): *)
	    let charset,lang =
	      try
		let l = String.length charset_lang in
		let k = String.index charset_lang '*' in
		(String.sub charset_lang 0 k,
		 String.sub charset_lang (k+1) (l - k - 1))
	      with
		  Not_found -> (charset_lang, "") 
	    in
	    let t = EncodedWord((String.uppercase charset, lang),
				 String.uppercase encoding,
				 content) in
	    let pair = mk_pair t (i-i0) in
	    target.scanner_pos <- i;
	    pair
    in

    if i < l then
      let c = s.[i] in
      match c with
	  ('\000'..'\031'|'\127'..'\255'|'"'|'('|'['|' ') ->
	    return_atom()
	| _ ->
	    if spec.is_special.( Char.code c ) then
	      return_atom()
	    else
	      scan_atom i0 (i+1)
    else
      return_atom()

  and scan_qstring i0 i n =
    if i < l then
      let c = s.[i] in
      match c with
	  '"' ->
	    (* Regular end of the quoted string: *)
	    let content, line, linepos = copy_qstring i0 (i-1) n in
	    let pair = mk_pair (QString content) (i-i0+2) in
	    target.scanner_pos <- i+1;
	    target.scanner_line <- line;
	    target.scanner_linepos <- linepos;
	    pair
	| '\\' when not spec.opt_no_backslash_escaping ->
	    scan_qstring i0 (i+2) (n+1)
	| _ ->
	    scan_qstring i0 (i+1) (n+1)
    else
      (* Missing right double quote *)
      let content, line, linepos = copy_qstring i0 (l-1) n in
      let pair = mk_pair (QString content) (l-i0+1) in
      target.scanner_pos <- l;
      target.scanner_line <- line;
      target.scanner_linepos <- linepos;
      pair

  and copy_qstring i0 i1 n =
    (* Used for quoted strings and for domain literals *)
    let r = String.create n in
    let k = ref 0 in
    let line = ref target.scanner_line in
    let linepos = ref target.scanner_linepos in
    let esc = ref false in
    for i = i0 to i1 do
      let c = s.[i] in
      match c with
	  '\\' when i < i1 &&  not spec.opt_no_backslash_escaping && not !esc ->
	    esc := true
	| '\n' ->
	    line := !line + 1;
	    linepos := i+1;
	    r.[ !k ] <- c; 
	    incr k;
	    esc := false
	| _ -> 
	    r.[ !k ] <- c; 
	    incr k;
	    esc := false
    done;
    assert (!k = n);
    r, !line, !linepos

  and scan_dliteral i0 i n =
    if i < l then
      let c = s.[i] in
      match c with
	  ']' ->
	    (* Regular end of the domain literal: *)
	    let content, line, linepos = copy_qstring i0 (i-1) n in
	    let pair = mk_pair (DomainLiteral content) (i-i0+2) in
	    target.scanner_pos <- i+1;
	    target.scanner_line <- line;
	    target.scanner_linepos <- linepos;
	    pair
	| '\\' when not spec.opt_no_backslash_escaping ->
	    scan_dliteral i0 (i+2) (n+1)
	| _ ->
	    (* Note: '[' is not allowed by RFC 822; we treat it here as
	     * a regular character (questionable)
	     *)
	    scan_dliteral i0 (i+1) (n+1)
    else
      (* Missing right bracket *)
      let content, line, linepos = copy_qstring i0 (l-1) n in
      let pair = mk_pair (DomainLiteral content) (l-i0+1) in
      target.scanner_pos <- l;
      target.scanner_line <- line;
      target.scanner_linepos <- linepos;
      pair


  and scan_comment i level line linepos =
    if i < l then
      let c = s.[i] in
      match c with
	  ')' ->
	    (i+1), line, linepos
	| '(' ->
	    (* nested comment *)
	    let i', line', linepos' = 
	      scan_comment (i+1) (level+1) line linepos 
	    in
	    scan_comment i' level line' linepos'
	| '\\' when not spec.opt_no_backslash_escaping ->
	    if (i+1) < l && s.[i+1] = '\n' then
	      scan_comment (i+2) level (line+1) (i+2)
	    else
	      scan_comment (i+2) level line linepos
	| '\n' ->
	    scan_comment (i+1) level (line+1) (i+1)
	| _ ->
	    scan_comment (i+1) level line linepos
    else
      (* Missing closing ')' *)
      i, line, linepos
  in

  scan target.scanner_pos
;;


let scan_token ((spec,target) as scn) =
  (* This function handles token queueing in order to recognize white space
   * that separates adjacent encoded words.
   *)

  let rec collect_whitespace () =
    (* Scans whitespace tokens and returns them as:
     * (ws_list, other_tok)     if there is some other_tok following the
     *                          list (other_tok = End is possible)
     *)
    let (et, t) as pair = scan_next_token scn in
    ( match t with
	  (Special ' '|Special '\t'|Special '\n'|Special '\r') ->
	    let ws_list, tok = collect_whitespace() in
	    pair :: ws_list, tok
	| _ ->
	    [], pair
    )
  in

  try
    (* Is there an already scanned token in the queue? *)
    let et = Queue.take target.scanned_tokens in
    let t = et.token in
    target.last_token <- t;
    et, et.token
  with
      Queue.Empty ->
	(* If not: inspect the last token. If that token is an EncodedWord,
	 * the next tokens are scanned in advance to determine if there
	 * are spaces separating two EncodedWords. These tokens are put
	 * into the queue such that it is avoided that they are scanned
	 * twice. (The sole purpose of the queue.)
	 *)
	match target.last_token with
	    EncodedWord(_,_,_) ->
	      let ws_list, tok = collect_whitespace() in
	      (* If tok is an EncodedWord, too, the tokens in ws_list must
	       * be flagged as separating two adjacent encoded words. 
	       *)
	      ( match tok with
		    _, EncodedWord(_,_,_) ->
		      List.iter
			(fun (et,t) ->
			   et.token_sep <- true)
			ws_list
		  | _ ->
		      ()
	      );
	      (* Anyway, queue the read tokens but the first up *)
	      ( match ws_list with
		    [] ->
		      (* Nothing to queue *)
		      let et, t = tok in
		      target.last_token <- t;
		      tok
		  | (et,t) as pair :: ws_list' ->
		      List.iter
			(fun (et',_) -> 
			   Queue.add et' target.scanned_tokens)
			ws_list';
		      ( match tok with
			  | _, End ->
			      ()
			  | (et',_) ->
			      Queue.add et' target.scanned_tokens
		      );
		      (* Return the first scanned token *)
		      target.last_token <- t;
		      pair
	      )
	  | _ ->
	      (* Regular case: Scan one token; do not queue it up *)
	      let (et, t) as pair = scan_next_token scn in 
	      target.last_token <- t;
	      pair
;;
	

let scan_token_list scn =
  let rec collect() =
    match scan_token scn with
	_, End ->
	  []
      | pair ->
	  pair :: collect()
  in
  collect()
;;


let scan_structured_value s specials options =
  let rec collect scn =
    match scan_token scn with
	_, End ->
	  []
      | _, t ->
	  t :: collect scn
  in
  let scn = create_mime_scanner specials options s in
  collect scn
;;


let specials_rfc822 =
  [ '<'; '>'; '@'; ','; ';'; ':'; '\\'; '.' ];;


let specials_rfc2045 =
  [ '<'; '>'; '@'; ','; ';'; ':'; '\\'; '/' ];;


let scan_encoded_text_value s =
  let specials = [ ' '; '\t'; '\r'; '\n'; '('; '['; '"' ] in
  let options =  [ Recognize_encoded_words ] in
  let scn = create_mime_scanner specials options s in
  
  let rec collect () =
    match scan_token scn with
	_, End ->
	  []
      | et, _ when separates_adjacent_encoded_words et ->
	  collect()
      | et, (Special _|Atom _|EncodedWord(_,_,_)) ->
	  et :: collect ()
      | et, (Control _) ->  (* ignore control characters *)
	  collect ()
      | _, _ ->
	  assert false
  in
  collect()
;;


let split_mime_type ct_type =
  let specials = specials_rfc2045 @ [ '"'; '['; ']' ] in
  let scn = create_mime_scanner specials [] ct_type in
  let rec collect () =
    match scan_token scn with
	_, End ->
	  []
      | _, tok ->
	  tok :: collect()
  in
  match collect() with
      [ Atom main_type; Special '/'; Atom sub_type ] ->
	(String.lowercase main_type, String.lowercase sub_type)
    | _ ->
	failwith "Mimestring.split_mime_type"
;;


type s_param =
    P_encoded of (string * string * string)
  | P of string
;;


let param_value = function 
    P_encoded(_,_,v) -> v
  | P v -> v ;;

let param_charset = function
    P_encoded(cs,_,_) -> cs
  | P _ -> "" ;;

let param_language = function
    P_encoded(_,l,_) -> l
  | P _ -> "" ;;

let mk_param ?(charset = "") ?(language = "") v =
  if charset = "" && language = "" then
    P v
  else
    P_encoded(charset,language,v)
;;

let print_s_param fmt = function
    P_encoded(cs,l,v) -> 
      Format.fprintf fmt
	"<S_PARAM charset=\"%s\" lang=\"%s\" value=\"%s\">"
	(String.escaped cs) (String.escaped l) (String.escaped v)
  | P v ->
      Format.fprintf fmt
	"<S_PARAM value=\"%s\">"
	(String.escaped v)
;;


let name_re = S.regexp "^([^*]+)(\\*\\d+)?(\\*)?$" ;;

let encoding_re = S.regexp "^([^\\']*)\\'([^\\']*)\\'(.*)$" ;;

let opt f x =
  try Some(f x) with Not_found -> None ;;

let decode_rfc2231_params ?(decode = true) params =
  (* Decodes the parameters passed as [(string * string) list] into
   * a [(string * s_param) list], applying the rules of RFC 2231.
   *
   * - Continuations are always recognized and decoded
   * - Encoded parameters are recognized if [~decode = true], and returned
   *   as [P_encoded].
   *)
  let p_complete = ref [] in         (* Parameters not defined in fragments *)
  let p_hash = Hashtbl.create 10 in  (* Fragmented parameters *)
  (* The keys are the names of the parameters. The values are pairs
   * (max, map) where max is the biggest fragment number so far,
   * and [map] is another hashtable mapping fragment number to [s_param].
   *)
  List.iter
    (fun (rawname, rawvalue) ->
       (* First look at [rawname]. It may have one of the forms:
	* NAME
	* NAME*
	* NAME*N
	* NAME*N*
	*)
       let name, n, star =
	 match S.string_match name_re rawname 0 with
	     None ->
	       (rawname, -1, false)
	   | Some r ->
	       (S.matched_group r 1 rawname,
		(match opt (S.matched_group r 2) rawname with
		     None -> -1
		   | Some v ->
		       int_of_string(String.sub v 1 (String.length v - 1))
		),
		(opt (S.matched_group r 3) rawname <> None)
	       )
       in
       let name, n, star =
	 if star && not decode then
	   (name ^ "*", n, false)
	   (* suppress recognition of encoded parameters *)
	 else
	   (name, n, star)
       in

       (* If necessary, decode the parameter value *)
       let value =
	 if star then begin
	   if n <= 0 then begin
	     match S.string_match encoding_re rawvalue 0 with
		 None ->
		   failwith "Mimestring.decode_rfc2231_params"
	       | Some r ->
		   let charset = S.matched_group r 1 rawvalue in
		   let lang = S.matched_group r 2 rawvalue in
		   let escaped_value = S.matched_group r 3 rawvalue in
		   P_encoded(String.uppercase charset, 
			     lang, 
			     Netencoding.Url.decode ~plus:false escaped_value)
	   end
	   else
	     P (Netencoding.Url.decode ~plus:false rawvalue)
	 end
	 else
	   P rawvalue
       in
       (* Store the value in p_complete or p_hash *)
       if n < 0 then
	 p_complete := (name, value) :: !p_complete
       else begin
	 let mx, map =
	   try Hashtbl.find p_hash name 
	   with 
	       Not_found -> 
		 let pair = (ref (-1), Hashtbl.create 10) in
		 Hashtbl.add p_hash name pair;
		 pair
	 in
	 mx := max !mx n;
	 if Hashtbl.mem map n then failwith "Mimestring.decode_rfc2231_params";
	 Hashtbl.add map n value
       end
    )
    params;
  (* Merge the fragments in p_hash: *)
  Hashtbl.iter
    (fun name (mx,map) ->
       let vl = ref [ ] in
       for i = 1 to !mx do
	 let v =
	   try Hashtbl.find map i 
	   with Not_found -> failwith "Mimestring.decode_rfc2231_params"
	 in
	 match v with
	     P s -> vl := s :: !vl
	   | _   -> assert false
       done;
       match 
	 ( try Hashtbl.find map 0 
	   with Not_found -> failwith "Mimestring.decode_rfc2231_params"
	 )
       with
	   P_encoded(charset,lang,v0) ->
	     p_complete := (name, 
			    P_encoded (charset, 
				       lang,
				       String.concat "" (v0 :: List.rev !vl))
			   ) :: !p_complete
	 | P v0 ->
	     p_complete := (name, P(String.concat "" (v0 :: List.rev !vl))) ::
	                   !p_complete
    )
    p_hash;
  (* Return result *)
  List.rev !p_complete;
;;


let scan_value_with_parameters_ep_int ?decode s options =
  let rec parse_params tl =
    match tl with
	Atom n :: Special '=' :: Atom v :: tl' ->
	  (n,v) :: parse_rest tl'
      | Atom n :: Special '=' :: QString v :: tl' ->
	  (n,v) :: parse_rest tl'
      (* The following cases are sometimes created by erroneous MUAs: *)
      | Special '=' :: Atom v :: tl' ->
	  parse_rest tl'  (* this may fail... *)
      | Special '=' :: QString v :: tl' ->
	  parse_rest tl'
      | _ ->
	  failwith "Mimestring.scan_value_with_parameters"
  and parse_rest tl =
    match tl with
	[] -> []
      | Special ';' :: tl' ->
	  parse_params tl'
      | _ ->
	  failwith "Mimestring.scan_value_with_parameters"
  in

  (* Note: Even if not used here, the comma is a very common separator
   * and should be recognized as being special. You will get a
   * failure if there is a comma in the scanned string.
   *)
  let tl = scan_structured_value s [ ';'; '='; ',' ] options in
  let v, pl =
    match tl with
	[ Atom n ] -> n, []
      | [ QString n ] -> n, []
      | Atom n :: Special ';' :: tl' ->
	  n, parse_params tl'
      | QString n :: Special ';' :: tl' ->
	  n, parse_params tl'
      | _ ->
	  failwith "Mimestring.scan_value_with_parameters"
  in
  (v, decode_rfc2231_params ?decode pl)
;;


let scan_value_with_parameters_ep s options = 
  scan_value_with_parameters_ep_int ~decode:true s options ;;

let scan_value_with_parameters s options =
  let v,pl = scan_value_with_parameters_ep_int ~decode:false s options in
  (v, 
   List.map
     (function 
	  (_, P_encoded(_,_,_)) -> assert false   
	                           (* not possible because of ~decode:false *)
	| (n,P v) -> (n,v)
     )
     pl
  )
;;
  

let scan_mime_type s options =
  let n, params = scan_value_with_parameters s options in
  (String.lowercase n),
  (List.map (fun (n,v) -> (String.lowercase n, v)) params)
;;


let scan_mime_type_ep s options =
  let n, params = scan_value_with_parameters_ep s options in
  (String.lowercase n),
  (List.map (fun (n,v) -> (String.lowercase n, v)) params)
;;


let generate_encoded_words cs lang enc_tok text len1 len =
  (* Generate a list of encoded words for the charset [cs], the
   * language [lang], encoded as [enc_tok]. The text is passed
   * in [text] (encoded only in [cs], [enc_tok] is not applied). 
   *
   * [len1] are the number of bytes of the first token,
   * [len] are the number of bytes for the following tokens.
   * If [len1] is too small for at least one token, [len]
   * is also used for the first token.
   *
   * If [cs] is not known to [Netconversion], it is assumed
   * it is a single-byte encoding.
   *)
  (* Determine prefix and suffix of encoded words: *)
  let prefix = 
    "=?" ^ cs ^ 
    (if lang <> "" then "*" ^ lang else "") ^
    ( match enc_tok with
	  "Q"|"q" -> "?Q?"
	| "B"|"b" -> "?B?"
	| _ -> failwith "Mimestring.write_value: Unknown encoding"
    ) in
  let suffix = "?=" in
  let prefix_len = String.length prefix in
  let suffix_len = String.length suffix in

  let encode s =
    match enc_tok with
	"Q"|"q" -> Netencoding.Q.encode s
      | "B"|"b" -> Netencoding.Base64.encode s
      | _ -> assert false (* already caught above *)
  in

  let find_tok_single_byte k l =
    (* k: The position where to determine the token
     * l: Desired length of the token
     *
     * Returns the position of the end of the token
     *)
    let p = ref k in
    let l_cur() = 
      let slice = String.sub text k (!p - k) in
      prefix_len + String.length (encode slice) + suffix_len
    in
    if l_cur() > l then
      k
    else (
      try
	while l_cur() <= l do
	  incr p;
	  if !p >= String.length text then raise Exit
	done;
	decr p;
	!p
      with
	  Exit ->
	    !p  (* at the end of the string *)
    )
  in

  let rec generate_single_byte k l =
    (* k: Position in enc_text
     * l: Desired length of the token
     *)
    if k >= String.length text then
      []
    else (
      let j = ref(find_tok_single_byte k l) in
      if k = !j then (
	(* l is too short, use len instead *)
	j := find_tok_single_byte k len;
	(* Still too short: Process only one char *)
	if k = !j then (
	  j := k+1
	)
      );
      let tok =
	prefix ^ 
	encode(String.sub text k (!j - k)) ^
	suffix in
      tok :: generate_single_byte !j len
    )
  in

  let find_tok_multi_byte cursor l =
    (* cursor: The cursor moving around text
     * l: Desired length of the token
     *
     * Moves the cursor to the end of the token
     *)
    let k_start = Netconversion.cursor_pos cursor in
    let l_cur() = 
      let k_cur = Netconversion.cursor_pos cursor in
      let slice = String.sub text k_start (k_cur - k_start) in
      prefix_len + String.length (encode slice) + suffix_len
    in
    if l_cur() > l then
      ()
    else (
      try
	while l_cur() <= l do
	  Netconversion.move cursor
	done;
	Netconversion.move ~num:(-1) cursor
      with
	  Netconversion.Cursor_out_of_range ->
	    ()  (* at the end of the string *)
    )
  in

  let rec generate_multi_byte cursor l =
    (* cursor: The cursor moving around text
     * l: Desired length of the token
     *)
    if Netconversion.cursor_at_end cursor then
      []
    else (
      let k_start = Netconversion.cursor_pos cursor in
      find_tok_multi_byte cursor l;
      if k_start = Netconversion.cursor_pos cursor then (
	(* l is too short, use len instead *)
	find_tok_multi_byte cursor len;
	(* Still too short: Process only one char *)
	if k_start = Netconversion.cursor_pos cursor then (
	  Netconversion.move cursor
	)
      );
      let k_end = Netconversion.cursor_pos cursor in
      let tok =
	prefix ^ 
	encode(String.sub text k_start (k_end - k_start)) ^
	suffix in
      tok :: generate_multi_byte cursor len
    )
  in

  (* Check if the charset(encoding) is known, and if so, is available. *)
  let is_single_byte, e =
    try
      let enc = Netconversion.encoding_of_string cs in
                (* or Failure *)
      ( Netconversion.is_single_byte enc ||
	not (List.mem enc (Netconversion.available_input_encodings())),
	enc )
    with
	Failure _ -> (true, `Enc_empty) in

  (* For single-byte, or when the multi-byte encoding is not
   * available, use [generate_single_byte]. 
   * For available multi-byte encodings, use [generate_multi_byte]
   * to split the string only at allowed positions.
   *)
  if is_single_byte then
    generate_single_byte 0 len1
  else
    let cursor = Netconversion.create_cursor e text in
    generate_multi_byte cursor len1
;;


exception Line_too_long;;

let write_value ?maxlen1 ?maxlen ?hardmaxlen1 ?hardmaxlen 
                ?(fold_qstring = true) ?(fold_literal = true) 
                ?unused ?hardunused
                (ch : Netchannels.out_obj_channel) tl =
  let do_folding = (maxlen <> None && maxlen1 <> None) in
    (* Whether to fold or not *)
  let have_hard_limit = 
    do_folding && (hardmaxlen <> None && hardmaxlen1 <> None) in
    (* Whether there is a hard limit or not *)
  let still_unused = ref (match maxlen1 with Some x -> x | None -> 0) in
    (* The number of characters that will fit into the current line *)
  let still_unused_hard = ref (match hardmaxlen1 with Some x -> x | None ->0) in
    (* The same number for the hard limit *)
  let break_buffer = Buffer.create 80 in
    (* Collects characters after the last breakpoint *)

  let output_nobreak ?(pos=0) ?len s =
    (* Outputs the string s without any breakpoint *)
    let len = (match len with Some l -> l | None -> String.length s - pos) in
    if do_folding then
      Buffer.add_substring break_buffer s pos len
    else
      ch # really_output s pos len
  in

  let flush() =
    (* Flushes the buffer (used by [output_break]) *)
    let l = Buffer.length break_buffer in
    ch # output_buffer break_buffer;
    still_unused := !still_unused - l;
    if have_hard_limit then still_unused_hard := !still_unused_hard - l;
    Buffer.clear break_buffer;
    (* Check hard limit: *)
    if have_hard_limit && !still_unused_hard < 0 then raise Line_too_long;
  in

  let get_maxlen() =
    match maxlen with 
	Some x -> x 
      | None -> assert false
  in

  let output_break() =
    (* Outputs a breakpoint *)
    if do_folding then begin
      (* If the material collected in [break_buffer] fits into the line,
       * just output it, and clear the [break_buffer].
       *)
      let l = Buffer.length break_buffer in
      if !still_unused >= l then 
	flush()
      else if l > 0 then begin
	(* Otherwise output first a linefeed... *)
	let new_length = get_maxlen() in
	if new_length > !still_unused then begin (* otherwise \n does not help*)
	  ch # output_char '\n';
	  still_unused := new_length;
	  (match hardmaxlen with
	       Some x -> still_unused_hard := x 
	     | None -> ());
	end;
	(* ... and then the line: *)
	flush();
      end
    end
  in

  let separate_words last =
    (* If [last] is one of the tokens Atom, QString, DomainLiteral, or
     * EncodedWord, output a breakpoint and a space.
     *)
    match last with
	Atom _ 
      | QString _ 
      | DomainLiteral _
      | EncodedWord(_,_,_) ->
	  output_break();
	  output_nobreak " "
      | _ ->
	  ()
  in

  let output_quoted special1 special2 fold s =
    let k1 = ref 0 in
    let k2 = ref 0 in
    let l = String.length s in
    let nobreak = ref true in
    while !k2 < l do
      let c = s.[ !k2 ] in
      match c with
	  '\\' ->
	    output_nobreak ~pos:!k1 ~len:(!k2 - !k1) s;
	    output_nobreak "\\\\";
	    incr k2;
	    k1 := !k2;
	    nobreak := false;
	| ' '
	| '\t'
	| '\n' ->
	    output_nobreak ~pos:!k1 ~len:(!k2 - !k1) s;
	    if fold && not !nobreak then output_break();
	    if fold && !nobreak && !k2 > 0 then output_nobreak "\\";
	    let s = match c with
		' ' | '\n' -> " "
	      | '\t' -> "\t"
	      | _ -> assert false
	    in
	    output_nobreak s;
	    incr k2;
	    k1 := !k2;
	    nobreak := true;  (* After a space there is never a breakpoint *)
	| '\r' ->
	    (* If the next character is LF: ignore. Otherwise 
	     * handle like space
	     *)
	    output_nobreak ~pos:!k1 ~len:(!k2 - !k1) s;
	    incr k2;
	    k1 := !k2;
	    if !k2 >= l || s.[ !k2 ] <> '\n' then begin
	      if fold && not !nobreak then output_break();
	      if fold && !nobreak && !k2 > 0 then output_nobreak "\\";
	      output_nobreak " ";
	      nobreak := true;
	    end
	| _ ->
	    if c = special1 || c = special2 then begin
	      output_nobreak ~pos:!k1 ~len:(!k2 - !k1) s;
	      output_nobreak "\\";
	      output_nobreak (String.make 1 c);
	      incr k2;
	      k1 := !k2;
	    end
	    else begin
	      incr k2;
	    end;
	    nobreak := false;
    done;
    output_nobreak ~pos:!k1 ~len:(!k2 - !k1) s;
  in

  let rec collect_words cs lang enc tl =
    match tl with
	EncodedWord((cs',lang'),enc',text) :: tl' ->
	  if cs=cs' && lang=lang' && enc=enc' then
	    let textrest, tl'' =  collect_words cs lang enc tl' in
	    (text :: textrest, tl'')
	  else
	    ([], tl')
      | _ ->
	  ([], tl)
  in
  
  let output_encoded cs lang enc_tok text =
    let len1 =
      max 0 (!still_unused - Buffer.length break_buffer) in
    let len =
      if do_folding then get_maxlen () - 1 else max_int in
    let words = 
      generate_encoded_words cs lang enc_tok text len1 len in
    let first = ref true in
    List.iter
      (fun word ->
	 if not !first then (
	   output_break();
	   output_nobreak " ";
	 );
	 output_nobreak word;
	 first := false
      )
      words
  in    

  let rec output_tokens last tl =
    match tl with
	Atom atext as cur :: tl' ->
	  separate_words last;
	  output_nobreak atext;
	  output_tokens cur tl'
      | Control c as cur :: tl' ->
	  output_nobreak (String.make 1 c);
	  output_tokens cur tl'
      | (Special ' ' | Special '\t' | Comment as cur) :: tl' ->
	  ( match last with
	        Special ' '
	      | Special '\t' 
	      | Comment -> ()
	      | Atom _
	      | QString _
	      | DomainLiteral _
	      | EncodedWord _ 
	      | Special _ -> output_break();
	      | _ -> ()
	  );
	  ( match cur with
		Special c ->
		  output_nobreak (String.make 1 c)
	      | Comment ->
		  output_nobreak " "
	      | _ ->
		  assert false
	  );
	  output_tokens cur tl'
      | Special c as cur :: tl' ->
	  output_nobreak (String.make 1 c);
	  output_tokens cur tl'
      | QString s as cur :: tl' ->
	  separate_words last;
	  output_nobreak "\"";
	  output_quoted '"' '"' fold_qstring s;
	  output_nobreak "\"";
	  output_tokens cur tl'
      | DomainLiteral s as cur :: tl' ->
	  separate_words last;
	  output_nobreak "[";
	  output_quoted '[' ']' fold_literal s;
	  output_nobreak "]";
	  output_tokens cur tl'
      | EncodedWord((cs,lang),enc,_) as cur :: _ ->
	  separate_words last;
	  let (textlist, tl'') = collect_words cs lang enc tl in
	  let text = String.concat "" textlist in
	  output_encoded cs lang enc text;
	  output_tokens cur tl''
      | End :: tl' ->
	  output_tokens End tl'
      | [] ->
	  ()
  in

  output_tokens End tl;
  output_break();
  
  if do_folding then begin
    match unused with
	Some r -> r := !still_unused
      | None -> ()
  end;
  if have_hard_limit then begin
    match hardunused with
	Some r -> r := !still_unused_hard
      | None -> ()
  end
;;

let hex = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
	     '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' |] ;;


let rec param_tokens ?(maxlen=max_int) pl =
  let qlen_char c =
    match c with
	'\\' | '"' -> 2
      | _ -> 1
  in

  let rec qlen s k =
    (* How long is [s] formatted as quoted string? *)
    if k >= String.length s then
      2  (* 2*quote *)
    else
      qlen_char (s.[k]) + qlen s (k+1)
  in

  let rec continued_param n s pos k =
    if pos >= String.length s then
      []
    else begin
      let name = n ^ "*" ^ string_of_int k in
      let l = ref (String.length name + 4) in
          (* 4 characters: semicolon, equal-to, 2*quote *)
      let p = ref pos in
      l := !l + qlen_char s.[pos];
      incr p;
      while !l < maxlen-2 && !p < String.length s do
	l := !l + qlen_char s.[!p];
	incr p;
      done;
      (* Now !l=maxlen-2 or !l=maxlen-1 or !p=String.length s *)
      let v = String.sub s pos (!p - pos) in
 
      ( [ Special ';'; Special ' '; Atom name; Special '='; QString v ] @
	continued_param n s !p (k+1)
      )
    end
  in

  let enclen_char c =
    match c with
	' ' | '*' | '\'' | '%' | '<' | '>' | '@' | ',' | ';' | ':' | '\\'
      | '.' | '/' | '\000'..'\031' | '\127'..'\255' -> 3
      | _ -> 1
  in

  let rec enclen s k =
    (* How long is [s] formatted as %-encoded string? *)
    if k >= String.length s then
      0
    else
      enclen_char (s.[k]) + enclen s (k+1)
  in

  let enc s =
    (* Perform %-encoding *)
    let l = String.length s in
    let l' = enclen s 0 in
    let s' = String.create l' in
    let k = ref 0 in
    for i = 0 to l-1 do
      if enclen_char s.[i] = 1 then begin
	s'.[ !k ] <- s.[i];
	incr k
      end 
      else begin
	let code = Char.code s.[i] in
	s'.[ !k ] <- '%';
	s'.[ !k+1 ] <- hex.( code asr 4 );
	s'.[ !k+2 ] <- hex.( code land 15 );
	k := !k + 3
      end
    done;
    s'
  in

  let rec continued_enc_param cs lang n s pos k =
    if pos >= String.length s then
      []
    else begin
      let name = n ^ "*" ^ string_of_int k ^ "*" in
      let designator = if k=0 then cs ^ "'" ^ lang ^ "'" else "" in
      let l = ref (String.length name + 2 + String.length designator) in
          (* 2 characters: semicolon, equal-to *)
      let p = ref pos in
      l := !l + enclen_char s.[pos];
      incr p;
      while !l < maxlen-3 && !p < String.length s do
	l := !l + enclen_char s.[!p];
	incr p;
      done;
      (* Now !l=maxlen-3 or !l=maxlen-2 or !l=maxlen-1 or !p=String.length s *)
      let v = String.sub s pos (!p - pos) in
 
      ( [ Special ';'; Special ' '; Atom name; Special '='; 
	  Atom (designator ^ enc v)
	] @
	continued_enc_param cs lang n s !p (k+1)
      )
    end
  in

  match pl with
      (n,P s) :: pl' ->
	let l = String.length n + qlen s 0 + 2 in
	if l < maxlen then
	  (Special ';' :: Special ' ' :: Atom n :: Special '=' :: QString s ::
	     param_tokens ~maxlen pl')
	else
	  continued_param n s 0 0 @ param_tokens ~maxlen pl'

    | (n,P_encoded(cs,lang,s)) :: pl' ->
	continued_enc_param cs lang n s 0 0 @ param_tokens ~maxlen pl'

    | [] ->
	[]
;;


let split_uri uri =
  let rec split k =
    if k >= String.length uri then
      []
    else
      let k' = min (k + 40) (String.length uri) in
      (QString (String.sub uri k (k' - k)) :: split k')
  in
  split 0
;;


let lf_re = S.regexp "[\n]";;


let read_multipart_body f boundary s =

  let rec search_window re start =
    try
      let i,r = S.search_forward re (Netbuffer.unsafe_buffer s#window) start in
      (* If match_end <= window_length, the search was successful.
       * Otherwise, we searched in the uninitialized region of the
       * buffer.
       *)
      if S.match_end r > s#window_length then raise Not_found;
      r
    with
	Not_found -> 
	  if s # window_at_eof then raise Not_found;
	  s#want_another_block();
	  search_window re start     (* try again with enlarged window *)
  in

  let search_end_of_line k =
    (* Search LF beginning at position k *)
    try
      S.match_end (search_window lf_re k)
    with
	Not_found ->
	    failwith "Mimestring.read_multipart_body: MIME boundary without line end";
  in

  let search_first_boundary() =
    (* Search boundary per regexp; return the position of the character
     * immediately following the boundary (on the same line), or
     * raise Not_found.
     *)
    let re = S.regexp ("\n--" ^ S.quote boundary) in
    S.match_end (search_window re 0)
  in

  let check_beginning_is_boundary() =
    let del = "--" ^ boundary in
    let ldel = String.length del in
    s # want ldel;
    let buf = Netbuffer.unsafe_buffer s#window in
    (s # window_length >= ldel) && (String.sub buf 0 ldel = del)
  in

  let rec go_to_eof stream =
    if stream#window_at_eof then
      stream#skip stream#window_length
    else begin
      stream#skip (stream#block_size);
      go_to_eof stream
    end
  in

  let rec parse_parts uses_crlf =
    (* PRE: [s] is at the beginning of the next part. 
     * [uses_crlf] must be true if CRLF is used as EOL sequence, and false
     *    if only LF is used as EOL sequence.
     *)
    let delimiter = (if uses_crlf then "\r" else "" ) ^ "\n--" ^ boundary in
    let sub_s = new Netstream.sub_stream ~delimiter s in
    let y = f sub_s in
    go_to_eof sub_s;
    (* Now the position of [s] is at the beginning of the delimiter. 
     * Check if there is a "--" after the delimiter (==> last part)
     *)
    let l_delimiter = String.length delimiter in
    s # want (l_delimiter+2);
    let buf = Netbuffer.unsafe_buffer s#window in
    let last_part = 
      (s#window_length >= (l_delimiter+2)) &&
      (buf.[l_delimiter] = '-') && 
      (buf.[l_delimiter+1] = '-') 
    in
    if last_part then begin
      go_to_eof s;
      [ y ]
    end
    else begin
      let k = search_end_of_line 2 in  (* [k]: Beginning of next part *)
      s # skip k;
      y :: parse_parts uses_crlf
    end
  in

  (* Check whether s directly begins with a boundary: *)
  if check_beginning_is_boundary() then begin
    (* Move to the beginning of the next line: *)
    let k_eol = search_end_of_line 0 in
    let uses_crlf = (Netbuffer.unsafe_buffer s#window).[k_eol-2] = '\r' in
    s # skip k_eol;
    (* Begin with first part: *)
    parse_parts uses_crlf
  end
  else begin
    (* Search the first boundary: *)
    try
      let k_eob = search_first_boundary() in   (* or Not_found *)
      (* Printf.printf "k_eob=%d\n" k_eob; *)
      (* Move to the beginning of the next line: *)
      let k_eol = search_end_of_line k_eob in
      let uses_crlf = (Netbuffer.unsafe_buffer s#window).[k_eol-2] = '\r' in
      (* Printf.printf "k_eol=%d\n" k_eol; *)
      s # skip k_eol;
      (* Begin with first part: *)
      parse_parts uses_crlf
    with
	Not_found ->
	  (* No boundary at all: The body is empty. *)
	  []
  end;
;;


let scan_multipart_body s ~start_pos ~end_pos ~boundary =
  let decode_part stream =
    let header = read_header stream in
    while not (stream # window_at_eof) do stream # want_another_block() done;
    let body = Netbuffer.sub stream#window 0 stream#window_length in
    header, body
  in

  let l_s = String.length s in
  if start_pos < 0 or end_pos < 0 or start_pos > l_s or end_pos >l_s then
    invalid_arg "Mimestring.scan_multipart_body";
  (* Set up a netstream beginning at ~start_pos and ending at ~end_pos: *)
  let len = end_pos - start_pos in
  let stream =
    new Netstream.input_stream 
      (new Netchannels.input_string ~pos:start_pos ~len s) in
  (* Note: We would save a copy of the string if there was a class combining
   * input_stream and input_string 
   *)
  (* read the multipart body: *)
  read_multipart_body decode_part boundary stream
;;
  

let scan_multipart_body_and_decode s ~start_pos:i0 ~end_pos:i1 ~boundary =
  let parts = scan_multipart_body s i0 i1 boundary in
  List.map
    (fun (params, value) ->
       let encoding =
	 try List.assoc "content-transfer-encoding" params
	 with Not_found -> "7bit"
       in

       (* NOTE: In the case of "base64" and "quoted-printable", the allocation
	* of the string "value" could be avoided.
	*)

       let value' =
	 match encoding with
	     ("7bit"|"8bit"|"binary") -> value
	   | "base64" ->
	       Netencoding.Base64.decode 
	         ~url_variant:false ~accept_spaces:true value
	   | "quoted-printable" ->
	       Netencoding.QuotedPrintable.decode
		 value 
	   | _ ->
	       failwith "Mimestring.scan_multipart_body_and_decode: Unknown content-transfer-encoding"
       in
       (params, value')
    )
    parts
;;


let scan_multipart_body_from_netstream s ~boundary ~create ~add ~stop =
  let decode_part stream =
    let header = read_header stream in
    let p = create header in
    try
      while stream#window_length > 0 do
	let l = stream#window_length in
	add p stream 0 l;
	stream # skip l
      done;
      stop p;
      ()
    with
	error ->
	  stop p; raise error
  in

  (* read the multipart body: *)
  let _ = read_multipart_body decode_part boundary s in
  ()
;;


let create_boundary ?(random = []) ?(nr = 0) () =
  let (x1,x2,x3) = Gc.counters() in
  let magic = 
    ref(Printf.sprintf "%.0f,%.0f,%.0f" x1 x2 x3) in
  List.iter
    (fun s ->
       let l = min 256 (String.length s) in
       magic := !magic ^ (Digest.substring s 0 l)
    )
    random;
  magic := Digest.string !magic;
  let hex = 
    Printf.sprintf "%02X%02X%02X%02X%02X%02X%02X%02X"
      (Char.code !magic.[0])
      (Char.code !magic.[1])
      (Char.code !magic.[2])
      (Char.code !magic.[3])
      (Char.code !magic.[4])
      (Char.code !magic.[5])
      (Char.code !magic.[6])
      (Char.code !magic.[7])   (* The other 8 bytes are ignored *)
  in
  Printf.sprintf "------------------------------=__%s<&>;%010d"
    hex
    nr
;;
