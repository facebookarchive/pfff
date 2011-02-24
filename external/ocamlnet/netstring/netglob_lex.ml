# 3 "netglob_lex.mll"
 
  exception Bracket_Unsupported
  exception Lexing_Error

  type bracket_token =
      Bracket_char of char
    | Bracket_range of (char * char)
    | Bracket_code of int  (* see Netglob.reparse_bracket_expr *)
    | Bracket_end

  type brace_token =
      Brace_literal of string
    | Brace_comma
    | Brace_braces of brace_token list  (* inner braces *)
    | Brace_end

  type glob_features =
      { enable_star : bool;
	enable_qmark : bool;
	enable_brackets : bool;
	enable_braces : bool;
	enable_tilde : bool;
	enable_escape : bool;
	mutable escaped : bool;  (* after a backslash *)
      }

  type glob_token =
      Glob_literal of string
    | Glob_star
    | Glob_qmark
    | Glob_brackets of (bool * bracket_token list)
    | Glob_braces of brace_token list
    | Glob_tilde of string * bool (* whether there is a slash *)
    | Glob_end

  type exploded_char =
      C of char   (* An unescaped character *)
    | E of char   (* An escaped character *)
    | Delim of char  (* delimiter *)



  let rec collect_until end_token parse_fun lexbuf =
    let tok = parse_fun lexbuf in
    if tok = end_token then
      []
    else
      tok :: (collect_until end_token parse_fun lexbuf)


  let string_of_exploded l =
    String.concat "" 
      (List.map
	 (function 
	    | C c -> String.make 1 c
	    | E c -> String.make 1 c
	    | Delim _ -> ""
	 )
	 l
      )

  let have_delim l =
    List.exists (function Delim _ -> true | _ -> false) l


# 68 "netglob_lex.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\250\255\000\000\252\255\001\000\001\000\002\000\003\000\
    \006\000\000\000\002\000\255\255\002\000\003\000\254\255\000\000\
    \004\000\253\255\251\255\007\000\251\255\005\000\007\000\008\000\
    \005\000\009\000\011\000\002\000\006\000\255\255\005\000\007\000\
    \254\255\006\000\009\000\253\255\252\255\012\000\250\255\251\255\
    \004\000\253\255\254\255\255\255\252\255\013\000\251\255\252\255\
    \253\255\254\255\255\255\029\000\248\255\040\000\250\255\251\255\
    \252\255\011\000\254\255\255\255\253\255\014\000\253\255\254\255\
    \010\000\255\255\015\000\254\255\255\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\006\000\255\255\006\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\005\000\005\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \006\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\006\000\255\255\255\255\
    \255\255\002\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \001\000\255\255\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\002\000\000\000\255\255\000\000\255\255\018\000\015\000\012\000\
    \009\000\255\255\255\255\000\000\255\255\255\255\000\000\255\255\
    \255\255\000\000\000\000\021\000\000\000\255\255\255\255\036\000\
    \033\000\030\000\027\000\255\255\255\255\000\000\255\255\255\255\
    \000\000\255\255\255\255\000\000\000\000\039\000\000\000\000\000\
    \044\000\000\000\000\000\000\000\000\000\047\000\000\000\000\000\
    \000\000\000\000\000\000\053\000\000\000\053\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\063\000\000\000\000\000\
    \065\000\000\000\068\000\000\000\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\060\000\005\000\005\000\007\000\
    \013\000\255\255\023\000\031\000\023\000\025\000\000\000\255\255\
    \042\000\049\000\010\000\008\000\028\000\016\000\006\000\255\255\
    \255\255\026\000\255\255\034\000\024\000\255\255\000\000\059\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\004\000\058\000\003\000\255\255\011\000\
    \014\000\017\000\022\000\029\000\032\000\255\255\035\000\255\255\
    \040\000\060\000\064\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \057\000\054\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\000\000\000\000\041\000\
    \048\000\043\000\050\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \056\000\000\000\000\000\055\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\255\255\255\255\255\255\255\255\255\255\255\255\020\000\
    \255\255\255\255\255\255\255\255\038\000\046\000\062\000\067\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\052\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\057\000\002\000\004\000\004\000\
    \012\000\007\000\021\000\030\000\022\000\022\000\255\255\025\000\
    \037\000\045\000\009\000\004\000\027\000\015\000\004\000\006\000\
    \008\000\022\000\024\000\033\000\022\000\026\000\255\255\051\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\053\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\051\000\000\000\005\000\010\000\
    \013\000\016\000\019\000\028\000\031\000\023\000\034\000\053\000\
    \037\000\057\000\061\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \051\000\051\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\053\000\053\000\255\255\255\255\037\000\
    \045\000\037\000\045\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \051\000\255\255\255\255\051\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\053\000\255\255\255\255\053\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\005\000\006\000\007\000\040\000\024\000\008\000\019\000\
    \023\000\025\000\064\000\026\000\037\000\045\000\061\000\066\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\051\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \053\000";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec bracket_rest lexbuf =
    __ocaml_lex_bracket_rest_rec lexbuf 0
and __ocaml_lex_bracket_rest_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 74 "netglob_lex.mll"
                       ( raise Bracket_Unsupported )
# 199 "netglob_lex.ml"

  | 1 ->
# 75 "netglob_lex.mll"
                       ( raise Bracket_Unsupported )
# 204 "netglob_lex.ml"

  | 2 ->
# 76 "netglob_lex.mll"
                       ( raise Bracket_Unsupported )
# 209 "netglob_lex.ml"

  | 3 ->
# 77 "netglob_lex.mll"
                       ( Bracket_end )
# 214 "netglob_lex.ml"

  | 4 ->
# 79 "netglob_lex.mll"
                       ( let c0 = Lexing.lexeme_char lexbuf 0 in
			 let c1 = Lexing.lexeme_char lexbuf 2 in
			 if c0 > '\127' || c1 > '\127' then raise Lexing_Error;
			 if c0 > c1 then raise Lexing_Error;
			 Bracket_range(c0,c1)
		       )
# 224 "netglob_lex.ml"

  | 5 ->
# 85 "netglob_lex.mll"
                       ( raise Lexing_Error )
# 229 "netglob_lex.ml"

  | 6 ->
# 86 "netglob_lex.mll"
                       ( Bracket_char (Lexing.lexeme_char lexbuf 0) )
# 234 "netglob_lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_bracket_rest_rec lexbuf __ocaml_lex_state

and bracket_first lexbuf =
    __ocaml_lex_bracket_first_rec lexbuf 19
and __ocaml_lex_bracket_first_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 94 "netglob_lex.mll"
                       ( raise Bracket_Unsupported )
# 245 "netglob_lex.ml"

  | 1 ->
# 95 "netglob_lex.mll"
                       ( raise Bracket_Unsupported )
# 250 "netglob_lex.ml"

  | 2 ->
# 96 "netglob_lex.mll"
                       ( raise Bracket_Unsupported )
# 255 "netglob_lex.ml"

  | 3 ->
# 97 "netglob_lex.mll"
                       ( let c0 = Lexing.lexeme_char lexbuf 0 in
			 let c1 = Lexing.lexeme_char lexbuf 2 in
			 if c0 > '\127' || c1 > '\127' then raise Lexing_Error;
			 if c0 > c1 then raise Lexing_Error;
			 Bracket_range(c0,c1)
		       )
# 265 "netglob_lex.ml"

  | 4 ->
# 103 "netglob_lex.mll"
                       ( raise Lexing_Error )
# 270 "netglob_lex.ml"

  | 5 ->
# 104 "netglob_lex.mll"
                       ( Bracket_char (Lexing.lexeme_char lexbuf 0) )
# 275 "netglob_lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_bracket_first_rec lexbuf __ocaml_lex_state

and brace lexbuf =
    __ocaml_lex_brace_rec lexbuf 37
and __ocaml_lex_brace_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 112 "netglob_lex.mll"
                       ( Brace_end )
# 286 "netglob_lex.ml"

  | 1 ->
# 113 "netglob_lex.mll"
                       ( Brace_comma )
# 291 "netglob_lex.ml"

  | 2 ->
# 114 "netglob_lex.mll"
                       ( let l = collect_until Brace_end brace lexbuf in
			 Brace_braces l )
# 297 "netglob_lex.ml"

  | 3 ->
# 116 "netglob_lex.mll"
                       ( Brace_literal (Lexing.lexeme lexbuf) )
# 302 "netglob_lex.ml"

  | 4 ->
# 117 "netglob_lex.mll"
                           ( Brace_literal (Lexing.lexeme lexbuf) )
# 307 "netglob_lex.ml"

  | 5 ->
# 118 "netglob_lex.mll"
                       ( raise Lexing_Error )
# 312 "netglob_lex.ml"

  | 6 ->
# 119 "netglob_lex.mll"
                       ( raise Lexing_Error )
# 317 "netglob_lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_brace_rec lexbuf __ocaml_lex_state

and brace_noescape lexbuf =
    __ocaml_lex_brace_noescape_rec lexbuf 45
and __ocaml_lex_brace_noescape_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 126 "netglob_lex.mll"
                       ( Brace_end )
# 328 "netglob_lex.ml"

  | 1 ->
# 127 "netglob_lex.mll"
                       ( Brace_comma )
# 333 "netglob_lex.ml"

  | 2 ->
# 128 "netglob_lex.mll"
                       ( let l = collect_until Brace_end brace_noescape lexbuf in
			 Brace_braces l )
# 339 "netglob_lex.ml"

  | 3 ->
# 130 "netglob_lex.mll"
                       ( Brace_literal (Lexing.lexeme lexbuf) )
# 344 "netglob_lex.ml"

  | 4 ->
# 131 "netglob_lex.mll"
                       ( raise Lexing_Error )
# 349 "netglob_lex.ml"

  | 5 ->
# 132 "netglob_lex.mll"
                       ( raise Lexing_Error )
# 354 "netglob_lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_brace_noescape_rec lexbuf __ocaml_lex_state

and glob_expr feat lexbuf =
    __ocaml_lex_glob_expr_rec feat lexbuf 51
and __ocaml_lex_glob_expr_rec feat lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 135 "netglob_lex.mll"
                       ( if feat.enable_star && not feat.escaped then 
			   Glob_star
			 else (
			   feat.escaped <- false;
			   Glob_literal "*"
			 )
		       )
# 371 "netglob_lex.ml"

  | 1 ->
# 142 "netglob_lex.mll"
                       ( if feat.enable_qmark && not feat.escaped then 
			   Glob_qmark
			 else (
			   feat.escaped <- false;
			   Glob_literal "?"
			 )
		       )
# 382 "netglob_lex.ml"

  | 2 ->
# 149 "netglob_lex.mll"
                       ( if feat.enable_brackets && not feat.escaped then (
			   let negated = 
			     String.length(Lexing.lexeme lexbuf) > 1 in
			   let t0 = bracket_first lexbuf in
			   let l = collect_until 
				     Bracket_end bracket_rest lexbuf in
			   Glob_brackets (negated, t0 :: l)
			 )
			 else (
			   feat.escaped <- false;
			   Glob_literal (Lexing.lexeme lexbuf)
			 )
		       )
# 399 "netglob_lex.ml"

  | 3 ->
# 162 "netglob_lex.mll"
                       ( if feat.enable_braces && not feat.escaped then (
			   let p =
			     if feat.enable_escape then
			       brace
			     else
			       brace_noescape in
			   let l = collect_until Brace_end p lexbuf in
			   Glob_braces l
			 )
			 else (
			   feat.escaped <- false;
			   Glob_literal "{"
			 )
		       )
# 417 "netglob_lex.ml"

  | 4 ->
# 176 "netglob_lex.mll"
                       ( if (feat.enable_tilde && not feat.escaped && 
                             Lexing.lexeme_start lexbuf = 0) then (
			   let p =
			     if feat.enable_escape then
			       generic_lex_until '/'
			     else
			       generic_lex_noescape_until '/' in
			   let l = p lexbuf in
			   let s = string_of_exploded l in
			   let slash = have_delim l in
			   Glob_tilde(s,slash)
			 ) else (
			   feat.escaped <- false;
			   Glob_literal "~"
			 )
                       )
# 437 "netglob_lex.ml"

  | 5 ->
# 192 "netglob_lex.mll"
                       ( if feat.enable_escape && not feat.escaped then (
			   feat.escaped <- true;
			   Glob_literal ""
			 )
			 else (
			   feat.escaped <- false;
			   Glob_literal "\\"
			 )
		       )
# 450 "netglob_lex.ml"

  | 6 ->
# 202 "netglob_lex.mll"
                       ( feat.escaped <- false;
			 Glob_literal (Lexing.lexeme lexbuf)
		       )
# 457 "netglob_lex.ml"

  | 7 ->
# 205 "netglob_lex.mll"
                       ( if feat.escaped then raise Lexing_Error;
			 Glob_end
		       )
# 464 "netglob_lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_glob_expr_rec feat lexbuf __ocaml_lex_state

and generic_lex_until c lexbuf =
    __ocaml_lex_generic_lex_until_rec c lexbuf 61
and __ocaml_lex_generic_lex_until_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 210 "netglob_lex.mll"
                       ( let char = E (Lexing.lexeme_char lexbuf 1) in
                         char :: generic_lex_until c lexbuf )
# 476 "netglob_lex.ml"

  | 1 ->
# 212 "netglob_lex.mll"
                       ( let lc = Lexing.lexeme_char lexbuf 0 in
			 if c = lc then [ Delim c ] else (
                           let char = C lc in
                           char :: generic_lex_until c lexbuf
			 ) )
# 485 "netglob_lex.ml"

  | 2 ->
# 217 "netglob_lex.mll"
                       ( [] )
# 490 "netglob_lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_generic_lex_until_rec c lexbuf __ocaml_lex_state

and generic_lex_noescape_until c lexbuf =
    __ocaml_lex_generic_lex_noescape_until_rec c lexbuf 66
and __ocaml_lex_generic_lex_noescape_until_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 220 "netglob_lex.mll"
                       ( let lc = Lexing.lexeme_char lexbuf 0 in
			 if c = lc then [ Delim c ] else (
                           let char = C lc in
                           char :: generic_lex_noescape_until c lexbuf
			 ) )
# 505 "netglob_lex.ml"

  | 1 ->
# 225 "netglob_lex.mll"
                       ( [] )
# 510 "netglob_lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_generic_lex_noescape_until_rec c lexbuf __ocaml_lex_state

;;

