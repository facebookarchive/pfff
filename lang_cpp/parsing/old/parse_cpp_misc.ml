
val tokens_string: string -> Parser_cpp.token list


val parse_simple:                 Common.filename -> Ast_cpp.program
val parse_print_error:            Common.filename -> Ast_cpp.program
val parse_gen: 
    ((Lexing.lexbuf -> Parser_cpp.token) -> Lexing.lexbuf -> 'a) -> string -> 'a

(* ---------------------------------------------------------------------- *)
(* Easy way to build complex Ast elements from simple strings.
 * Can also be useful when called from the ocaml toplevel to test. 
 *)
val type_of_string      : string -> Ast_cpp.fullType
val statement_of_string : string -> Ast_cpp.statement

(* return fake program but with good tokens. just use the lexer *)
val parse_tokens:
  Common.filename -> (program2 * Statistics_parsing.parsing_stat)



let tokens_string string = 
  let lexbuf = Lexing.from_string string in
  try 
    let rec tokens_s_aux () = 
      let tok = Lexer.token lexbuf in
      if TH.is_eof tok
      then [tok]
      else tok::(tokens_s_aux ())
    in
    tokens_s_aux ()
  with
    | Lexer.Lexical s -> failwith ("lexical error " ^ s ^ "\n =" )
    | e -> raise e



let parse_tokens2 filename =

  let stat = Stat.default_stat filename in

  let toks_orig = tokens filename in
  let toks = Parsing_hacks.fix_tokens_define toks_orig in

  (* TODO *)
  [Ast.NotParsedCorrectly [], ("", toks)], stat

let parse_tokens a = 
  Common.profile_code "Parse_cpp.parse_tokens" (fun () -> parse_tokens2 a)


(*****************************************************************************)
(* Parsing subelements, useful to debug parser *)
(*****************************************************************************)

(*
 * !!!Those function use refs, and are not reentrant !!! so take care.
 * It use globals defined in Lexer_parser.
 *)


(* old: 
 *   let parse_gen parsefunc s = 
 *     let lexbuf = Lexing.from_string s in
 *     let result = parsefunc Lexer.token lexbuf in
 *     result
 *)

let parse_gen parsefunc s = 
  let toks = tokens_string s +> List.filter TH.is_not_comment in


  (* Why use this lexing scheme ? Why not classically give lexer func
   * to parser ? Because I now keep comments in lexer. Could 
   * just do a simple wrapper that when comment ask again for a token,
   * but maybe simpler to use cur_tok technique.
   *)
  let all_tokens = ref toks in
  let cur_tok    = ref (List.hd !all_tokens) in

  let lexer_function = 
    (fun _ -> 
      if TH.is_eof !cur_tok
      then (pr2 "LEXER: ALREADY AT END"; !cur_tok)
      else
        let v = Common.pop2 all_tokens in
        cur_tok := v;
        !cur_tok
    ) 
  in
  let lexbuf_fake = Lexing.from_function (fun buf n -> raise Impossible) in
  let result = parsefunc lexer_function lexbuf_fake in
  result


let type_of_string       = parse_gen Parser.type_id
let statement_of_string  = parse_gen Parser.statement
let expression_of_string = parse_gen Parser.expr

(* ex: statement_of_string "(struct us_data* )psh->hostdata = NULL;" *)




val print_commentized       : Parser_cpp.token list -> unit


let print_commentized xs = 
  let line = ref (-1) in
  begin
    let ys = commentized xs in
    ys +>
    List.iter
      (function
      | Parse_info.OriginTok pinfo 
      | Parse_info.ExpandedTok (_,pinfo,_) -> 

          let newline = pinfo.Parse_info.line in
          let s = pinfo.Parse_info.str in

          let s = Str.global_substitute 
            (Str.regexp "\n") (fun s -> "") s 
          in
            if newline = !line
            then prerr_string (s ^ " ")
            else begin
              if !line = -1 
              then pr2_no_nl "passed:" 
              else pr2_no_nl "\npassed:";
              line := newline;
              pr2_no_nl (s ^ " ");
            end
        | _ -> ());
    if not (null ys) then pr2 "";
  end



(*****************************************************************************)
(* Parsing, but very basic, no more used *)
(*****************************************************************************)

(*
 * !!!Those function use refs, and are not reentrant !!! so take care.
 * It use globals defined in Lexer_parser.
 * 
 * update: because now lexer return comments tokens, those functions
 * may not work anymore.
 *)

let parse_simple file = 
  let lexbuf = Lexing.from_channel (open_in file) in
  let result = Parser.main Lexer.token lexbuf in
  result


let parse_print_error file = 
  let chan = (open_in file) in
  let lexbuf = Lexing.from_channel chan in

  let error_msg () = Parse_info.error_message file (lexbuf_to_strpos lexbuf) in
  try 
    lexbuf +> Parser.main Lexer.token
  with 
  | Lexer.Lexical s ->   
      failwith ("lexical error " ^s^ "\n =" ^  error_msg ())
  | Parsing.Parse_error -> 
      failwith ("parse error \n = " ^ error_msg ())
  | Semantic.Semantic (s, i) -> 
      failwith ("semantic error " ^ s ^ "\n =" ^ error_msg ())
  | e -> raise e


(*****************************************************************************)
(* C vs C++ file disambiguator *)
(*****************************************************************************)

val threshold_cplusplus : int ref
(* when have a .h, want to know if have to use C or C++ parser *)
val is_problably_cplusplus_file: Common.filename -> bool



let threshold_cplusplus = ref 5 

let verbose_problably = ref false
let is_problably_cplusplus_file file =
  let toks = tokens file in
  let _length_orig = List.length toks in

  let toks_cplusplus = 
    toks +> List.filter TH.is_cpp_keyword in
  let toks_really_cplusplus = 
    toks +> List.filter TH.is_really_cpp_keyword in
  let toks_cplusplus_no_fp = 
    toks +> List.filter (fun x -> 
      TH.is_cpp_keyword x && not (TH.is_maybenot_cpp_keyword x)
    )
  in

  let toks_cplusplus_n = List.length toks_cplusplus in
  let toks_really_cplusplus_n = List.length toks_really_cplusplus in
  let toks_cplusplus_no_fp_n = List.length toks_cplusplus_no_fp in

  if !verbose_problably then begin
    pr2_gen toks_cplusplus;
    pr2_gen toks_really_cplusplus;
    pr2_gen toks_cplusplus_no_fp;
  end;
  

  if toks_really_cplusplus_n > 0
  then true 
  else 
    if toks_cplusplus_no_fp_n > 20 
    then 
      true 
    else toks_cplusplus_n >= !threshold_cplusplus


val parse_print_error_heuristic:  
  Common.filename (*cfile*) -> (program2 * Statistics_parsing.parsing_stat)
val parse_c_and_cpp : (* alias of previous func *)
  Common.filename (*cfile*) -> (program2 * Statistics_parsing.parsing_stat)
