(* Copyright (C) 2008 Yoann Padioleau *)

open Common 

module TH = Token_helpers_java

module PI = Parse_info

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let lexbuf_to_strpos lexbuf     = 
  (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)    

let token_to_strpos tok = 
  (TH.str_of_tok tok, TH.pos_of_tok tok)

let error_msg_tok tok = 
  let file = TH.file_of_tok tok in
  if !Flag_parsing_java.verbose_parsing
  then Parse_info.error_message file (token_to_strpos tok) 
  else ("error in " ^ file  ^ "set verbose_parsing for more info")


(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file = 
 let table     = Parse_info.full_charpos_to_pos_large file in

 Common.with_open_infile file (fun chan -> 
  let lexbuf = Lexing.from_channel chan in
  try 
    let rec tokens_aux () = 
      let tok = Lexer_java.token lexbuf in


      (* fill in the line and col information *)
      let tok = tok +> TH.visitor_info_of_tok (fun ii -> 
        { ii with PI.token=
          (* could assert pinfo.filename = file ? *)
           match PI.pinfo_of_info ii with
           | PI.OriginTok pi ->
               PI.OriginTok 
                 (PI.complete_parse_info_large file table pi)
           | _ -> raise Todo
        })

      in

      if TH.is_eof tok
      then [tok]
      else tok::(tokens_aux ())
    in
    tokens_aux ()
  with
    | Lexer_java.Lexical s -> 
        failwith ("lexical error " ^ s ^ "\n =" ^ 
                  (PI.error_message file (lexbuf_to_strpos lexbuf)))
    | e -> raise e
 )

let tokens a = 
  Common.profile_code "Java parsing.tokens" (fun () -> tokens2 a)


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

type info_item = (Parser_java.token list)
type program2 = 
  (Ast_java.compilation_unit, Ast_java.info list) Common.either * info_item



let error msg =
  Printf.eprintf "%s: %s\n" (Lexer_helper.location ()) msg

let parse_java_old filename =
  Lexer_helper.set_file_name filename;
  try
    Lexer_helper.with_lexbuf
      (fun lexbuf ->
	let ast = Parser_java.goal Lexer_java.token lexbuf in
        let toks = [] in (* TODO *)

        let stat = 
          { PI.correct = (Common.cat filename +> List.length); 
            PI.bad = 0;
            PI.filename = filename;
          }
        in
	Printf.eprintf "%s: OK\n" (Lexer_helper.location ());
        (Left ast, toks), stat
      )
  with e -> 
    error (Printexc.to_string e);
    let toks = [] in (* TODO *)
    let stat = { PI.correct = 0; 
                 bad = (Common.cat filename +> List.length); 
                 PI.filename = filename;
    } 
    in
    (Right (), toks), stat




let parse_java filename =
  let stat = Parse_info.default_stat filename in

  let toks_orig = tokens filename in

  let toks = toks_orig +> Common.exclude TH.is_comment in

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

  Lexer_helper.set_file_name filename;

  try (
    let ast = Parser_java.goal lexer_function lexbuf_fake in
    stat.PI.correct <- Common.cat filename +> List.length;
    (* Printf.eprintf "%s: OK\n" (Lexer_helper.location ()); *)
    (Left ast, toks_orig), stat
  )
  with e -> begin
    (match e with
    (* Lexical is not anymore launched I think *)
    | Lexer_java.Lexical s -> 
        pr2 ("lexical error " ^s^ "\n =" ^ error_msg_tok !cur_tok)
    | Parsing.Parse_error -> 
        pr2 ("parse error \n = " ^ error_msg_tok !cur_tok)
(*
    | Semantic_java.Semantic (s, i) -> 
        pr2 ("semantic error " ^s^ "\n ="^ error_msg_tok tr.current)
*)
    | e -> raise e
    );
    error (Printexc.to_string e);
    stat.PI.bad <- Common.cat filename +> List.length;
    let info_of_bads = Common.map_eff_rev TH.info_of_tok toks_orig in 

    (Right info_of_bads, toks_orig), stat
  end

