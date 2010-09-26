(* Copyright (C) 2008 Yoann Padioleau *)

open Common 

module TH = Token_helpers_java

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
  then Common.error_message file (token_to_strpos tok) 
  else ("error in " ^ file  ^ "set verbose_parsing for more info")


(*****************************************************************************)
(* Stat *)
(*****************************************************************************)
type parsing_stat = {
  mutable correct: int;
  mutable bad: int;
}

let print_parsing_stat_list statxs =
  let total = List.length statxs in
  let perfect = 
    statxs 
      +> List.filter (function 
      | {bad = n} when n = 0 -> true 
      | _ -> false)
      +> List.length 
  in

  pr "\n\n\n---------------------------------------------------------------";
  pr (
  (sprintf "NB total files = %d; " total) ^
  (sprintf "perfect = %d; " perfect) ^
  (sprintf "=========> %d" ((100 * perfect) / total)) ^ "%"
  );

  let good = statxs +> List.fold_left (fun acc {correct = x} -> acc+x) 0 in
  let bad  = statxs +> List.fold_left (fun acc {bad = x} -> acc+x) 0  in

  let gf, badf = float_of_int good, float_of_int bad in
  pr (
  (sprintf "nb good = %d,  nb bad = %d " good bad) ^
  (sprintf "=========> %f"  (100.0 *. (gf /. (gf +. badf))) ^ "%"
   )
  )


(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)


let tokens2 file = 
 let table     = Common.full_charpos_to_pos file in

 Common.with_open_infile file (fun chan -> 
  let lexbuf = Lexing.from_channel chan in
  try 
    let rec tokens_aux () = 
      let tok = Lexer_java.token lexbuf in


      (* fill in the line and col information *)
      let tok = tok +> TH.visitor_info_of_tok (fun ii -> 
        let pi = ii.Ast_java.pinfo in
        { ii with Ast_java.pinfo=
          (* could assert pinfo.filename = file ? *)
          
            (Common.complete_parse_info file table pi)
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
                  (Common.error_message file (lexbuf_to_strpos lexbuf)))
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
          { correct = (Common.cat filename +> List.length); bad = 0 }
        in
	Printf.eprintf "%s: OK\n" (Lexer_helper.location ());
        (Left ast, toks), stat
      )
  with e -> 
    error (Printexc.to_string e);
    let toks = [] in (* TODO *)
    let stat = { correct = 0; bad = (Common.cat filename +> List.length); } in
    (Right (), toks), stat




let parse_java filename =
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

    let stat = 
      { correct = (Common.cat filename +> List.length); bad = 0 }
    in
    Printf.eprintf "%s: OK\n" (Lexer_helper.location ());
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
    let stat = { correct = 0; bad = (Common.cat filename +> List.length); } in

    let info_of_bads = Common.map_eff_rev TH.info_of_tok toks_orig in 

    (Right info_of_bads, toks_orig), stat
  end

