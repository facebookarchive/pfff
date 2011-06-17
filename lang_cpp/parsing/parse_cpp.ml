(* Yoann Padioleau
 *
 * Copyright (C) 2002-2011 Yoann Padioleau
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)

open Common

module Flag = Flag_parsing_cpp
module Ast = Ast_cpp

module TH = Token_helpers_cpp
module LP = Lexer_parser_cpp

module Parser = Parser_cpp
module Lexer = Lexer_cpp
module Semantic = Semantic_cpp

module PI = Parse_info
module Stat = Statistics_parsing

module Hack = Parsing_hacks_lib

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * A heuristic based C/C++/CPP parser.
 * 
 * See "Parsing C/C++ Code without Pre-Preprocessing - Yoann Padioleau, CC'09"
 * avalaible at http://padator.org/papers/yacfe-cc09.pdf for more
 * information.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type program2 = toplevel2 list
     and toplevel2 = Ast.toplevel * info_item
      and info_item =  string * Parser.token list

let program_of_program2 xs = 
  xs +> List.map fst

let with_program2 f program2 = 
  program2 
  +> Common.unzip 
  +> (fun (program, infos) -> f program, infos)
  +> Common.uncurry Common.zip

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag_parsing_cpp.verbose_parsing

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let lexbuf_to_strpos lexbuf     = 
  (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)    

let token_to_strpos tok = 
  (TH.str_of_tok tok, TH.pos_of_tok tok)

let mk_info_item2 filename toks = 
  let s = 
    Common.with_open_stringbuf (fun (_pr, buf) ->
      toks +> List.iter (fun tok -> 
        match TH.pinfo_of_tok tok with
        | PI.OriginTok _ -> Buffer.add_string buf (TH.str_of_tok tok)
        | PI.ExpandedTok _ | PI.FakeTokStr _ -> ()
        | PI.Ab -> raise Impossible
      );
    )
  in
  (s, toks) 
let mk_info_item a b = 
  Common.profile_code "Parse_cpp.mk_info_item"  (fun () -> mk_info_item2 a b)

(*****************************************************************************)
(* Error diagnostic *)
(*****************************************************************************)

let error_msg_tok tok = 
  Parse_info.error_message_info (TH.info_of_tok tok)

let print_bad line_error (start_line, end_line) filelines  = 
  begin
    pr2 ("badcount: " ^ i_to_s (end_line - start_line));

    for i = start_line to end_line do 
      let line = filelines.(i) in 

      if i = line_error 
      then  pr2 ("BAD:!!!!!" ^ " " ^ line) 
      else  pr2 ("bad:" ^ " " ^      line) 
    done
  end

(*****************************************************************************)
(* Stats on what was passed/commentized  *)
(*****************************************************************************)

let commentized xs = xs +> Common.map_filter (function
  | Parser.TComment_Cpp (cppkind, ii) -> 
      if !Flag.filter_classic_passed
      then 
        (match cppkind with
        | Token_cpp.CppOther -> 
            let s = Ast.str_of_info ii in
            (match s with
            | s when s =~ "KERN_.*" -> None
            | s when s =~ "__.*" -> None
            | _ -> Some (ii.PI.token)
            )
             
        | Token_cpp.CppDirective | Token_cpp.CppAttr | Token_cpp.CppMacro
            -> None
        | _ -> raise Todo
        )
      else Some (ii.PI.token)
      
  | Parser.TAny_Action ii ->
      Some (ii.PI.token)
  | _ -> 
      None
 )
  
let count_lines_commentized xs = 
  let line = ref (-1) in
  let count = ref 0 in
  begin
    commentized xs +>
    List.iter
      (function
      | PI.OriginTok pinfo 
      | PI.ExpandedTok (_,pinfo,_) -> 
          let newline = pinfo.PI.line in
          if newline <> !line
          then begin
            line := newline;
            incr count
          end
      | _ -> ());
    !count
  end

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

(* called by parse below *)
let tokens2 file = 
 let table     = Parse_info.full_charpos_to_pos file in

 Common.with_open_infile file (fun chan -> 
  let lexbuf = Lexing.from_channel chan in
  try 
    let rec tokens_aux () = 
      let tok = Lexer.token lexbuf in
      (* fill in the line and col information *)
      let tok = tok +> TH.visitor_info_of_tok (fun ii -> 
        { ii with PI.token=
          (* could assert pinfo.filename = file ? *)
          match Parse_info.pinfo_of_info ii with
          |  PI.OriginTok pi ->
             PI.OriginTok (Parse_info.complete_parse_info file table pi)
          | PI.ExpandedTok (pi,vpi, off) ->
              PI.ExpandedTok(
                (Parse_info.complete_parse_info file table pi),vpi, off)
          | PI.FakeTokStr (s,vpi_opt) -> PI.FakeTokStr (s,vpi_opt)
          | PI.Ab -> raise Impossible
      })
      in

      if TH.is_eof tok
      then [tok]
      else tok::(tokens_aux ())
    in
    tokens_aux ()
  with
    | Lexer.Lexical s -> 
        failwith ("lexical error " ^ s ^ "\n =" ^ 
                  (Parse_info.error_message file (lexbuf_to_strpos lexbuf)))
    | e -> raise e
 )

let tokens a = 
  Common.profile_code "Parse_cpp.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Extract macros *)
(*****************************************************************************)

(* It can be used to to parse the macros defined in a macro.h file. It 
 * can also be used to try to extract the macros defined in the file 
 * that we try to parse *)
let extract_macros2 file = 
  Common.save_excursion Flag_parsing_cpp.verbose_lexing false (fun () -> 
    let toks = tokens (* todo: ~profile:false *) file in
    let toks = Parsing_hacks_define.fix_tokens_define toks in
    Pp_token.extract_macros toks
  )
let extract_macros a = 
  Common.profile_code_exclusif "Parse_cpp.extract_macros" (fun () -> 
    extract_macros2 a)

(*****************************************************************************)
(* Error recovery *)
(*****************************************************************************)

(* see parsing_recovery_cpp.ml *)

(*****************************************************************************)
(* Consistency checking *)
(*****************************************************************************)

(* see parsing_consistency_cpp.ml *)
      
(*****************************************************************************)
(* Helper for main entry point *)
(*****************************************************************************)

(* 
 * The use of local refs (remaining_tokens, passed_tokens, ...) makes
 * possible error recovery. Indeed, they allow to skip some tokens and
 * still be able to call again the ocamlyacc parser. It is ugly code
 * because we cant modify ocamllex and ocamlyacc. As we want some
 * extended lexing tricks, we have to use such refs.
 * 
 * Those refs are now also used for my lalr(k) technique. Indeed They
 * store the futur and previous tokens that were parsed, and so
 * provide enough context information for powerful lex trick.
 * 
 * - passed_tokens_last_ckp stores the passed tokens since last
 *   checkpoint. Used for NotParsedCorrectly and also to build the
 *   info_item attached to each program_element.
 * - passed_tokens_clean is used for lookahead, in fact for lookback.
 * - remaining_tokens_clean is used for lookahead. Now remaining_tokens
 *   contain some comments and so would make pattern matching difficult
 *   in lookahead. Hence this variable. We would like also to get rid 
 *   of cpp instruction because sometimes a cpp instruction is between
 *   two tokens and makes a pattern matching fail. But lookahead also
 *   transform some cpp instruction (in comment) so can't remove them.
 *   c++ext: update, want also get rid of useless nested_qualifier which
 *   do not help to infer typedef (as they can be used to qualify a type
 *   or variable).
 * 
 * So remaining_tokens, passed_tokens_last_ckp contain comment-tokens,
 * whereas passed_tokens_clean and remaining_tokens_clean does not contain
 * comment-tokens.
 * 
 * Normally we have:
 * toks = (reverse passed_tok) ++ cur_tok ++ remaining_tokens   
 *    after the call to pop2.
 * toks = (reverse passed_tok) ++ remaining_tokens   
 *     at the and of the lexer_function call.
 * At the very beginning, cur_tok and remaining_tokens overlap, but not after.
 * At the end of lexer_function call,  cur_tok  overlap  with passed_tok.
 * 
 * convention: I use "tr"  for "tokens refs"
 * 
 * I now also need this lexing trick because the lexer return comment
 * tokens.
 *)

type tokens_state = {
  mutable rest :         Parser.token list;
  mutable rest_clean :   Parser.token list;
  mutable current :      Parser.token;
  (* it's passed since last "checkpoint", not passed from the beginning *)
  mutable passed :       Parser.token list;
  mutable passed_clean : Parser.token list;
}


let mk_tokens_state toks = { 
    rest       = toks;
    rest_clean = (toks +> Common.exclude TH.is_comment);
    current    = (List.hd toks);
    passed = []; 
    passed_clean = [];
  }

(* Hacked lex. This function use refs passed by parse_print_error_heuristic 
 * tr means token refs.
 *)
let rec lexer_function tr = fun lexbuf -> 
  raise Todo

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let (_defs : (string, Pp_token.define_body) Hashtbl.t ref)  = 
  ref (Hashtbl.create 101)

let init_defs file =     
  pr2 (spf "using %s macro file" file);
  _defs := Common.hash_of_list (extract_macros file)

(* 
 * note: as now we go in two passes, there is first all the error message of
 * the lexer, and then the error of the parser. It is not anymore
 * interwinded.
 * 
 * !!!This function use refs, and is not reentrant !!! so take care.
 * It uses the _defs global defined above!!!!
 *)
let parse2 file = 

  let stat = Statistics_parsing.default_stat file in
  let filelines = Common.cat_array file in

  (* -------------------------------------------------- *)
  (* call lexer and get all the tokens *)
  (* -------------------------------------------------- *)
  let toks_orig = tokens file in

  let toks = Parsing_hacks_define.fix_tokens_define toks_orig in
  (* todo: _defs_builtins *)
  let toks = Parsing_hacks.fix_tokens ~macro_defs:!_defs toks in

  let tr = mk_tokens_state toks in
  let lexbuf_fake = Lexing.from_function (fun buf n -> raise Impossible) in

  let rec loop () =

    (* todo?: I am not sure that it represents current_line, cos maybe
     * tr.current partipated in the previous parsing phase, so maybe tr.current
     * is not the first token of the next parsing phase. Same with checkpoint2.
     * It would be better to record when we have a } or ; in parser.mly,
     *  cos we know that they are the last symbols of external_declaration2.
     *)
    let checkpoint = TH.line_of_tok tr.current in

    (* bugfix: may not be equal to 'file' as after macro expansions we can
     * start to parse a new entity from the body of a macro, for instance
     * when parsing a define_machine() body, cf standard.h
     *)
    let checkpoint_file = TH.file_of_tok tr.current in

    tr.passed <- [];
    (* for some statistics *)
    let was_define = ref false in

    let elem = 
      (try 
          (* -------------------------------------------------- *)
          (* Call parser *)
          (* -------------------------------------------------- *)
          Parser.celem (lexer_function tr) lexbuf_fake
        with e -> 
          begin
            (match e with
            (* Lexical is not anymore launched I think *)
            | Lexer.Lexical s -> 
                pr2 ("lexical error " ^s^ "\n =" ^ error_msg_tok tr.current)
            | Parsing.Parse_error -> 
                pr2 ("parse error \n = " ^ error_msg_tok tr.current)
            | Semantic.Semantic (s, i) -> 
                pr2 ("semantic error " ^s^ "\n ="^ error_msg_tok tr.current)
            | e -> raise e
            );

            let line_error = TH.line_of_tok tr.current in

            (*  error recovery, go to next synchro point *)
            let (passed', rest') = 
              Parsing_recovery_cpp.find_next_synchro tr.rest tr.passed in
            tr.rest <- rest';
            tr.passed <- passed';

            tr.current <- List.hd passed';
            tr.passed_clean <- [];           (* enough ? *)
            (* with error recovery, rest and rest_clean may not be in sync *)
            tr.rest_clean <- (tr.rest +> Common.exclude TH.is_comment);

            let checkpoint2 = TH.line_of_tok tr.current in (* <> line_error *)
            let checkpoint2_file = TH.file_of_tok tr.current in

            (* was a define ? *)
            let xs = tr.passed +> List.rev +> Common.exclude TH.is_comment in
            if List.length xs >= 2 
            then 
              (match Common.head_middle_tail xs with
              | Parser.TDefine _, _, Parser.TCommentNewline_DefineEndOfMacro _ 
                  -> 
                  was_define := true
              | _ -> ()
              )
            else pr2 "WIERD: length list of error recovery tokens < 2 ";
            
            if !was_define && !Flag.filter_define_error
            then ()
            else 
              (* bugfix: *)
              if (checkpoint_file = checkpoint2_file) && checkpoint_file = file
              then print_bad line_error (checkpoint, checkpoint2) filelines
              else pr2 "PB: bad: but on tokens not from original file"
              ;

            let info_of_bads = Common.map_eff_rev TH.info_of_tok tr.passed in 
            Ast.NotParsedCorrectly info_of_bads
          end
      )
    in

    (* again not sure if checkpoint2 corresponds to end of bad region *)
    let checkpoint2 = TH.line_of_tok tr.current in
    let checkpoint2_file = TH.file_of_tok tr.current in

    let diffline = 
      if (checkpoint_file = checkpoint2_file) && (checkpoint_file = file)
      then (checkpoint2 - checkpoint) 
      else 0
        (* TODO? so if error come in middle of something ? where the
         * start token was from original file but synchro found in body
         * of macro ? then can have wrong number of lines stat.
         * Maybe simpler just to look at tr.passed and count
         * the lines in the token from the correct file ?
         *)
    in
    let info = mk_info_item file (List.rev tr.passed) in 

    (* some stat updates *)
    stat.Stat.commentized <- 
      stat.Stat.commentized + count_lines_commentized (snd info);
    (match elem with
    | Ast.NotParsedCorrectly xs -> 
        if !was_define && !Flag.filter_define_error
        then stat.Stat.correct <- stat.Stat.correct + diffline
        else stat.Stat.bad     <- stat.Stat.bad     + diffline
    | _ -> stat.Stat.correct <- stat.Stat.correct + diffline
    );

    (match elem with
    | Ast.FinalDef x -> [(Ast.FinalDef x, info)]
    | xs -> (xs, info):: loop () (* recurse *)
    )
  in
  let v = loop() in
  let v = with_program2 Parsing_consistency_cpp.consistency_checking v in
  (v, stat)

let parse a  = 
  Common.profile_code "Parse_cpp.parse" (fun () -> parse2 a)

let parse_program file = 
  let (ast2, _stat) = parse file in
  program_of_program2 ast2
