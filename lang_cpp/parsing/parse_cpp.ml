(* Yoann Padioleau
 *
 * Copyright (C) 2002-2008 Yoann Padioleau
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
(* module Visitor_c = Visitor_cplusplus *)

module Stat = Statistics_parsing

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

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
  +> (fun (program, infos) -> 
    f program, infos
  )
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
  let buf = Buffer.create 100 in
  let s = 
    (* old: get_slice_file filename (line1, line2) *)
    begin
      toks +> List.iter (fun tok -> 
        match TH.pinfo_of_tok tok with
        | Parse_info.OriginTok _ -> Buffer.add_string buf (TH.str_of_tok tok)
        | Parse_info.Ab -> raise Impossible
        | _ -> ()
      );
      Buffer.contents buf
    end
  in
  (s, toks) 

let mk_info_item a b = 
  Common.profile_code "C parsing.mk_info_item" 
    (fun () -> mk_info_item2 a b)


(*****************************************************************************)
(* Error diagnostic *)
(*****************************************************************************)

let error_msg_tok tok = 
  let file = TH.file_of_tok tok in
  if !Flag.verbose_parsing
  then Parse_info.error_message file (token_to_strpos tok) 
  else ("error in " ^ file  ^ "set verbose_parsing for more info")


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
  | Parser.TCommentCpp (cppkind, ii) -> 
      if !Flag.filter_classic_passed
      then 
        (match cppkind with
        | Ast.CppOther -> 
            let s = Ast.str_of_info ii in
            (match s with
            | s when s =~ "KERN_.*" -> None
            | s when s =~ "__.*" -> None
            | _ -> Some (ii.Parse_info.token)
            )
             
        | Ast.CppDirective | Ast.CppAttr | Ast.CppMacro
            -> None
        | _ -> raise Todo
        )
      else Some (ii.Parse_info.token)
      
  | Parser.TCommentMisc ii
  | Parser.TAction ii 
    ->
      Some (ii.Parse_info.token)
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
      | Parse_info.OriginTok pinfo 
      | Parse_info.ExpandedTok (_,pinfo,_) -> 
	  let newline = pinfo.Parse_info.line in
	  if newline <> !line
	  then begin
            line := newline;
            incr count
	  end
      | _ -> ());
    !count
  end


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
(* Lexing only *)
(*****************************************************************************)

(* called by parse_print_error_heuristic *)
let tokens2 file = 
 let table     = Parse_info.full_charpos_to_pos file in

 Common.with_open_infile file (fun chan -> 
  let lexbuf = Lexing.from_channel chan in
  try 
    let rec tokens_aux () = 
      let tok = Lexer.token lexbuf in
      (* fill in the line and col information *)
      let tok = tok +> TH.visitor_info_of_tok (fun ii -> 
        { ii with Parse_info.token=
          (* could assert pinfo.filename = file ? *)
	  match Parse_info.pinfo_of_info ii with
	    Parse_info.OriginTok pi ->
              Parse_info.OriginTok (Parse_info.complete_parse_info file table pi)
	  | Parse_info.ExpandedTok (pi,vpi, off) ->
              Parse_info.ExpandedTok(
                (Parse_info.complete_parse_info file table pi),vpi, off)
	  | Parse_info.FakeTokStr (s,vpi_opt) -> 
              Parse_info.FakeTokStr (s,vpi_opt)
	  | Parse_info.Ab -> failwith "should not occur"
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
  Common.profile_code "C parsing.tokens" (fun () -> tokens2 a)


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


(*****************************************************************************)
(* C vs C++ file disambiguator *)
(*****************************************************************************)

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

(*****************************************************************************)
(* Parsing default define macros (in a standard.h file) *)
(*****************************************************************************)

(* TODO: extract macros *)

let parse_cpp_define_file file = 
  let toks = tokens file in
  let toks = Parsing_hacks.fix_tokens_define toks in
  Parsing_hacks.extract_cpp_define toks

(*****************************************************************************)
(* Error recovery *)
(*****************************************************************************)

(* see parsing_recovery_cpp.ml *)

(*****************************************************************************)
(* Consistency checking *)
(*****************************************************************************)

(* see parsing_consistency_cpp.ml *)
      
(*****************************************************************************)
(* Include/Define hacks *)
(*****************************************************************************)

(* Sometimes I prefer to generate a single token for a list of things in the
 * lexer so that if I have to passed them, liking passing TInclude then
 * it's easy. Also if I don't do a single token, then I need to 
 * parse the rest which may not need special stuff, like detecting 
 * end of line which the parser is not really ready for. So for instance
 * could I parse a #include <a/b/c/xxx.h> as 2 or more tokens ? just
 * lex #include ? so then need recognize <a/b/c/xxx.h> as one token ? 
 * but this kind of token is valid only after a #include and the
 * lexing and parsing rules are different for such tokens so not that
 * easy to parse such things in parser_c.mly. Hence the following hacks.
 * 
 * less?: maybe could get rid of this like I get rid of some of fix_define.
 *)

(* ------------------------------------------------------------------------- *)
(* helpers *)
(* ------------------------------------------------------------------------- *)

(* used to generate new token from existing one *)
let new_info posadd str ii =
  { Parse_info.token = 
      Parse_info.OriginTok { (Parse_info.parse_info_of_info ii) with 
        Parse_info.
        charpos = Parse_info.pos_of_info ii + posadd;
        str     = str;
        column = Parse_info.col_of_info ii + posadd;
      };
    comments = ();
    transfo = Parse_info.NoTransfo;
   }


let rec comment_until_defeol xs = 
  match xs with
  | [] -> failwith "cant find end of define token TDefEOL"
  | x::xs -> 
      (match x with
      | Parser.TDefEOL i -> 
          Parser.TCommentCpp (Ast.CppDirective, TH.info_of_tok x)
          ::xs
      | _ -> 
          let x' = 
            (* bugfix: otherwise may lose a TComment token *)
            if TH.is_real_comment x
            then x
            else Parser.TCommentCpp (Ast.CppOther, TH.info_of_tok x)
          in
          x'::comment_until_defeol xs
      )

let drop_until_defeol xs = 
  List.tl 
    (Common.drop_until (function Parser.TDefEOL _ -> true | _ -> false) xs)



(* ------------------------------------------------------------------------- *)
(* returns a pair (replaced token, list of next tokens) *)
(* ------------------------------------------------------------------------- *)

let tokens_include (info, includes, filename, inifdef) = 
  Parser.TIncludeStart (Parse_info.rewrap_str includes info, inifdef), 
  [Parser.TIncludeFilename 
      (filename, (new_info (String.length includes) filename info))
  ]

(*****************************************************************************)
(* Helper for main entry point *)
(*****************************************************************************)

(* The use of local refs (remaining_tokens, passed_tokens, ...) makes
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

  (* c++ext: *)
  mutable pending_qualifier: Parser.token list;
  mutable already_disambiguated : bool;
}

let retag_for_typedef xs = 
  xs +> List.map (function
  | Parser.TColCol ii -> Parser.TColCol2 ii
  | Parser.Tclassname (s,ii) -> Parser.Tclassname2 (s,ii)

  | Parser.Tclassname2 (s,ii) -> 
      pr2 "already transformed";
      Parser.Tclassname2 (s,ii)
  | Parser.TColCol2 ii -> 
      pr2 "already transformed";
      Parser.TColCol2 ii

  | _ -> raise Impossible
  )

let useless_token x = 
  match x with
  | x when TH.is_comment x -> true
      (* c++ext: *)
  | Parser.TColCol _ -> true
  | Parser.Tclassname _ -> true

  | Parser.TColCol2 _ -> true
  | Parser.Tclassname2 _ -> true

  | _ -> false 

(* Hacked lex. This function use refs passed by parse_print_error_heuristic 
 * tr means token refs.
 *)
let rec lexer_function tr = fun lexbuf -> 

  if tr.already_disambiguated && not (null tr.pending_qualifier)
  then
    let x = List.hd tr.pending_qualifier  in
    tr.pending_qualifier <- List.tl tr.pending_qualifier;
    x
  else begin

  match tr.rest with
  | [] -> pr2 "ALREADY AT END"; tr.current
  | v::xs -> 
    tr.rest <- xs;
    tr.current <- v;

    if !Flag.debug_lexer then Common.pr2_gen v;

    if TH.is_comment v
    then begin
      tr.passed <- v::tr.passed;
      lexer_function tr lexbuf
    end
    else 
      (* c++ext: in C there is an ambiguity that must be resolved by 
       * tagging if an ident is a typedef or really an ident. With this
       * info the LALR(1) yacc can know where to go. With c++ this typedef
       * or ident may be preceded by some qualifiers which forbid the
       * disambiguation. So we need to help yacc by tagging the qualifiers
       * tokens before the ident or typedef.
      *)
      if useless_token v 
      then begin
        tr.passed <- v::tr.passed;
        (*old: 
          v
        *)
        tr.already_disambiguated <- false;
        tr.pending_qualifier <- tr.pending_qualifier ++ [v];
        lexer_function tr lexbuf
      end
      else 
      begin
      let x = List.hd tr.rest_clean  in
      tr.rest_clean <- List.tl tr.rest_clean;
      assert (x = v);
      
      (match v with
      (* fix_define1 *)
      | Parser.TDefine (tok) -> 
          if not (LP.current_context () = LP.InTopLevel) 
          then begin
            pr2_once ("CPP-DEFINE: inside function, I treat it as comment");
            let v' = Parser.TCommentCpp (Ast.CppDirective,TH.info_of_tok v)
            in
            tr.passed <- v'::tr.passed;
            tr.rest       <- comment_until_defeol tr.rest;
            tr.rest_clean <- drop_until_defeol tr.rest_clean;
            lexer_function tr lexbuf
          end
          else begin
            tr.passed <- v::tr.passed;
            tr.passed_clean <- v::tr.passed_clean;
            v
          end
            
      | Parser.TInclude (includes, filename, inifdef, info) -> 
          if not (LP.current_context () = LP.InTopLevel) 
          then begin
            pr2_once ("CPP-INCLUDE: inside function, I treat it as comment");
            let v = Parser.TCommentCpp(Ast.CppDirective, info) in
            tr.passed <- v::tr.passed;
            lexer_function tr lexbuf
          end
          else begin
            let (v,new_tokens) = 
              tokens_include (info, includes, filename, inifdef) in
            let new_tokens_clean = 
              new_tokens +> List.filter TH.is_not_comment  in

            tr.passed <- v::tr.passed;
            tr.passed_clean <- v::tr.passed_clean;
            tr.rest <- new_tokens ++ tr.rest;
            tr.rest_clean <- new_tokens_clean ++ tr.rest_clean;
            v
          end
            
      | _ -> 
          
          (* typedef_fix1 *)
          let v = match v with
            | Parser.TIdent (s, ii) -> 
                if LP.is_typedef s 
                then Parser.TypedefIdent (s, ii)
                else Parser.TIdent (s, ii)
            | x -> x
          in
          
          let v = Parsing_hacks.lookahead (v::tr.rest_clean) tr.passed_clean in

          tr.passed <- v::tr.passed;

          (* the lookahead may have changed the status of the token and
           * consider it as a comment, for instance some #include are
           * turned into comments, hence this code. *)
          match v with
          | Parser.TCommentCpp _ -> lexer_function tr lexbuf
          | v -> 
              tr.passed_clean <- v::tr.passed_clean;

              (match v with
              | Parser.TypedefIdent _ -> 
                  tr.already_disambiguated <- true;
                  tr.pending_qualifier <- 
                    retag_for_typedef tr.pending_qualifier;
                  tr.pending_qualifier <- 
                    tr.pending_qualifier ++ [v];

                  let x = List.hd tr.pending_qualifier  in
                  tr.pending_qualifier <- List.tl tr.pending_qualifier;
                  x
              | Parser.TIdent _ -> 
                  tr.already_disambiguated <- true;
                  tr.pending_qualifier <- 
                    tr.pending_qualifier ++ [v];

                  let x = List.hd tr.pending_qualifier  in
                  tr.pending_qualifier <- List.tl tr.pending_qualifier;
                  x
              | v -> 
                  (* may have other stuff after a qualifier, like in ::new,
                   * or in destructor, so for those cases just flush
                   * the pending
                   *)
                  tr.already_disambiguated <- true;
                  tr.pending_qualifier <- 
                    tr.pending_qualifier ++ [v];

                  let x = List.hd tr.pending_qualifier  in
                  tr.pending_qualifier <- List.tl tr.pending_qualifier;
                  x
              )
                  
      )
    end
  end


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* can not be put in parsing_hack, cos then mutually recursive problem as
 * we also want to parse the standard.h file.
 *)
let init_defs std_h =     
  if not (Common.lfile_exists std_h)
  then pr2 ("warning: Can't find default macro file: " ^ std_h)
  else 
    Parsing_hacks._defs := Common.hash_of_list (parse_cpp_define_file std_h)


(* note: as now we go in 2 passes, there is first all the error message of
 * the lexer, and then the error of the parser. It is not anymore
 * interwinded.
 * 
 * !!!This function use refs, and is not reentrant !!! so take care.
 * It use globals defined in Lexer_parser and also the _defs global
 * in parsing_hack.ml.
 *)
let parse_print_error_heuristic2 file = 

  (* -------------------------------------------------- *)
  (* call lexer and get all the tokens *)
  (* -------------------------------------------------- *)
  LP.lexer_reset_typedef(); 
  let toks_orig = tokens file in

  let toks = Parsing_hacks.fix_tokens_define toks_orig in
  let toks = Parsing_hacks.fix_tokens_cpp toks in

  let filelines = (""::Common.cat file) +> Array.of_list in
  let stat = Statistics_parsing.default_stat file in

  let tr = { 
    rest       = toks;
    rest_clean = (toks +> Common.exclude useless_token);
    current    = (List.hd toks);
    passed = []; 
    passed_clean = [];

    (* c++ext: *)
    pending_qualifier = [];
    already_disambiguated = false;
  } in
  let lexbuf_fake = Lexing.from_function (fun buf n -> raise Impossible) in

  let rec loop () =

    if not (LP.is_enabled_typedef()) && !Flag.debug_typedef
    then pr2 "TYPEDEF:_handle_typedef=false. Not normal if dont come from exn";

    (* normally have to do that only when come from an exception in which
     * case the dt() may not have been done 
     * TODO but if was in scoped scope ? have to let only the last scope
     * so need do a LP.lexer_reset_typedef ();
     *)
    LP.enable_typedef();  
    LP._lexer_hint := (LP.default_hint ());
    LP.save_typedef_state();

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
            (* bugfix: otherwise get some List.hd exn *)
            tr.pending_qualifier <- [];
            tr.already_disambiguated <- false;

            (* choice: LP.restore_typedef_state(); *)
            LP.lexer_reset_typedef(); 


            let line_error = TH.line_of_tok tr.current in

            (*  error recovery, go to next synchro point *)
            let (passed', rest') = 
              Parsing_recovery_cpp.find_next_synchro tr.rest tr.passed in
            tr.rest <- rest';
            tr.passed <- passed';

            tr.current <- List.hd passed';
            tr.passed_clean <- [];           (* enough ? *)
            (* with error recovery, rest and rest_clean may not be in sync *)
            tr.rest_clean <- (tr.rest +> Common.exclude useless_token);

            let checkpoint2 = TH.line_of_tok tr.current in (* <> line_error *)
            let checkpoint2_file = TH.file_of_tok tr.current in

            (* was a define ? *)
            let xs = tr.passed +> List.rev +> Common.exclude useless_token in
            if List.length xs >= 2 
            then 
              (match Common.head_middle_tail xs with
              | Parser.TDefine _, _, Parser.TDefEOL _ -> 
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
    stat.Stat.commentized <- stat.Stat.commentized + count_lines_commentized (snd info);
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


let parse_print_error_heuristic a  = 
  Common.profile_code "C parsing" (fun () -> parse_print_error_heuristic2 a)

(* alias *)
let parse_c_and_cpp a = parse_print_error_heuristic a

let parse a = parse_print_error_heuristic a


let parse_tokens2 filename =

  let stat = Stat.default_stat filename in

  let toks_orig = tokens filename in
  let toks = Parsing_hacks.fix_tokens_define toks_orig in

  (* TODO *)
  [Ast.NotParsedCorrectly [], ("", toks)], stat

let parse_tokens a = 
  Common.profile_code "Parse_cpp.parse_tokens" (fun () -> parse_tokens2 a)


let parse_program file = 
  let (ast2, _stat) = parse file in
  program_of_program2 ast2
