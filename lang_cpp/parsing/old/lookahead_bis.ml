tokens_state

  (* c++ext: *)
  mutable pending_qualifier: Parser.token list;
  mutable already_disambiguated : bool;


(* ??? *)
let useless_token x = 
  match x with
  | x when TH.is_comment x -> true
  (* c++ext: *)
  | Parser.TColCol _ -> true
  | Parser.TIdent_ClassnameInQualifier _ -> true

  | Parser.TColCol_BeforeTypedef _ -> true
  | Parser.TIdent_ClassnameInQualifier_BeforeTypedef _ -> true

  | _ -> false 


    (* c++ext: *)
    pending_qualifier = [];
    already_disambiguated = false;



(* ??? *)
let retag_for_typedef xs = 
  xs +> List.map (function
  | Parser.TColCol ii -> Parser.TColCol_BeforeTypedef ii
  | Parser.TIdent_ClassnameInQualifier (s,ii) -> 
      Parser.TIdent_ClassnameInQualifier_BeforeTypedef (s,ii)

  | Parser.TIdent_ClassnameInQualifier_BeforeTypedef (s,ii) -> 
      Parser.TIdent_ClassnameInQualifier_BeforeTypedef (s,ii)
  | Parser.TColCol_BeforeTypedef ii -> Parser.TColCol_BeforeTypedef ii
  | _ -> raise Impossible
  )



  (* ??? *)
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

    if TH.is_comment v (* this will pass too the TCommentCpp *)
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
      (* fix_define1 
       * TODO: move to fix_tokens_xxx style
       *)
      | Parser.TDefine (tok) -> 
          if not (LP.current_context () = LP.InTopLevel) 
          then begin
            let v' = 
              Hack.fresh_tok 
                (Parser.TComment_Cpp (Token_cpp.CppDirective,TH.info_of_tok v))
            in
            tr.passed <- v'::tr.passed;
            tr.rest       <- Parsing_hacks_define.comment_until_defeol tr.rest;
            tr.rest_clean <- Parsing_hacks_define.drop_until_defeol tr.rest_clean;
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
            let v = 
              Hack.fresh_tok
                (Parser.TComment_Cpp (Token_cpp.CppDirective, info))
            in
            tr.passed <- v::tr.passed;
            lexer_function tr lexbuf
          end
          else begin
            let (v,new_tokens) = 
              Parsing_hacks_define.tokens_include (info, includes, filename, inifdef) in
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
                then Parsing_hacks_lib.fresh_tok (Parser.TIdent_Typedef (s, ii))
                else Parser.TIdent (s, ii)
            | x -> x
          in
          
          let v = Parsing_hacks.lookahead (v::tr.rest_clean) tr.passed_clean in

          tr.passed <- v::tr.passed;

          (* the lookahead may have changed the status of the token and
           * consider it as a comment, for instance some #include are
           * turned into comments, hence this code. *)
          match v with
          | Parser.TComment_Cpp _ -> lexer_function tr lexbuf
          | v -> 
              tr.passed_clean <- v::tr.passed_clean;

              (* ??? *)
              (match v with
              | Parser.TIdent_Typedef _ -> 
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


....
            (* bugfix: otherwise get some List.hd exn *)
            tr.pending_qualifier <- [];
            tr.already_disambiguated <- false;

            (* choice: LP.restore_typedef_state(); ??? *)
            LP.lexer_reset_state(); 




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



...
            tr.passed_clean <- [];           (* enough ? *)
            (* with error recovery, rest and rest_clean may not be in sync *)
            tr.rest_clean <- (tr.rest +> Common.exclude TH.is_comment);



...
(* called when need to pass some tokens during some error recovery *)
val drop_until_defeol: Parser_cpp.token list -> Parser_cpp.token list
val comment_until_defeol: Parser_cpp.token list -> Parser_cpp.token list

(* generates TIncludeStart and TIncludeFilename tokens *)
val tokens_include: 
  Ast_cpp.info * string * string * bool ref ->
  Parser_cpp.token * Parser_cpp.token list



(* TODO: port to a fix_tokens_pp style.  
 * When not InToplevel then skip the define and comment everything
 *)
let rec comment_until_defeol xs = 
  match xs with
      
  | [] -> 
      (* job not done in Cpp_token_c.define_parse ? *)
      failwith "cant find end of define token TDefEOL"
  | x::xs -> 
      (match x with
      | Parser.TCommentNewline_DefineEndOfMacro i -> 
          (* fresh_tok *) 
            Parser.TComment_Cpp (Token_cpp.CppDirective, TH.info_of_tok x)
          ::xs
      | _ -> 
          let x' = 
            (* bugfix: otherwise may lose a TComment token *)
            if TH.is_real_comment x
            then x
            else
            (* fresh_tok *) 
              Parser.TComment_Cpp (Token_cpp.CppOther, TH.info_of_tok x)
          in
          x'::comment_until_defeol xs
      )

let drop_until_defeol xs = 
  xs +> Common.drop_until (function 
    Parser.TCommentNewline_DefineEndOfMacro _ -> true | _ -> false)
  +> List.tl

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

(*****************************************************************************)
(* Parsing hacks for #include *)
(*****************************************************************************)
(* returns a pair (replaced token, list of next tokens) *)

(* todo: move in a fix_tokens style *)
let tokens_include (info, includes, filename, inifdef) = 
  (* fresh_tok *) 
   Parser.TInclude_Start (Parse_info.rewrap_str includes info, inifdef),
  [ (* fresh_tok *) Parser.TInclude_Filename 
      (filename, (new_info (String.length includes) filename info))
  ]
