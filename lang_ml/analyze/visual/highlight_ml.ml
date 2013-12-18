(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

open Ast_ml
module Ast = Ast_ml
module V = Visitor_ml
open Highlight_code
module T = Parser_ml
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Syntax highlighting for OCaml code for codemap.
 * 
 * This code can also be abused to generate the light database 
 * and the TAGS file (because codemap needs to know about
 * def and use of entities), but you should now prefer to
 * base such analysis on graph_code_cmt.ml instead of this file.
 *)

(*****************************************************************************)
(* Helpers when have global analysis information *)
(*****************************************************************************)

(* quite pad specific ... see my ~/.emacs *)
let h_pervasives_pad = Common.hashset_of_list [
  "pr2";"pr";"pr2_gen";
  "sprintf";"i_to_s";
  "pp2";"spf";
  "log";"log2";"log3"
]

let h_builtin_modules = Common.hashset_of_list [
  "Pervasives"; "Common";
  "List"; "Hashtbl"; "Array"; "Stack";
  "String"; "Str";
  "Sys"; "Unix"; "Gc";
  "Filename";
]

let h_builtin_bool = Common.hashset_of_list [
  "not";
  "exists"; "forall";
]


let fake_no_def2 = NoUse
let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)

(* set to true when want to debug the ast based tagger *)
let disable_token_phase2 = false

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The Ast is better for tagging idents
 * to figure out what kind of ident it is.
 *)
let visit_program
 ?(lexer_based_tagger=false)
 ~tag_hook prefs  (*db_opt *) (ast, toks) =

  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.add already_tagged ii true
  )
  in


  (* -------------------------------------------------------------------- *)
  (* ast phase 1 *) 
  (* -------------------------------------------------------------------- *)

  (* try better colorize identifiers which can be many different things
   * e.g. a field, a type, a function, a parameter, etc
   *)
  let in_let = ref false in
  let in_try_with = ref false in

  let v = V.mk_visitor { V.default_visitor with

    V.kitem = (fun (k, _) x ->

      match x with
      | Val (tok, name, tok2, ty) 
      | External (tok, name, tok2, ty, _, _)
         ->
          let info = Ast.info_of_name name in
          (match ty with
          (* todo: actually it can be a typedef alias to a function too
           * but this would require some analysis
           *)
          | TyFunction _ ->
              tag info (FunctionDecl NoUse)
          | _ ->
              tag info (Global (Def2 NoUse))
          );
          k x

      | Exception (tok, name, args) ->
          let info = Ast.info_of_name name in
          tag info (TypeDef Def);
          k x

      | Open (_tok, lname) ->
          let name = Ast.name_of_long_name lname in
          let info = Ast.info_of_name name in
          tag info (Module Use);
          k x

      | Ast.Module (_tok, uname, _tok2, mod_expr) ->
          let ii = Ast.info_of_name uname in
          tag ii (Module Def);
          k x

      | Let _
      | Type _ 
      | ItemTodo _
         ->
          k x
    );

    V.kqualifier = (fun (k, bigf) qu ->
      let module_infos = Ast.module_infos_of_long_name (qu, ()) in
      module_infos +> List.iter (fun ii ->
        tag ii (Module Use)
      )
    );
    V.kmodule_expr = (fun (k, bigf) mod_expr ->
      (match mod_expr with
      | ModuleName lname -> 
          let name = Ast.name_of_long_name lname in
          let info = Ast.info_of_name name in
          tag info (Module Use);
      | _ -> ()
      );
      k mod_expr
    );

    V.klet_binding = (fun (k, bigf) x ->
      match x with
      | LetClassic let_def -> 
          let name = let_def.l_name in
          let info = Ast.info_of_name name in

          if not !in_let then begin
            if List.length let_def.l_args > 0 || 
              Lib_parsing_ml.is_function_body let_def.l_body
            then tag info (Function (Def2 NoUse))
            else tag info (Global (Def2 NoUse))
          end else begin
            tag info (Local (Def))
          end;

          Common.save_excursion in_let true (fun () ->
            k x
          )
      | LetPattern (pat, _tok, body) ->
          (match pat with
          | PatTyped (_, PatVar name, _, _ty, _) ->
              let info = Ast.info_of_name name in

              if not !in_let then begin
                if Lib_parsing_ml.is_function_body body
                then tag info (Function (Def2 NoUse))
                else tag info (Global (Def2 NoUse))
              end else begin
                tag info (Local (Def))
          end;

          | _ -> 
              ()
          );
          Common.save_excursion in_let true (fun () ->
            k x
          )

    );

    V.kexpr = (fun (k, bigf) x ->
      match x with
      | L long_name ->

          let name = Ast.name_of_long_name long_name in
          let info = Ast.info_of_name name in

          (* could have been tagged as a function name in the rule below *)
          if not (Hashtbl.mem already_tagged info)
          then begin 
            (* TODO could be a param, could be a local. Need scope analysis 
             * TODO could also be actually a func passed to a higher
             *  order function, as in List.map snd, or even x +> Common.sort
             *)
            tag info (Local Use) 
          end;
          k x

      | FunCallSimple (long_name, args) ->
          let name = Ast.name_of_long_name long_name in
          let info = Ast.info_of_name name in

          let module_name = Ast.module_of_long_name long_name in
          let module_infos = Ast.module_infos_of_long_name long_name in

          let s = Ast.str_of_name name in
          (match () with
          | _ when s = "ref" -> tag info UseOfRef
          | _ when Hashtbl.mem h_builtin_modules module_name ->
            module_infos +> List.iter (fun ii -> tag ii BuiltinCommentColor);
            tag info Builtin;
          | _ ->
            tag info (Function (Use2 fake_no_use2));
          );

          k x

      (* disambiguate "with" which can be used for match, try, or record *)
      | Match (_match, _e1, tok_with, _match_cases) ->
          tag tok_with (KeywordConditional);
          k x

      | Try (try_tok, e, tok_with, match_cases) ->
          tag tok_with (KeywordExn);

          k (Try (try_tok, e, tok_with, []));

          Common.save_excursion in_try_with true (fun () ->
            match_cases +> Ast.unpipe +> List.iter (fun match_case ->
              bigf (MatchCase match_case)
            )
          )

      | FieldAccess (e, _tok, long_name) 
      | FieldAssign (e, _tok, long_name, _, _) 
        ->
          let name = Ast.name_of_long_name long_name in
          let info = Ast.info_of_name name in
          tag info (Field (Use2 fake_no_use2));
          k x

      | ObjAccess (e, tok, name) ->
          let info = Ast.info_of_name name in
          tag info (Method (Use2 fake_no_use2));
          k x

      | Constr (long_name, eopt) ->
          let name = Ast.name_of_long_name long_name in
          let info = Ast.info_of_name name in
          tag info (ConstructorUse fake_no_use2);
          k x

      (* very pad specific ... *)
      | Infix (l1, ("=~", _), C(Ast.String(s, tok))) ->
          tag tok Regexp;
          k x

      | _ -> k x
    );

    V.kpattern = (fun (k, _) x ->
      match x with
      | PatConstr (long_name, popt) ->
          let name = Ast.name_of_long_name long_name in
          let info = Ast.info_of_name name in

          (if !in_try_with 
          then tag info (KeywordExn)
          else tag info (ConstructorMatch fake_no_use2);
          );
          k x
      | _ -> k x
    );

    V.kty = (fun (k, _) t ->
      match t with
      | TyName long_name ->
          let name = Ast.name_of_long_name long_name in
          let info = Ast.info_of_name name in
          tag info TypeMisc;
          k t

      | TyApp (ty_args, long_name) ->
          let name = Ast.name_of_long_name long_name in
          let info = Ast.info_of_name name in
          (* different color for higher-order types *)
          tag info TypeVoid;

          (* todo: ty_args *)

          k t

      | TyVar (_tok, name) ->
          let info = Ast.info_of_name name in

          tag info TypeVoid;
          k t

          
      | TyTuple _
      | TyTuple2 _
      | TyFunction _
      | TyTodo
          -> k t
    );

    V.ktype_declaration = (fun (k, _) x ->
      match x with
      | TyDef (ty_params, name, tok, type_kind) ->

          let info = Ast.info_of_name name in
          tag info (TypeDef Def);
          (* todo: ty_params *)

          (match type_kind with
          | TyAlgebric xs ->
              xs +> Ast.unpipe +> List.iter (fun (name, args) ->
                let info = Ast.info_of_name name in
                tag info (ConstructorDef fake_no_def2)
              );

          | TyCore _ | TyRecord _ -> ()
          );

          k x
          
      | TyAbstract _ ->
          k x
    );

    V.kfield_decl = (fun (k, _) fld ->
      let name = fld.fld_name in
      let info = Ast.info_of_name name in
      tag info (Field (Def2 fake_no_def2));
      
      k fld
    );

    V.kfield_expr = (fun (k, _) x ->
      match x with
      | FieldExpr (long_name, _, _)
      | FieldImplicitExpr long_name
          ->

          let name = Ast.name_of_long_name long_name in
          let info = Ast.info_of_name name in
          tag info (Field (Use2 fake_no_use2));
          k x
    );
  }
  in
  v (Program ast);

  (* -------------------------------------------------------------------- *)
  (* toks phase 1 *)
  (* -------------------------------------------------------------------- *)
  (* 
   * note: all TCommentSpace are filtered in xs so easier to write
   * rules (but regular comments are kept as well as newlines).
   *)
  let rec aux_toks xs = 
    match xs with
    | [] -> ()

    (* a little bit pad specific *)
    |   T.TComment(ii)
      ::T.TCommentNewline (ii2)
      ::T.TComment(ii3)
      ::T.TCommentNewline (ii4)
      ::T.TComment(ii5)
      ::xs ->

        let s = PI.str_of_info ii in
        let s5 =  PI.str_of_info ii5 in
        (match () with
        | _ when s =~ ".*\\*\\*\\*\\*" && s5 =~ ".*\\*\\*\\*\\*" ->
          tag ii CommentEstet;
          tag ii5 CommentEstet;
          tag ii3 CommentSection1
        | _ when s =~ ".*------" && s5 =~ ".*------" ->
          tag ii CommentEstet;
          tag ii5 CommentEstet;
          tag ii3 CommentSection2
        | _ when s =~ ".*####" && s5 =~ ".*####" ->
          tag ii CommentEstet;
          tag ii5 CommentEstet;
          tag ii3 CommentSection0
        | _ ->
            ()
        );
        aux_toks (T.TComment ii3::T.TCommentNewline ii4::T.TComment ii5::xs)


    (* If had a parse error, then the AST will not contain the definitions, but
     * we can still try to tag certain things. Here is a
     * poor's man semantic tagger. Infer if ident is a func, or class,
     * or module based on the few tokens around. 
     * 
     * This may look ridiculous to do such semantic tagging using tokens 
     * instead of the full ast but many ocaml files could not parse with
     * the default parser because of camlp4 extensions so having
     * a solid token-based tagger is still useful as a last resort.
     *)
    | T.Tlet(ii)::T.TLowerIdent(s, ii3)::T.TEq ii5::xs
        when PI.col_of_info ii = 0 ->

        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Global (Def2 NoUse));

        aux_toks xs;

    | T.Tlet(ii)::T.TLowerIdent(s, ii3)::xs
        when PI.col_of_info ii = 0 ->

        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Function (Def2 NoUse));

        aux_toks xs;

    | (T.Tval(ii)|T.Texternal(ii))::T.TLowerIdent(s, ii3)::xs
        when PI.col_of_info ii = 0 ->

        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (FunctionDecl NoUse);
        aux_toks xs;

    | T.Tlet(ii)::
      T.Trec(_ii)::
      T.TLowerIdent(s, ii3)::xs
        when PI.col_of_info ii = 0 ->

        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Function (Def2 NoUse));
        aux_toks xs;

    | T.Tand(ii)::T.TLowerIdent(s, ii3)::xs
        when PI.col_of_info ii = 0 ->

        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Function (Def2 NoUse));
        aux_toks xs;

    | T.Ttype(ii)::T.TLowerIdent(s, ii3)::xs
        when PI.col_of_info ii = 0 ->

        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (TypeDef Def);
        aux_toks xs;

    (* module defs *)

    | T.Tmodule(_)
      ::T.TUpperIdent(_,ii_mod)
      ::T.TEq (_)
      ::T.Tstruct (_)::xs ->

        tag ii_mod (Module Def);
        aux_toks xs

    | T.Tmodule(_)
      ::T.TUpperIdent(_,ii_mod)
      ::T.TColon (_)
      ::T.Tsig (_)::xs ->

        tag ii_mod (Module Def);
        aux_toks xs



    (* bad smell, use of ref *)

    | T.TBang ii1::T.TLowerIdent(s2, ii2)::xs ->
        tag ii2 (UseOfRef);
        aux_toks xs

    | T.TBang ii1::T.TUpperIdent(s, ii)::T.TDot _::T.TLowerIdent(s2, ii2)::xs ->
        tag ii (Module Use);
        tag ii2 (UseOfRef);
        aux_toks xs

    |    T.TLowerIdent(_, ii1)
      ::(T.TAssign ii2 | T.TAssignMutable ii2)
      ::xs ->
        tag ii1 (UseOfRef);
        tag ii2 (UseOfRef);
        aux_toks xs


    (* module use, and function call! *)

    | T.TUpperIdent(s, ii)::T.TDot ii2::T.TUpperIdent(s2, ii3)::xs ->
        tag ii (Module Use);
        aux_toks xs;

    | T.TUpperIdent(s, ii)::T.TDot ii2::T.TLowerIdent(s2, ii3)::xs ->
        
        (* see my .emacs *)
        if Hashtbl.mem h_builtin_modules s then begin
          tag ii BuiltinCommentColor;
          if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
          then tag ii3 Builtin;
        end else begin
          tag ii (Module Use);
          (* tag ii3 (Function Use); *)
        end;
        aux_toks xs;

    (* labels *)
    | T.TTilde ii1::T.TLowerIdent (s, ii2)::xs ->
        (* TODO when parser, can also have Use *)
        tag ii1 (Parameter Def);
        tag ii2 (Parameter Def);
        aux_toks xs

    (* grammar rules in ocamlyacc *)
    | T.TLowerIdent (s, ii1)::T.TColon _::xs 
      when PI.col_of_info ii1 = 0
        ->
        tag ii1 GrammarRule;
        aux_toks xs
        

    | x::xs ->
        aux_toks xs
  in
  let toks' = toks +> Common.exclude (function
    | T.TCommentSpace _ -> true
    | _ -> false
  )
  in
  aux_toks toks';

  (* -------------------------------------------------------------------- *)
  (* toks phase 2 *)
  (* -------------------------------------------------------------------- *)

  if not disable_token_phase2 then
  toks +> List.iter (fun tok -> 
    match tok with
    | T.TComment ii ->
        if not (Hashtbl.mem already_tagged ii)
        then
          (* a little bit syncweb specific *)
          let s = PI.str_of_info ii in
          (match s with
          (* yep, s e x are the syncweb markers *)
          | _ when s =~ "(\\*[sex]:"  -> tag ii CommentSyncweb
          (* normally then use of *** or ### or --- should be enough,
           * but in some files like ocamlyacc files the preceding
           * heuristic fail in which case it's useful to have those
           * rules. Moreover ocamldoc use something similar
           *)
          | _ when s =~ "(\\*1 "  -> tag ii CommentSection1
          | _ when s =~ "(\\*2 "  -> tag ii CommentSection2
          | _ when s =~ "(\\*3 "  -> tag ii CommentSection3
          | _ -> tag ii Comment
          )

    | T.TCommentMisc ii ->
        tag ii Comment

    | T.TCommentNewline ii | T.TCommentSpace ii 
      -> ()

    | T.TUnknown ii 
      -> tag ii Error
    | T.EOF ii
      -> ()

    | T.TSharpDirective ii -> tag ii Ifdef

    | T.TString (s,ii) ->
        (* can have been tagged as a regexp *)
        if not (Hashtbl.mem already_tagged ii)
        then tag ii String

    | T.TChar (s, ii) ->
        tag ii String
    | T.TFloat (s,ii) | T.TInt (s,ii) ->
        tag ii Number

    | T.Tfalse ii | T.Ttrue ii -> 
        tag ii Boolean



    | T.Tlet ii | T.Tin ii | T.Tand ii | T.Trec ii 
    | T.Tval ii | T.Texternal ii
        -> tag ii Keyword

    | T.Tfun ii | T.Tfunction ii ->
        tag ii Keyword

    | T.Ttype ii
    | T.Tof ii
      -> tag ii Keyword





    | T.Tif ii | T.Tthen ii | T.Telse ii ->
        tag ii KeywordConditional

    | T.Tmatch ii -> (* TODO: should also colorize it's with *)
        tag ii KeywordConditional
    | T.Twhen ii -> (* TODO: should also colorize it's with, when parser *)
        tag ii KeywordConditional

    | T.Ttry ii  ->
        tag ii KeywordExn


    | T.Twith ii ->
        if not (Hashtbl.mem already_tagged ii)
        then
          tag ii Keyword

    | T.Tfor ii | T.Tdo ii | T.Tdone ii | T.Twhile ii
    | T.Tto ii
    | T.Tdownto ii
      -> tag ii KeywordLoop

    | T.Tbegin ii | T.Tend ii ->
        tag ii KeywordLoop (* TODO: better categ ? *)


    | T.TBang ii 
    | T.TAssign ii 
    | T.TAssignMutable ii 
      ->
        if not (Hashtbl.mem already_tagged ii)
        then
          tag ii UseOfRef;

    | T.TEq ii ->
        if not (Hashtbl.mem already_tagged ii)
        then
          tag ii Punctuation
        

    | T.TSemiColon ii | T.TPipe ii | T.TComma ii

    | T.TOBracket ii | T.TCBracket ii
    | T.TOBrace ii | T.TCBrace ii
    | T.TOParen ii | T.TCParen ii
    | T.TOBracketPipe ii | T.TPipeCBracket ii

    | T.TPlus ii | T.TMinus ii
    | T.TLess ii | T.TGreater ii

    | T.TDot (ii)
    | T.TColon (ii)
        ->
        tag ii Punctuation


    | T.TUpperIdent (s, ii) ->
        if not (Hashtbl.mem already_tagged ii)
        then
          ()
          (* tag ii Constructor *)

    | T.TLabelDecl (s, ii) ->
        tag ii (Parameter Def)


    | T.Topen ii  ->
        tag ii BadSmell

    | T.Tmodule ii | T.Tstruct ii | T.Tsig ii | T.Tinclude ii | T.Tfunctor ii
        -> tag ii KeywordModule

    | T.Tclass ii  | T.Tvirtual ii | T.Tprivate ii | T.Tobject ii
    | T.Tnew ii | T.Tmethod ii | T.Tinitializer ii | T.Tinherit ii
    | T.Tconstraint ii
        -> tag ii KeywordObject

    | T.Tmutable ii
        -> tag ii KeywordLoop

    | T.Tas ii
        -> tag ii Keyword

    | T.Texception ii
        -> tag ii KeywordExn

    | T.Tlazy ii
    | T.Tassert ii

        -> tag ii Keyword

    | T.TUnderscore ii

    | T.TTilde ii

    | T.TStar ii
    | T.TSemiColonSemiColon ii
      -> tag ii Punctuation


    | T.Tland ii  | T.Tasr ii | T.Tlxor ii | T.Tlsr ii | T.Tlsl ii
    | T.Tlor ii | T.Tmod ii | T.Tor ii ->
        tag ii Punctuation

    | T.TSharp ii  | T.TQuote ii  | T.TBackQuote ii
    | T.TQuestion ii | T.TQuestionQuestion ii

    | T.TDotDot ii  | T.TColonGreater ii | T.TColonColon ii

    | T.TAnd ii | T.TAndAnd ii
      -> tag ii Punctuation

    | T.TPrefixOperator (_, ii) ->
        tag ii Operator
    | T.TInfixOperator (_, ii) ->
        tag ii Operator

    | T.TMinusDot ii
    | T.TPlusDot ii

    | T.TArrow ii | T.TBangEq ii
    | T.TOBracketGreater ii | T.TGreaterCBrace ii
    | T.TOBraceLess ii    | T.TGreaterCBracket ii | T.TOBracketLess ii

    | T.TOptLabelUse (_, ii)
    | T.TLabelUse (_, ii)
        -> tag ii (Parameter Def) (* TODO *)

    | T.TOptLabelDecl (_, ii)
        -> tag ii (Parameter Def)

    | T.TLowerIdent (s, ii)
     -> 
        match s with
        | _ when Hashtbl.mem h_pervasives_pad s ->
            tag ii BuiltinCommentColor
        | _ when Hashtbl.mem h_builtin_bool s ->
            tag ii BuiltinBoolean
        | "failwith" | "raise" ->
            tag ii KeywordExn
        | _ ->
            ()

  );

  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)  

  ()
