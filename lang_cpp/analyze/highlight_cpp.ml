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

open Ast_cpp

module Ast = Ast_cpp
module V = Visitor_cpp

open Highlight_code

module T = Parser_cpp
module TH = Token_helpers_cpp

module S = Scope_code

module Type = Type_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers when have global analysis information *)
(*****************************************************************************)

let h_debug_functions = Common.hashset_of_list [
  "DEBUG"
]

let fake_no_def2 = NoUse
let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

let visit_toplevel 
    ~tag_hook
    prefs 
    (*db_opt *)
    (toplevel, toks)
  =
  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.add already_tagged ii true
  )
  in

  (* -------------------------------------------------------------------- *)
  (* toks phase 1 *)

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
        let s = Ast.str_of_info ii in
        let s5 =  Ast.str_of_info ii5 in
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
        aux_toks xs

    (* a little bit hphp specific *)
    | T.TComment ii::T.TCommentSpace ii2::T.TComment ii3::xs 
      ->
        let s = Ast.str_of_info ii in
        let s2 = Ast.str_of_info ii3 in
        (match () with
        | _ when s =~ "//////////.*" 
            && s2 =~ "// .*" 
            ->
            tag ii3 CommentSection1
        | _ -> 
            ()
        );
        aux_toks xs

    (* heuristic for class/struct definitions. 
     * 
     * Must be before the heuristic for function definitions
     * otherwise this pattern will never be exercised
     * 
     * the code below has been commented because it generates
     * too much false positives. For instance in 'struct foo * bar(...) '
     * foo would be classified as the start of a struct definition
     *
     * don't want forward class declarations to generate noise
     * | (T.Tclass(ii) | T.Tstruct(ii) | T.Tenum(ii))
     * ::T.TCommentSpace ii2::T.TIdent(s, ii3)::T.TPtVirg _::xs ->
     * aux_toks xs
     * | (T.Tclass(ii) | T.Tstruct(ii) | T.Tenum (ii) 
     * | T.TIdent ("service", ii)
     * )
     * ::T.TCommentSpace ii2::T.TIdent(s, ii3)::xs
     * when Ast.col_of_info ii = 0 ->
     * 
     * tag ii3 (Class (Def2 fake_no_def2));
     * aux_toks xs;
     *)

    | (T.Tclass(ii) | T.Tstruct(ii) | T.Tenum (ii)
        (* thrift stuff *)
        | T.TIdent ("service", ii)
      )
      ::T.TCommentSpace ii2
      ::T.TIdent(s, ii3)
      ::T.TCommentSpace ii4
      ::T.TOBrace ii5
      ::xs
        when Ast.col_of_info ii = 0 ->

        tag ii3 (Class (Def2 fake_no_def2));
        aux_toks xs;


    (* heuristic for function definitions *)
    | t1::xs when TH.col_of_tok t1 = 0 && TH.is_not_comment t1 ->
        let line_t1 = TH.line_of_tok t1 in
        let rec find_ident_paren xs =
          match xs with
          | T.TIdent(s, ii1)::T.TOPar ii2::_ ->
              tag ii1 (Function (Def2 NoUse));
          | T.TIdent(s, ii1)::T.TCommentSpace _::T.TOPar ii2::_ ->
              tag ii1 (Function (Def2 NoUse));
          | x::xs ->
              find_ident_paren xs
          | [] -> ()
        in
        let same_line = (t1::xs) +> Common.take_while (fun t ->
          TH.line_of_tok t = line_t1) 
        in
        find_ident_paren same_line;
        aux_toks xs

    | x::xs ->
        aux_toks xs
  in
  aux_toks toks;

  let is_at_toplevel = ref true in

  (* -------------------------------------------------------------------- *)
  (* ast phase 1 *) 

  let hooks = { (*V.default_visitor with *)

    (* -------------------------------------------------------------------- *)
    V.kcompound =  (fun (k, vx) x ->
      Common.save_excursion is_at_toplevel false (fun () ->
        k x
      )
    );

    (* -------------------------------------------------------------------- *)
    V.kvar_declaration = (fun (k, _) x ->
      match x with
      | DeclList (xs_comma, ii) ->
          let xs = Ast.uncomma xs_comma in
          xs +> List.iter (fun onedecl ->

            let (nameopt, ft, sto) = onedecl in
            nameopt +> Common.do_option (fun ((s, ini_opt), ii) ->

              let categ = 
                if Type.is_function_type ft
                then FunctionDecl NoUse
                else
                 (* could be a global too when the decl is at the top *)
                  if !is_at_toplevel  || fst sto = (Sto Extern)
                  then (Global (Def2 fake_no_def2))
                  else (Local Def)
              in
                
              ii +> List.iter (fun ii -> tag ii categ)
            );
          );
          k x
      | MacroDecl _ ->
           k x
    );
    (* -------------------------------------------------------------------- *)
    V.kstmt = (fun (k, _) x ->
      match x with
      | Labeled (Ast.Label (_s, st)), ii ->
          ii +> List.iter (fun ii -> tag ii KeywordExn);
          k x

      | Jump (Goto s), ii ->
          let (iigoto, lblii, iiptvirg) = Common.tuple_of_list3 ii in
          tag lblii KeywordExn;
          k x
      | _ -> k x
    );

    (* -------------------------------------------------------------------- *)
    V.kexpr = (fun (k, _) x ->
      let (ebis, aref), ii = x in
      match ebis with

      | Ident (name, idinfo) ->
          let ii = Ast.info_of_name_tmp name in
          let s = Ast.str_of_info ii in
          if s =~ "[A-Z][A-Z_]*" &&
            (* the FunCall case might have already tagged it with something *)
            not (Hashtbl.mem already_tagged ii)
          then 
            tag ii (MacroVar (Use2 fake_no_use2))
          else 
            (match idinfo.Ast.i_scope with
            | S.Local -> 
                tag ii (Local Use)
            | S.Param ->
                tag ii (Parameter Use)
            | S.Global ->
                tag ii (Global (Use2 fake_no_use2));
            | S.NoScope ->
                ()
            | S.Static ->
                (* todo? could invent a Static in highlight_code ? *)
                tag ii (Global (Use2 fake_no_use2));
                
            | 
              (S.ListBinded|S.LocalIterator|S.LocalExn|S.Class)
                -> failwith "scope not handled"
            )
          

      | FunCallSimple (name, args) ->
          let ii = Ast.info_of_name_tmp name in
          let s = Ast.str_of_info ii in
          (if Hashtbl.mem h_debug_functions s
          then
            tag ii BuiltinCommentColor
          else
            tag ii (Function (Use2 fake_no_use2))
          );
          k x

      | RecordAccess (e, name)
      | RecordPtAccess (e, name) 
          ->
          let ii = Ast.info_of_name_tmp name in
          tag ii (Field (Use2 fake_no_use2));
          k x
      | _ -> k x
    );

    (* -------------------------------------------------------------------- *)
    V.kparameterType = (fun (k, vx) x ->
      let (_, ii) = x in
      ii +> List.iter (fun ii -> tag ii (Parameter Def));
      k x
    );

    (* -------------------------------------------------------------------- *)
    V.ktypeC = (fun (k, vx) x ->
      let (typeCbis, ii)  = x in
      match typeCbis with
      | TypeName (s, opt) ->
          ii +> List.iter (fun ii -> tag ii (TypeDef Use));
          k x

      | StructUnionName (su, s) ->
          ii +> List.iter (fun ii -> tag ii (StructName Use));
          k x

      | _ -> k x
    );
    (* -------------------------------------------------------------------- *)
    V.kfieldkind = (fun (k, vx) x -> 
      match Ast.unwrap x with
      | FieldDecl onedecl ->
          let (nameopt, ft, sto) = onedecl in
          nameopt +> Common.do_option (fun ((s, ini_opt), ii) ->

            let kind = 
              (* poor's man object using function pointer; classic C idiom *)
              if Type.is_method_type ft
              then Method (Def2 fake_no_def2)
              else Field (Def2 NoUse)
            in
            ii +> List.iter (fun ii -> tag ii kind)
          );
          k x

      | BitField (sopt, ft, e) ->
          let (_, ii) = x in
          (match ii with
          | [iiname;iicolon] ->
              tag iiname (Field (Def2 NoUse))
          | [iicolon] -> ()
          | _ -> failwith "wrong bitfield"
          )
      | _ -> k x
    );
  }
  in
  let visitor = V.mk_visitor hooks in
  visitor.V.vtoplevel toplevel;

  (* -------------------------------------------------------------------- *)
  (* toks phase 2 *)

  toks +> List.iter (fun tok -> 
    match tok with

    | T.TComment ii ->
        if not (Hashtbl.mem already_tagged ii)
        then
          tag ii Comment

    | T.TInt (_,ii) | T.TFloat (_,ii) ->
        tag ii Number

    | T.TString (s,ii) ->
        tag ii String

    | T.TChar (s,ii) ->
        tag ii String

    | T.Tfalse ii | T.Ttrue ii  ->
        tag ii Boolean

    | T.TPtVirg ii
    | T.TOPar ii | T.TCPar ii
    | T.TOBrace ii | T.TCBrace ii 
    | T.TOCro ii | T.TCCro ii
    | T.TDot ii | T.TComma ii | T.TPtrOp ii  
    | T.TAssign (_, ii)
    | T.TEq ii 
    | T.TWhy ii | T.TTilde ii | T.TBang ii 
    | T.TEllipsis ii 
    | T.TCol ii ->
        tag ii Punctuation

    | T.TInc ii | T.TDec ii 
    | T.TOrLog ii | T.TAndLog ii | T.TOr ii 
    | T.TXor ii | T.TAnd ii | T.TEqEq ii | T.TNotEq ii
    | T.TInf ii | T.TSup ii | T.TInfEq ii | T.TSupEq ii
    | T.TShl ii | T.TShr ii  
    | T.TPlus ii | T.TMinus ii | T.TMul ii | T.TDiv ii | T.TMod ii  
        ->
        tag ii Operator

    | T.Tshort ii | T.Tint ii ->
        tag ii TypeInt
    | T.Tdouble ii | T.Tfloat ii 
    | T.Tlong ii |  T.Tunsigned ii | T.Tsigned ii 
    | T.Tchar ii 
        -> tag ii TypeInt (* TODO *)
    | T.Tvoid ii 
        -> tag ii TypeVoid
    | T.Tbool ii 
    | T.Twchar_t ii
      -> tag ii TypeInt
    (* thrift stuff *)

    | T.TIdent (
        ("string" | "i32" | "i64" | "i8" | "i16" | "byte"
          | "list" | "map" | "set" 
          | "binary"
        ), ii) ->
        tag ii TypeInt


    | T.Tauto ii | T.Tregister ii | T.Textern ii | T.Tstatic ii  
    | T.Tconst ii | T.Tvolatile ii 
    | T.Tbreak ii | T.Tcontinue ii
    | T.Treturn ii
    | T.Tdefault ii 
    | T.Tsizeof ii 
    | T.Trestrict ii 
      -> 
        tag ii Keyword

    | T.Tgoto ii ->
        (* people often use goto as an try/throw exception mechanism
         * so let's use the same color
         *)
        tag ii Keyword

    | T.Tasm ii 
    | T.Tattribute ii 
    | T.Tinline ii 
    | T.Ttypeof ii 
     -> 
        tag ii Keyword

    (* pp *)
    | T.TDefine ii -> 
        tag ii Define

    (* todo: could be also a MacroFunc *)
    | T.TIdentDefine (_, ii) ->
        tag ii (MacroVar (Def2 NoUse))

    (* never executed ? only the TIncludeStart token is in the token list ? *)
    | T.TInclude (_, _, _, ii) -> 
        tag ii Include

    | T.TIfdef ii | T.TIfdefelse ii | T.TIfdefelif ii | T.TEndif ii ->
        tag ii Ifdef
    | T.TIfdefBool (_, ii) | T.TIfdefMisc (_, ii) | T.TIfdefVersion (_, ii) ->
        tag ii Ifdef

    | T.Tthis ii  
    | T.Tnew ii | T.Tdelete ii  
    | T.Ttemplate ii | T.Ttypeid ii | T.Ttypename ii  
    | T.Toperator ii  
    | T.Tpublic ii | T.Tprivate ii | T.Tprotected ii | T.Tfriend ii  
    | T.Tvirtual ii  
    | T.Tnamespace ii | T.Tusing ii  
    | T.Tconst_cast ii | T.Tdynamic_cast ii
    | T.Tstatic_cast ii | T.Treinterpret_cast ii
    | T.Texplicit ii | T.Tmutable ii  
    | T.Texport ii 
      ->
        tag ii Keyword

    | T.TPtrOpStar ii | T.TDotStar ii  ->
        tag ii Punctuation

    | T.TColCol ii  
    | T.TColCol2 ii ->
        tag ii Punctuation


    | T.Ttypedef ii | T.Tstruct ii |T.Tunion ii | T.Tenum ii  
        ->
        tag ii TypeMisc (* TODO *)

    | T.Tif ii | T.Telse ii ->
        tag ii KeywordConditional

    | T.Tswitch ii | T.Tcase ii ->
        tag ii KeywordConditional

    | T.Ttry ii | T.Tcatch ii | T.Tthrow ii ->
        tag ii KeywordExn

    (* thrift *)
    | T.TIdent (("throws" | "exception"), ii) ->
        tag ii KeywordExn


    | T.Tfor ii | T.Tdo ii | T.Twhile ii ->
        tag ii KeywordLoop

    | T.Tclass ii ->
        tag ii Keyword

    (* thrift *)
    | T.TIdent (("service" | "include" | "extends"), ii) ->
        tag ii Keyword

    | T.TIdent (_, ii) ->
        ()

    | T.Tbool2 ii
    | T.Tlong2 ii
    | T.Tshort2 ii
    | T.Twchar_t2 ii
    | T.Tdouble2 ii
    | T.Tfloat2 ii
    | T.Tint2 ii
    | T.Tchar2 ii
        -> tag ii TypeInt


    | T.TtemplatenameQ2 _
    | T.TtemplatenameQ _
    | T.TypedefIdent2 _
    | T.Tconstructorname _
    | T.Ttemplatename _
    | T.Tclassname2 _
    | T.Tclassname _
    | T.TIntZeroVirtual _

    | T.TCCro2 _
    | T.TOCro2 _
    | T.TSup2 _
    | T.TInf2 _
    | T.TOParCplusplusInit _

    | T.TAction _
    | T.TCParEOL _

    | T.TMacroIterator _
    | T.TMacroDeclConst _
    | T.TMacroDecl _
    | T.TMacroString _
    | T.TMacroStmt _

    | T.TCommentCpp _
    | T.TCommentMisc _
      -> ()

    | T.TIncludeFilename (_, ii) ->
        tag ii String
    | T.TIncludeStart (ii, _aref) ->
        tag ii Include


    | T.TDefEOL _

    | T.TOBraceDefineInit _
    | T.TOParDefine _
    | T.TCppEscapedNewline _
    | T.TDefParamVariadic _

    | T.TypedefIdent _

    | T.TCommentNewline _
    | T.TCommentSpace _

    | T.EOF _
    | T.TUnknown _
        -> ()


  );

  ()
