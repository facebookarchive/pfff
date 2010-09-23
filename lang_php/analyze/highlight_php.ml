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

open Ast_php

module Ast = Ast_php
module V = Visitor_php

open Highlight_code

module S = Scope_php

module Db = Database_php

module T = Parser_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(* todo: facebook specific. Also would be good to associate a message with *)
let hbad_functions = Common.hashset_of_list [
  "HTML";
  "curl_exec";
  "debug_rlog";
]

(*****************************************************************************)
(* Helpers when have global analysis information *)
(*****************************************************************************)

(* look if def in same file of current file *)
let place_ids current_file ids db = 
  match ids with
  | [] -> NoInfoPlace 
  | [x] ->
      let other_file = Db.filename_of_id x db in
      if other_file = current_file
      then PlaceLocal
      else 
        if Common.dirname current_file = 
           Common.dirname other_file
        then PlaceSameDir
        else PlaceExternal

  | x::y::xs -> 
      let other_files = 
        Common.map (fun id -> Db.filename_of_id id db) ids +> Common.uniq 
      in
      if List.mem current_file other_files
      then PlaceLocal
      else 
        if List.exists (fun other_file -> 
          Common.dirname current_file = 
          Common.dirname other_file
        ) other_files
        then PlaceSameDir
        else PlaceExternal


let arity_of_number nbuses =
  match nbuses with
  | 0 -> NoUse
  | 1 -> UniqueUse
  | n when n < 50 -> MultiUse
  | _ -> LotsOfUse


let use_arity_ident_function_or_macro s db =
  let ids = Db.function_ids__of_string s db in
  let nbuses = 
    ids +> List.map (fun id -> 
      try 
        let callers = db.Db.uses.Db.callers_of_f#assoc id in
        List.length callers
      with Not_found -> 0
    )
    +> Common.sum_int 
  in
  arity_of_number nbuses


let fake_no_def2 = NoUse
let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)


(*****************************************************************************)
(* PHP Code highlighter *)
(*****************************************************************************)

(* PHP mode and also expansion specificities coloring, and also now semantic
 * coloring and global-information-semantic feedback coloring!
 * Was using emacs_mode_xxx before but now have inlined the code and extended
 * it.
 *
 * note: Can do either a single function that do all, or multiple independent 
 * functions that repeatedly visit the same ast and tokens. The 
 * former is faster (no forest) but less flexible. If for instance want
 * to impose an order between the coloring, such as first keywords and 
 * then yacfe specific coloring such as "expanded", then need extra
 * stuff such as priority list. If have separate functions for those then
 * the caller can simply choose to call them in the order he wants so that
 * the last color win. Thus can easily separate concern.
 * 
 * 
 *)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The Ast is better for tagging idents
 * to figure out what kind of ident it is.
 *)
let visit_toplevel 
    ~tag
    ~maybe_add_has_type_icon
    prefs 
    db_opt
    (toplevel, toks)
  =
  let already_tagged = Hashtbl.create 101 in

  let tag = (fun ii categ ->

  (* with xhp lots of tokens such as 'var' can also be used
   * as attribute name so we must highlight them with their
   * generic keyword category only if there were not already
   * tagged. 
   * 
   * The same is true for other kinds of tokens.
   *)
    if not (Hashtbl.mem already_tagged ii)
    then begin 
      tag ii categ;
      Hashtbl.add already_tagged ii true
    end
  )
  in
  (* Strings are more than just strings in PHP and webapps in general *)
  let tag_string s ii =
    match s with
    | s when s =~ "/.*" -> tag ii EmbededUrl
    | s when s =~ "[a-z]+://" -> tag ii EmbededUrl
    (* security: html in strings is BAD ! *)
    | s when s =~ "<" -> tag ii BadSmell
    | _ -> tag ii String
  in

  (* toks phase 1 *)
  toks +> List.iter (fun tok -> 
    ()
  );

  (* ast phase 1 *) 
  let hooks = { V.default_visitor with

    (* -------------------------------------------------------------------- *)
    V.ktop = (fun (k, vx) x -> 
      match x with
      | Ast_php.FuncDef def ->

          tag def.f_tok Keyword;

          let name = def.f_name in
          let info = Ast.info_of_name name in
          tag info (Function (Def2 NoUse));

          db_opt +> Common.do_option (fun (id, current_file, db) ->
            maybe_add_has_type_icon id info db;
          );

          k x
      (* todo more ? *)
      | Ast_php.ClassDef def ->
          let name = def.c_name in
          let info = Ast.info_of_name name in
          tag info (Class (Def2 fake_no_def2));

          def.c_extends +> Common.do_option (fun (tok, name) ->
            let info = Ast.info_of_name name in
            tag info (Class (Use2 fake_no_use2));
          );

          def.c_implements +> Common.do_option (fun (tok, xs) ->
            xs +> Ast.uncomma +> List.iter (fun name ->
              let info = Ast.info_of_name name in
              tag info (Class (Use2 fake_no_use2));
            );
          );

          k x

      | InterfaceDef def -> 
          let name = def.i_name in
          let info = Ast.info_of_name name in
          tag info (Class (Def2 fake_no_def2));

          k x

      | StmtList _ ->
          k x
      | Halt (_, _, _) ->
          k x

      | NotParsedCorrectly _ -> 
          (* handled later *)
          ()
      | FinalDef _ -> 
          ()
    );
    (* -------------------------------------------------------------------- *)
    V.kparameter = (fun (k, _) param ->

      let info = Ast.info_of_dname param.p_name in
      tag info (Parameter Def);

      param.p_type +> Common.do_option (function
      | Hint name -> 
          let info = Ast.info_of_name name in
          tag info (TypeMisc);
      | HintArray tok ->
          tag tok (TypeMisc);
      )
    );

    (* -------------------------------------------------------------------- *)
    V.kstmt_and_def = (fun (k, bigf) x ->
      match x with
      | FuncDefNested def ->
          bigf.V.vtop (FuncDef def)
      | ClassDefNested def ->
          bigf.V.vtop (Ast.ClassDef def)
      | InterfaceDefNested def ->
          bigf.V.vtop (Ast.InterfaceDef def)
      | Stmt _ -> k x
    );

    (* -------------------------------------------------------------------- *)
    V.kclass_stmt = (fun (k, bigf) x ->
      match x with
      | Ast.Method def ->
          tag def.m_tok KeywordObject;

          let name = def.m_name in
          let info = Ast.info_of_name name in

          let kind = 
            if def.m_modifiers |> List.exists (fun (modifier, ii) -> 
              modifier = Ast.Static
            )
            then (StaticMethod (Def2 fake_no_def2))
            else (Method (Def2 fake_no_def2))
          in

          tag info kind;

          (* see scoping_php.ml *)
          let params = Ast.uncomma (Ast.unparen def.m_params) in
          params +> List.iter (fun param ->
            let info = Ast.info_of_dname param.p_name in
            tag info (Parameter Def);

          );

          db_opt +> Common.do_option (fun (id, current_file, db) ->
            let idmethod = Db.id_of_phpname name db in
            maybe_add_has_type_icon idmethod info db;
          );

          k x
      | Ast.XhpDecl d -> 
          (match d with
          | XhpAttributesDecl _ -> k x
          | XhpChildrenDecl _ -> k x
          | XhpCategoriesDecl (tok, decls, tok2) ->
              decls +> Ast.uncomma +> List.iter (fun (_tag, ii) ->
                tag ii TypeMisc
              );
          )

      | Ast.ClassConstants (tok, vars, tok2) ->
          vars +> Ast.uncomma +> List.iter (fun (name, _opt) ->
            let info = Ast.info_of_name name in
            tag info (Macro (Def2 NoUse));
          );
          k x;

      | Ast.ClassVariables (modifiers, vars, tok) ->
          vars +> Ast.uncomma +> List.iter (fun (dname, _opt) ->
            let info = Ast.info_of_dname dname in
            tag info (Field Def);
          );
          k x
    );

    (* -------------------------------------------------------------------- *)
    V.kstmt = (fun (k,bigf) stmt -> 
      k stmt;
      match stmt with

      | ExprStmt ((e, tok)) -> 
          ()
      | EmptyStmt tok -> 
          ()
      | Block xs_brace -> 
          ()
      | If ((tok, eparen, stmt, elseif, elseopt)) ->
          ()
      | IfColon ((v1, v2, v3, v4, v5, v6, v7, v8)) ->
          ()
      | While ((v1, v2, v3)) ->
          ()
      | Do ((v1, v2, v3, v4, v5)) ->
          ()
      | For ((v1, v2, v3, v4, v5, v6, v7, v8, v9)) ->
          ()
      | Switch ((v1, v2, v3)) ->
          ()
      | Foreach ((v1, v2, v3, v4, v5, v6, v7, v8)) ->
          ()
      | Break ((v1, v2, v3)) ->
          ()
      | Continue ((v1, v2, v3)) ->
          ()
      | Return ((v1, v2, v3)) ->
          ()

      | Throw ((v1, v2, v3)) ->
          ()
      | Try ((v1, v2, v3, v4)) ->
          ()

      | Echo ((v1, v2, v3)) ->
          ()

      | InlineHtml v1 -> 
          ()
      | Globals ((v1, v2, v3)) ->

          (* see scoping_php.ml *)
          v2 +> Ast.uncomma +> List.iter (fun x ->
            match x  with
            | GlobalVar dname ->
                let info = Ast.info_of_dname dname in
                tag info (Global (Def2 NoUse))

            (* TODO ?? *)
            | GlobalDollar _ -> ()
            | GlobalDollarExpr _ ->  ()
          );

      | StaticVars ((v1, v2, v3)) ->
          v2 +> Ast.uncomma +> List.iter (fun svar ->
            let (dname, affect_opt) = svar in
            let info = Ast.info_of_dname dname in
            tag info (Local Def);
          );
          ()
      | Ast.Use ((v1, v2, v3)) ->
          ()
      | Unset ((v1, v2, v3)) ->
          ()
      | Declare ((v1, v2, v3)) -> 
          ()

    );
    (* -------------------------------------------------------------------- *)
    V.kcatch = (fun (k,bigf) c -> 
      let (tok, (lp, (cname, dname), rp), stmts) = c in
      let info_class = Ast.info_of_name cname in
      tag info_class (Class (Use2 fake_no_use2));

      let info_dname = Ast.info_of_dname dname in
      tag info_dname (Local Use);
      k c
        
    );

    (* -------------------------------------------------------------------- *)
    V.kexpr = (fun (k,bigf) expr -> 
      (* old: k expr; *)
      k expr; (* still ? *)
      
      (* changer order ? color if notype after the expr visit ? *)
      (*
      (match Ast.get_onlytype_expr expr with
      | None -> 
          let ii = Ast.get_local_ii_of_expr_inlining_ii_of_name expr in
          if prefs.show_type_error then
            tag ii NoType;
      | Some _ -> ()
        );
      *)



      let (exprbis, tinfo) = expr in
      match exprbis with

      | Lv v1 ->
          ()
      | Sc v1 ->
          ()
      | Assign ((v1, v2, v3)) ->
          ()
      | AssignRef ((v1, v2, v3, v4)) ->
          ()
      | AssignNew ((v1, v2, v3, v4, v5, v6)) ->
          ()
      | AssignOp ((v1, v2, v3)) ->
          ()
      | Postfix ((v1, v2)) ->
          ()
      | Infix ((v1, v2)) ->
          ()
      | Binary ((v1, v2, v3)) ->
          ()
      | Unary ((v1, v2)) ->
          ()
      | CondExpr ((v1, v2, v3, v4, v5)) ->
          ()

      | AssignList ((v1, v2, v3, v4)) ->
          ()
      | ConsArray ((v1, v2)) ->
          ()


      | New ((v1, v2, v3)) ->
          ()
      | Clone ((v1, v2)) ->
          ()
      | InstanceOf ((v1, v2, v3)) ->
          ()

      | Cast (((cast, v1), v2)) ->
          tag v1 TypeMisc
      | CastUnset ((v1, v2)) ->
          ()
      | Exit ((v1, v2)) ->
          ()
      | At ((v1, v2)) ->
          ()
      | Print ((v1, v2)) ->
          ()

      | BackQuote ((v1, v2, v3)) ->
          ()
      | Ast.Include ((v1, v2)) ->
          ()
      | IncludeOnce ((v1, v2)) ->
          ()
      | Require ((v1, v2)) ->
          ()
      | RequireOnce ((v1, v2)) ->
          ()

      | Empty ((v1, v2)) ->
          ()
      | Isset ((v1, v2)) ->
          ()

      | Eval ((v1, v2)) ->
          ()
      | ParenExpr (v1, e, v2) -> 
          (* this is handled in a generic way in the token annotater below *)
          ()

      | XhpHtml _ ->
          ()
      | EDots _ -> ()
      | Lambda _ -> 
          ()

    );
    (* -------------------------------------------------------------------- *)
    V.kxhp_html = (fun (k, _) x ->
      match x with
      | Xhp ((_tag, iitag), attrs, tok_close1, body, (tag_opt, iitag_close)) ->
          k x
      | XhpSingleton ((_tag, iitag), attrs, iitag_close) ->
          k x
    );
    V.kxhp_attribute = (fun (k, _) x ->
      let ((attr_name, ii_attr_name), tok_eq, attr_val) = x in

      (match attr_name with
      | "href" | "src" ->
          (match attr_val with
          | XhpAttrString (tok1, xs, tok2) ->
              tag tok1 String;
              tag tok2 String;
              xs +> List.iter (function
              | EncapsString (s, ii) ->
                  tag ii EmbededUrl
              | EncapsExpr (_, _, _)
              | EncapsDollarCurly (_, _, _)
              | EncapsCurly (_, _, _)
              | EncapsVar _ ->
                  ()
              );
          | XhpAttrExpr e -> ()
          )
      | _ -> ()
      );
      
      k x
    );

    (* -------------------------------------------------------------------- *)
    V.kxhp_attr_decl = (fun (k, _) x ->
      match x with
      | XhpAttrInherit (xhp_tag, ii) ->
          tag ii (Class (Use2 (fake_no_use2)));

      | XhpAttrDecl ((attr_type, (attr_name, iiname), affect_opt, tok_opt)) ->
          (match attr_type with
          | XhpAttrType name ->
              let info = Ast.info_of_name name in
              tag info (TypeMisc);
          | XhpAttrEnum (tok_enum, xs) ->
              ()
          );
          tag iiname (Field (Use));
          k x
              

    );

    (* -------------------------------------------------------------------- *)
    V.klvalue = (fun (k,vx) x ->
      match Ast.untype x with
      | Var (dname, aref) ->
          (* see scoping_php.ml *)

          let info = Ast.info_of_dname dname in
          (match !aref with
          | S.Local -> 
              tag info (Local Use)
          | S.Param ->
              tag info (Parameter Use)

          | S.Class -> 
              tag info (Field Use)

          | S.Global ->
              (* TODO, need global_used table *)
              tag info (Global (Use2 fake_no_use2));

          | S.ListBinded ->
              tag info (Local Use)

          | S.LocalIterator
          | S.LocalExn ->
              tag info (Local Use)

          | S.NoScope ->
              tag info (NoType)
          )

      | This (tok) ->
          tag tok (Class (Use2 fake_no_use2))

      | VArrayAccess (var, exprbracket) ->
          (match Ast.unbracket exprbracket with
          | None -> 
              k x
          | Some (exprbis, tbis) ->
              (match exprbis with
              | Sc (C (Ast.String (s, info))) ->
                  tag info (Field Use);
                  vx.V.vlvalue var;

              | Sc (C (Int (s, info))) ->
                  k x
              | _ -> k x
              )
          )

      | FunCallSimple (callname, args) ->
          let info = Ast.info_of_name callname in
          let f = Ast.name callname in

          (* security: *)
          if Hashtbl.mem hbad_functions f 
          then tag info BadSmell;

          db_opt +> Common.do_option (fun (id, current_file, db) ->
          
            let ids = Db.function_ids__of_string f db in

            (* todo? look if macro ? *)
          
            let nbdefs = Highlight_code.arity_ids ids in
            let nbuses = use_arity_ident_function_or_macro f db in
            let place = place_ids current_file ids db in
            tag info (Function (Use2 (place, nbdefs, nbuses)));
          
            (* the args *)
            (match ids with
            | [id] -> 
                let ast = Db.ast_of_id id db in
                (match ast with
                | Ast_entity_php.Function def ->
                    let params = Ast.unparen def.f_params +> Ast.uncomma in
                    let args = Ast.unparen args +> Ast.uncomma in
                    let zipped = Common.zip_safe params args in
                    zipped +> List.iter (fun (param, arg) ->
                      if param.Ast.p_ref = None
                      then vx.V.vargument arg
                      else begin
                        let iis = Lib_parsing_php.ii_of_argument arg in
                        iis +> List.iter (fun ii ->
                          tag ii (CallByRef)
                        )
                      end
                    );
                | _ ->
                    tag info Error;
                )
            | _ -> ()
            )
          );
          if db_opt = None then 
            tag info (Function (Use2 fake_no_use2));

          k x
          

      | MethodCallSimple (lval, tok, name, args) ->
          let info = Ast.info_of_name name in
          tag info (Method (Use2 fake_no_use2));
          k x

      | ObjAccessSimple (lval, tok, name) ->
          let info = Ast.info_of_name name in
          tag info (Field Use);
          k x

      | StaticMethodCallSimple (qualif, name, args) -> 
          let info = Ast.info_of_name name in
          tag info (StaticMethod (Use2 fake_no_use2));
          k x

      | FunCallVar (qu_opt, var, args) ->
          (* function pointer call !!! put in big font *)
          k x

      | ObjAccess (_, _) 
      | VQualifier (_, _)
      | Indirect (_, _)
      | VBraceAccess (_, _)
      | VBrace (_, _)
      | VArrayAccessXhp (_, _) ->
          k x
    );

    V.kobj_dim = (fun (k, vx) x ->
      match x with
      | OName name ->
          let info = Ast.info_of_name name in
          tag info (Field (Use))
      | _ -> k x


    );

    (* -------------------------------------------------------------------- *)
    V.kconstant = (fun (k, vx) e ->
      match e with
      | Int v1 -> 
          tag (snd v1) Number
      | Double v1 -> 
          tag (snd v1) Number
      | Ast.String (s, ii) -> 
          (* this can be sometimes tagged as url, or field access in array *)
          if not (Hashtbl.mem already_tagged ii)
          then 
            tag_string s ii

      | CName name -> 
          (* cf also typing_php.ml *)
          let s = Ast.name name in
          let info = Ast.info_of_name name in
          (match s with
          | "true" -> 
              tag info Boolean
          | "false" -> 
              tag info Boolean
          | "null" -> 
              tag info Null
              
          | _ ->
              (* TODO *)
              tag info (Macro (Use2 fake_no_use2))
          )

      | PreProcess v1 -> 
          tag (snd v1) Builtin
            
      | XdebugClass (_, _) -> 
          ()
      | XdebugResource ->
          ()
    );
    (* -------------------------------------------------------------------- *)
    V.kscalar = (fun (k, vx) sc ->
      match sc with
      | C cst -> 
          k sc
      | ClassConstant (qualif, name) ->
          k sc;
          let info = Ast.info_of_name name in
          tag info (Macro (Use2 fake_no_use2))
      | Guil _ | HereDoc _ -> 
          k sc
    );

    (* -------------------------------------------------------------------- *)
    V.kencaps = (fun (k, vx) e ->
      match e with
      | EncapsString (s, ii) ->
          if not (Hashtbl.mem already_tagged ii)
          then tag_string s ii
      | _ -> k e
    );
    (* -------------------------------------------------------------------- *)
    V.kclass_name_reference = (fun (k, vx) x ->
      match x with 
      | ClassNameRefStatic name ->
          let info = Ast.info_of_name name in
          tag info (Class (Use2 fake_no_use2));
      | ClassNameRefDynamic _ ->
          ()
    );
    (* -------------------------------------------------------------------- *)
    V.kfully_qualified_class_name = (fun (k, vx) x ->
      let info = Ast.info_of_name x in
      tag info (Class (Use2 fake_no_use2));
    );
    (* -------------------------------------------------------------------- *)
    V.kqualifier = (fun (k, vx) x ->
      match x with
      | Self (tok, _)
      | Parent (tok, _) ->
          tag tok (Class (Use2 fake_no_use2));
      | Qualifier _ ->
          k x
    );
    (* -------------------------------------------------------------------- *)

  } 
  in
  let visitor = V.mk_visitor hooks in
  visitor.V.vtop toplevel;

  (* -------------------------------------------------------------------- *)
  (* toks phase 2 *)
  toks +> List.iter (fun tok -> 
    (* all the name and varname should have been tagged by now. *)
     
    match tok with

    | T.EOF ii -> ()
    | T.TUnknown ii -> tag ii Error

    | T.T_WHITESPACE ii -> ()

    (* used ? *)
    | T.TCommentPP ii -> ()
    | T.TComment ii -> ()
    | T.TCommentNewline ii -> ()
    | T.TCommentSpace ii -> ()
   
    (* they should have been covered before *)
    | T.T_VARIABLE (_, ii) -> 
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Error

    | T.T_IDENT (_, ii) ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Error

    
    | T.T_OPEN_TAG ii -> 
        tag ii Keyword

    | T.TOPAR ii   | T.TCPAR ii
    | T.TOBRACE ii | T.TCBRACE ii
    | T.TOBRA ii   | T.TCBRA ii 
      ->
        tag ii Punctuation


    | T.T_COMMENT (ii)  | T.T_DOC_COMMENT (ii)  ->

        assert(is_origintok ii);

        tag ii Comment;

        let _s = Ast.str_of_info ii in
        let _start_pos = Ast.pos_of_info ii in
        (*
        let toks = Parse_comments.tokens_string s in

        toks +> List.iter (fun tok -> 
          match tok with
          | Token_comments.TWord (s, pi) -> 
              let startp = start_pos + pi.Common.charpos in
              let endp = startp + String.length s in

              (match s with
              | _ when List.mem (String.lowercase s)
                    ["interrupts"; "interrupt";
                     "irq"; "irqs";
                     "lock";"locking";"locks";"locked";
                    ] -> 
                  pr2_gen (startp, endp);
                  let (i1, i2) = iter_range_of_range bufinfo (startp, endp) in
                  buffer#apply_tag_by_name 
                    (stag buffer CommentWordImportantNotion) i1 i2

              | _ when List.mem (String.lowercase s)
                    ["must";
                     "should";

                     "hold"; "held";
                     "enabled";"enable";"enables";
                     "disabled";"disable";"disables";
                    ] -> 
                  pr2_gen (startp, endp);
                  let (i1, i2) = iter_range_of_range bufinfo (startp, endp) in
                  buffer#apply_tag_by_name 
                    (stag buffer CommentWordImportantModal) i1 i2

              | _ -> ()
              )

          | _ -> ()
        );
        *)

        ()


      | T.T_XHP_PCDATA ii -> tag ii KeywordObject
      | T.T_XHP_ANY ii -> tag ii KeywordObject
      | T.T_XHP_REQUIRED ii -> tag ii KeywordObject
      | T.T_XHP_ENUM ii -> tag ii KeywordObject
      | T.T_XHP_CATEGORY ii -> tag ii KeywordObject
      | T.T_XHP_CHILDREN ii -> tag ii KeywordObject
      | T.T_XHP_ATTRIBUTE ii -> tag ii KeywordObject

      | T.T_XHP_TEXT (_, ii) -> tag ii String
      | T.T_XHP_ATTR (_, ii) -> tag ii (Field Use)

      | T.T_XHP_CLOSE_TAG (_, ii) -> tag ii EmbededHtml
      | T.T_XHP_SLASH_GT ii -> tag ii EmbededHtml
      | T.T_XHP_GT ii -> tag ii EmbededHtml
      | T.T_XHP_OPEN_TAG (_, ii) -> tag ii EmbededHtml

      (* should have been transformed into a XhpName or XhpInherit in Ast *)

      | T.T_XHP_PERCENTID_DEF (_, ii) -> 
          if not (Hashtbl.mem already_tagged ii)
          then tag ii Error

      | T.T_XHP_COLONID_DEF (_, ii) -> 
          if not (Hashtbl.mem already_tagged ii)
          then tag ii Error

      | T.T_RESOURCE_XDEBUG ii -> ()
      | T.T_CLASS_XDEBUG ii -> ()

      | T.TDOTS ii -> tag ii Punctuation

      | T.TGUIL ii -> tag ii String

      | T.TDOLLAR ii -> tag ii Punctuation
      | T.TSEMICOLON ii -> tag ii Punctuation
      | T.TBACKQUOTE ii -> tag ii Punctuation

      (* we want to highlight code using eval! *)
      | T.T_EVAL ii -> tag ii BadSmell

      | T.T_REQUIRE_ONCE ii -> tag ii Include
      | T.T_REQUIRE ii -> tag ii Include
      | T.T_INCLUDE_ONCE ii -> tag ii Include
      | T.T_INCLUDE ii -> tag ii Include

      | T.T_INSTANCEOF ii -> tag ii KeywordObject
      | T.T_CLONE ii -> tag ii KeywordObject
      | T.T_NEW ii -> tag ii KeywordObject

      | T.T__AT ii -> tag ii Builtin

      | T.T_IS_NOT_EQUAL ii
      | T.T_IS_EQUAL ii
      | T.T_IS_NOT_IDENTICAL ii
      | T.T_IS_IDENTICAL ii
          -> tag ii Operator

      (* done in Cast *)
      | T.T_UNSET_CAST ii
      | T.T_OBJECT_CAST ii
      | T.T_ARRAY_CAST ii
      | T.T_STRING_CAST ii
      | T.T_DOUBLE_CAST ii
      | T.T_INT_CAST ii
      | T.T_BOOL_CAST ii
          -> ()

      | T.T_IS_GREATER_OR_EQUAL ii
      | T.T_IS_SMALLER_OR_EQUAL ii
      | T.T_SR ii
      | T.T_SL ii
      | T.T_LOGICAL_XOR ii
      | T.T_LOGICAL_AND ii
      | T.T_LOGICAL_OR ii
      | T.T_BOOLEAN_AND ii
      | T.T_BOOLEAN_OR ii
      | T.T_DEC ii
      | T.T_INC ii
      | T.T_SR_EQUAL ii
      | T.T_SL_EQUAL ii
      | T.T_XOR_EQUAL ii
      | T.T_OR_EQUAL ii
      | T.T_AND_EQUAL ii
      | T.T_MOD_EQUAL ii
      | T.T_CONCAT_EQUAL ii
      | T.T_DIV_EQUAL ii
      | T.T_MUL_EQUAL ii
      | T.T_MINUS_EQUAL ii
      | T.T_PLUS_EQUAL ii
      | T.TGREATER ii
      | T.TSMALLER ii
      | T.TEQ ii
      | T.TXOR ii
      | T.TOR ii
      | T.TAND ii
      | T.TMOD ii
      | T.TDIV ii
      | T.TMUL ii
      | T.TMINUS ii
      | T.TPLUS ii
          -> tag ii Operator

      | T.TQUESTION ii
      | T.TTILDE ii
      | T.TBANG ii
      | T.TDOT ii
      | T.TCOMMA ii
      | T.TCOLON ii
      | T.TCOLCOL ii
          -> tag ii Punctuation

      | T.T_CURLY_OPEN ii -> tag ii Punctuation
      | T.T_DOLLAR_OPEN_CURLY_BRACES ii -> tag ii Punctuation

      | T.T_END_HEREDOC ii -> tag ii Punctuation
      | T.T_START_HEREDOC ii -> tag ii Punctuation

      | T.T_CLOSE_TAG_OF_ECHO ii -> tag ii Punctuation
      | T.T_OPEN_TAG_WITH_ECHO ii -> tag ii Punctuation

      | T.T_CLOSE_TAG ii -> tag ii Punctuation

      (* done in PreProcess *)
      | T.T_FILE ii
      | T.T_LINE ii
      | T.T_FUNC_C ii
      | T.T_METHOD_C ii
      | T.T_CLASS_C ii
          -> ()

      (* can be a type hint *)
      | T.T_ARRAY ii -> 
          if not (Hashtbl.mem already_tagged ii)
          then 
            tag ii Builtin

      | T.T_LIST ii -> tag ii Builtin

      | T.T_DOUBLE_ARROW ii -> 
          tag ii Punctuation
      | T.T_OBJECT_OPERATOR ii -> 
          tag ii Punctuation

      | T.T_IMPLEMENTS ii -> tag ii KeywordObject
      | T.T_EXTENDS ii -> tag ii KeywordObject
      | T.T_INTERFACE ii -> tag ii KeywordObject

      | T.T_CLASS ii -> tag ii KeywordObject

      | T.T_HALT_COMPILER ii -> tag ii Builtin

      | T.T_EMPTY ii -> tag ii Builtin
      | T.T_ISSET ii -> tag ii Builtin
      | T.T_UNSET ii -> tag ii Builtin

      | T.T_VAR ii -> tag ii Keyword
      | T.T_PUBLIC ii -> tag ii Keyword
      | T.T_PROTECTED ii -> tag ii Keyword
      | T.T_PRIVATE ii -> tag ii Keyword

      | T.T_FINAL ii -> tag ii KeywordObject
      | T.T_ABSTRACT ii -> tag ii KeywordObject

      | T.T_STATIC ii -> tag ii Keyword

      | T.T_CONST ii -> tag ii Keyword

      (* could be for func or method so tagged via ast *)
      | T.T_FUNCTION ii -> ()

      | T.T_AS ii -> tag ii Keyword
      | T.T_GLOBAL ii -> tag ii Keyword
      | T.T_USE ii -> tag ii Keyword
      | T.T_ENDDECLARE ii -> tag ii Keyword
      | T.T_DECLARE ii -> tag ii Keyword
      | T.T_EXIT ii -> tag ii Keyword

      | T.T_THROW ii -> tag ii KeywordExn
      | T.T_CATCH ii -> tag ii KeywordExn
      | T.T_TRY ii -> tag ii KeywordExn

      | T.T_RETURN ii -> tag ii Keyword
      | T.T_CONTINUE ii -> tag ii Keyword
      | T.T_BREAK ii -> tag ii Keyword

      | T.T_DEFAULT ii -> tag ii Keyword
      | T.T_CASE ii -> tag ii Keyword

      | T.T_ENDSWITCH ii -> tag ii KeywordConditional
      | T.T_SWITCH ii -> tag ii KeywordConditional

      | T.T_ENDFOREACH ii -> tag ii KeywordLoop
      | T.T_FOREACH ii -> tag ii KeywordLoop
      | T.T_ENDFOR ii -> tag ii KeywordLoop
      | T.T_FOR ii -> tag ii KeywordLoop
      | T.T_ENDWHILE ii -> tag ii KeywordLoop
      | T.T_WHILE ii -> tag ii KeywordLoop
      | T.T_DO ii -> tag ii KeywordLoop

      | T.T_IF ii -> tag ii KeywordConditional
      | T.T_ELSEIF ii -> tag ii KeywordConditional
      | T.T_ELSE ii -> tag ii KeywordConditional
      | T.T_ENDIF ii -> tag ii KeywordConditional

      | T.T_PRINT ii -> tag ii Builtin
      | T.T_ECHO ii -> tag ii Builtin

      | T.T_BAD_CHARACTER ii -> tag ii Error
      | T.T_CHARACTER ii -> tag ii String

      (* should have been handled in field *)
      | T.T_STRING_VARNAME ii -> ()

      | T.T_INLINE_HTML (_, ii) -> tag ii EmbededHtml

      | T.T_NUM_STRING ii -> ()

      | T.T_ENCAPSED_AND_WHITESPACE (s, ii) -> 
          if not (Hashtbl.mem already_tagged ii)
          then tag_string s ii

      | T.T_CONSTANT_ENCAPSED_STRING (s, ii) -> 
          if not (Hashtbl.mem already_tagged ii)
          then tag_string s ii

      (* should been handled in Constant *)
      | T.T_DNUMBER ii -> ()
      | T.T_LNUMBER ii -> ()

  );



  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)  
  (match toplevel with
  | NotParsedCorrectly iis -> 
      (*
        let (max,min) = Lib_parsing_c.max_min_ii_by_pos ii in
        let i1 = iterline_of_info bufinfo min in
        let i2 = iterline_of_info bufinfo max in
      *)
      iis +> List.iter (fun ii -> tag ii NotParsed)

  | _ -> ()
  );
  ()
