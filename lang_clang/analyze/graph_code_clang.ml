(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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

module E = Database_code
module G = Graph_code

open Ast_clang
open Parser_clang

module Ast = Ast_clang
module Loc = Location_clang
module Typ = Type_clang

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for Clang ASTs. See graph_code.ml and
 * main_codegraph.ml for more information.
 * 
 * 
 * schema:
 *  Root -> Dir -> File (.c|.h) -> Function | Prototype
 *                              -> Global | GlobalExtern
 *                              -> Type (for Typedef)
 *                              -> Type (struct|enum|union) TODO use Class?
 *                                 -> Field
 *                                 -> Constant (enum)
 *       -> Dir -> SubDir -> ...
 * 
 * procedure to analyze a project:
 *  $ make V=1 > make_trace.txt
 *  $ ~/pfff/pfff_test -analyze_make_trace make_trace.txt > compile_commands.json
 *  $ ~/pfff/pfff -gen_clang compile_commands.json 
 *  $ ~/pfff/pfff_test -uninclude_clang
 *  $ ~/pfff/codegraph -lang clang2 -build .
 * 
 * alternative when project uses cmake:
 *  $ cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON 
 *  $ mv compile_commands.json old.json
 *  $ ~/pfff/pfff -sanitize_compile_commands old.json > compile_commands.json
 *  $ ~/pfff/pfff -gen_clang ...
 *  $ ...
 * 
 * related:
 *  - http://code.google.com/p/include-what-you-use/wiki/WhyIWYU
 * 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  g: Graph_code.graph;
  phase: phase;

  cnt: int ref;

  current: Graph_code.node;

  root: Common.dirname;
  (* readable path *)
  c_file_readable: Common.filename;

  current_c_file_line: int ref;
  (* this is there in support for current_c_file_line, see update_line() *)
  current_c_file: Common.filename ref;
  (* absolute path, as mentionned in the .clang *)
  target_c_file: Common.filename;

  (* for error reports *)
  clang2_file: Common.filename;
  (* line number in .clang file (not .c file) *)
  clang_line: int;

  at_toplevel: bool;
  (* we don't need to store also the params as they are marked specially
   * as ParamVar in the AST.
   *)
  locals: string list ref;
  (* static functions and globals and main renaming *)
  local_rename: (string, string) Hashtbl.t;

  log: string -> unit;
  pr2_and_log: string -> unit;
}
 and phase = Defs | Uses

let unknown_location = "Unknown_Location", E.File

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

let parse2 file = 
  (* clang2_old: Parse_clang.parse file *)
  Common2.get_value file
let parse a = 
  Common.profile_code "Parse_clang.parse" (fun () -> parse2 a)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let new_str_if_defs env s =
  if env.phase = Defs
  then begin
    incr env.cnt;
    let s2 = spf "%s__%d" s !(env.cnt) in
    Hashtbl.add env.local_rename s s2;
    s2
  end
  else Hashtbl.find env.local_rename s

let str env s =
  if Hashtbl.mem env.local_rename s
  then Hashtbl.find env.local_rename s
  else s

let loc_of_env env =
  env.clang2_file, env.clang_line

let error env s =
  Errors_clang.error (loc_of_env env) s

let str_of_angle_loc env loc =
  Location_clang.str_of_angle_loc env.clang_line loc env.clang2_file

let update_current_c_file_line env (enum, l, xs) =
  let locations = 
    Loc.locations_of_paren env.clang2_file (enum, l, xs) in
  let update_line_if_same_file l =
    if !(env.current_c_file) =$= env.target_c_file
    then env.current_c_file_line := l
  in

  let rec aux = function
    | [] -> ()
    (* for range, we care about the first position, so discard second Line *)
    | [Loc.File (file, l, _col);Loc.Line _] ->
      env.current_c_file := file;
      update_line_if_same_file l
    | [Loc.Line (l, _col);Loc.Line _] ->
      update_line_if_same_file l
    | x::xs ->
      (match x with
      | Loc.File (file, l, _col) ->
        env.current_c_file := file;
        update_line_if_same_file l
      | Loc.Line (l, _col) ->
        update_line_if_same_file l
      | Loc.Col _ | Loc.Other -> ()
      );
      aux xs
  in
  aux locations

(*****************************************************************************)
(* Add Node *)
(*****************************************************************************)

let add_node_and_edge_if_defs_mode env node =
  let (str, kind) = node in
  let str' =
    match kind, env.current with
    | E.Field, (s, E.Type) ->
        s ^ "." ^ str
    | _ -> str
  in
  let node = (str', kind) in

  if env.phase = Defs then begin
    if G.has_node node env.g
    then
      (match kind with
      | E.Function | E.Global | E.Constant 
      | E.Type
      | E.Field
          ->
          (match kind, str with
          | E.Type, (
              (* clang builtins *)
                "T____int128_t" | "T____uint128_t"  | "T____builtin_va_list"
              (* /usr/include dupes. todo: could look if same def body and
               * also if both duped entities are in EXTERNAL/
               *)
              | "T__pid_t" | "T__intptr_t" | "T__off_t" | "T__ssize_t"
              | "T__dev_t" | "T__mode_t"
            )
              -> ()
          | _ when env.clang2_file =~ ".*EXTERNAL" -> ()
          | _ ->
              env.pr2_and_log (spf "DUPE entity: %s" (G.string_of_node node));
              let nodeinfo = G.nodeinfo node env.g in
              let orig_file = nodeinfo.G.pos.Parse_info.file in
              env.log (spf " orig = %s" orig_file);
              env.log (spf " dupe = %s" env.clang2_file);
          )
      (* todo: have no Use for now for those so skip errors *) 
      | E.Prototype | E.GlobalExtern -> ()
      | _ ->
          failwith (spf "Unhandled category: %s" (G.string_of_node node))
      )
    else begin
      try
        let nodeinfo = { Graph_code.
          pos = { Parse_info.
            str = "";
            charpos = -1; column = -1;
            line = !(env.current_c_file_line); 
            file = env.c_file_readable;
          };
          props = [];
        } in
        env.g +> G.add_node node;
        env.g +> G.add_edge (env.current, node) G.Has;
        env.g +> G.add_nodeinfo node nodeinfo;
      with Not_found ->
        error env ("Not_found:" ^ str)
    end
  end;
  { env with current = node }

(*****************************************************************************)
(* Add edge *)
(*****************************************************************************)

let rec add_use_edge env (s, kind) =
  let src = env.current in
  let dst = (s, kind) in
  if not (G.has_node src env.g)
  then error env ("SRC FAIL:" ^ G.string_of_node src);

  if G.has_node dst env.g
  then G.add_edge (src, dst) G.Use env.g
  else 
    (match kind with
    (* look for Prototype if no Function *)
    | E.Function -> add_use_edge env (s, E.Prototype)
    (* look for GlobalExtern if no Global *)
    | E.Global -> add_use_edge env (s, E.GlobalExtern)
    | _ when env.clang2_file =~ ".*EXTERNAL" -> ()
    (* todo? if we use 'b' in the 'a':'b' type string, still need code below?*)
    | E.Type when s =~ "S__\\(.*\\)" ->
        add_use_edge env ("T__" ^ Common.matched1 s, E.Type)
    | E.Type when s =~ "U__\\(.*\\)" ->
        add_use_edge env ("T__" ^ Common.matched1 s, E.Type)
    | E.Type when s =~ "E__\\(.*\\)" ->
        add_use_edge env ("T__" ^ Common.matched1 s, E.Type)
    | _ ->
        env.pr2_and_log (spf "Lookup failure on %s (%s:%d)"
                            (G.string_of_node dst)
                            env.clang2_file
                            env.clang_line
        )
    )
      
let add_type_deps env typ =
  match typ with
  (* in a codegraph context we want to use the original type, not the
   * final type as we want to create dependencies to typedefs
   *)
  | Brace (toks, _toks_final) ->
      if env.phase = Uses then begin
        let t = Type_clang.extract_type_of_tokens (loc_of_env env) toks in
        let rec aux t = 
          match t with
          | Typ.Builtin _ -> ()
              
          | Typ.StructName s ->
              add_use_edge env ("S__"^s, E.Type)
          | Typ.UnionName s ->
              add_use_edge env ("U__"^s, E.Type)
          | Typ.EnumName s ->
              add_use_edge env ("E__"^s, E.Type)
          | Typ.Typename s ->
              add_use_edge env ("T__"^s, E.Type)

           (* less: use the canonical type in that case? *)
          | Typ.TypeofStuff -> ()
               
          | Typ.AnonStuff -> ()
          | Typ.Other _ -> ()
          | Typ.Pointer x-> aux x
          (* todo: should analyze parameters *)
          | Typ.Function x -> aux x
        in
        aux t
      end
  | T (TString s) ->
      failwith "you're using an old version of the AST dumper, apply patch"
  | _ ->
      error env "wrong type format"

(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)
let rec extract_defs_uses env ast =

  let c_file =
    if env.clang2_file =~ "\\(.*\\).clang2"
    then Common.matched1 env.clang2_file
    else failwith "not a clang2 file?"
  in
  let c_file_readable = Common.readable ~root:env.root c_file in

  if env.phase = Defs then begin
    let dir = Common2.dirname c_file_readable in
    G.create_intermediate_directories_if_not_present env.g dir;
    let node = (c_file_readable, E.File) in
    env.g +> G.add_node node;
    env.g +> G.add_edge ((dir, E.Dir), node) G.Has;
  end;
  let env = { env with 
    current = (c_file_readable, E.File);
    c_file_readable;
    current_c_file_line = ref 1;

    target_c_file = c_file;
    current_c_file = ref c_file;
  } in
  match ast with
  | Paren (TranslationUnitDecl, l, _loc::xs) ->
      List.iter (sexp_toplevel env) xs
  | _ -> 
      error env "not a TranslationDecl"
  

and sexp_toplevel env x =
  match x with
  | Paren (enum, l, xs) ->
      let env = { env with clang_line = l } in
      update_current_c_file_line env (enum, l, xs);

      (* dispatcher *)
      (match enum with
      | FunctionDecl | VarDecl
      | TypedefDecl | RecordDecl | EnumDecl 
      | FieldDecl | EnumConstantDecl
        -> decl env (enum, l, xs)
      | LinkageSpecDecl ->
          (match xs with
          | _loc::T (TUpperIdent "C")::xs ->
              xs +> List.iter (sexp_toplevel env)
          | _ -> error env "weird LinkageSpecDecl"
          )

      | CallExpr | DeclRefExpr | MemberExpr
        -> expr env (enum, l, xs)
      | _ -> 
          sexps env xs
      )
  | Angle (xs) ->
      sexps env xs
  | Anchor (xs) ->
      sexps env xs
  | Bracket (xs) ->
      sexps env xs
  | Brace (xs, _) ->
      ()
  | T tok ->
      ()

and sexp env x =
  sexp_toplevel { env with at_toplevel = false} x

and sexps env xs = List.iter (sexp env) xs

(* ---------------------------------------------------------------------- *)
(* Decls *)
(* ---------------------------------------------------------------------- *)

(* coupling: must add constructor in dispatcher above *)
and decl env (enum, l, xs) =
  let env =
    match enum, xs with
    | FunctionDecl, loc::(T (TLowerIdent s | TUpperIdent s))::typ::rest->
        let kind = 
          if rest +> List.exists (function 
          | Paren (CompoundStmt, _, _) -> true
          | _ -> false
          )
          then E.Function
          else E.Prototype
        in
        let static = 
          match rest with
          (* if we are in an header file, then we don't want to rename
           * the inline static function because of uninclude_clang which
           * splitted in different files, and so with different
           * local_rename hash. Renaming in the header file would lead to
           * some unresolvev lookup in the c files.
           *)
          | T (TLowerIdent "static")::T (TLowerIdent "inline")::_rest ->
              env.clang2_file =~ ".*\\.[cm]\\.clang2"
          | T (TLowerIdent "static")::_rest -> true
          | _ when s = "main" -> true
          | _ -> false
        in
        let s = 
          if static && kind = E.Function
          then new_str_if_defs env s
          else s
        in
        let env = add_node_and_edge_if_defs_mode env (s, kind) in
        add_type_deps env typ;
        { env with locals = ref [] }

    | VarDecl, loc::(T (TLowerIdent s | TUpperIdent s))::typ::rest ->
        let kind =
          match rest with
          | T (TLowerIdent "extern")::_ -> E.GlobalExtern
          | _ -> E.Global
        in
        let static = 
          match rest with
          (* if we are in an header file, then we don't want to rename
           * the static global because of uninclude_clang which
           * splitted in different files, and so with different
           * local_rename hash. Renaming in the header file would lead to
           * some unresolved lookup in the c files.
           *)
          | T (TLowerIdent "static")::_rest ->
              env.clang2_file =~ ".*\\.[cm]\\.clang2"
          | _ -> false
        in
        let env =
          if env.at_toplevel 
          then 
            let s = 
              if static
              then new_str_if_defs env s
              else s
            in
            add_node_and_edge_if_defs_mode env (s, kind)
          else begin 
            env.locals := s::!(env.locals);
            env
          end
        in
        add_type_deps env typ;
        env
        

    (* I am not sure about the namespaces, so I prepend strings *)
    | TypedefDecl, loc::(T (TLowerIdent s | TUpperIdent s))::typ::_rest ->
        let env = add_node_and_edge_if_defs_mode env ("T__" ^ s, E.Type) in
        add_type_deps env typ;
        env
        
    | EnumDecl, loc::(T (TLowerIdent s | TUpperIdent s))::_rest ->
        add_node_and_edge_if_defs_mode env ("E__" ^ s, E.Type) 

    (* ignore forward decl, to avoid duped entities *)
    | RecordDecl, loc::(T (TLowerIdent "struct"))
        ::(T (TLowerIdent s | TUpperIdent s))::[] ->
        env
          
    | RecordDecl, loc::(T (TLowerIdent "struct"))
        ::(T (TLowerIdent s | TUpperIdent s))::_rest ->
        add_node_and_edge_if_defs_mode env ("S__" ^ s, E.Type)
    | RecordDecl, loc::(T (TLowerIdent "union"))
        ::(T (TLowerIdent s | TUpperIdent s))::_rest ->
        add_node_and_edge_if_defs_mode env ("U__" ^ s, E.Type)
          
    (* usually embedded struct *)
    | RecordDecl, loc::(T (TLowerIdent "struct"))::_rest ->
        add_node_and_edge_if_defs_mode env 
          (spf "S__anon__%s" (str_of_angle_loc env loc), E.Type)
          
    (* todo: usually there is a typedef just behind *)
    | EnumDecl, loc::_rest ->
        add_node_and_edge_if_defs_mode env 
          (spf "E__anon__%s" (str_of_angle_loc env loc), E.Type)
    | RecordDecl, loc::(T (TLowerIdent "union"))::_rest ->
        add_node_and_edge_if_defs_mode env 
          (spf "U__anon__%s" (str_of_angle_loc env loc), E.Type)

    | FieldDecl, loc::(T (TLowerIdent s | TUpperIdent s))::typ::_rest ->
        let env = add_node_and_edge_if_defs_mode env (s, E.Field) in 
        add_type_deps env typ;
        env

    | FieldDecl, loc::_rest ->
        add_node_and_edge_if_defs_mode env 
          (spf "F__anon__%s" (str_of_angle_loc env loc), E.Field)
    | EnumConstantDecl, loc::(T (TLowerIdent s | TUpperIdent s))::_rest ->
        add_node_and_edge_if_defs_mode env (s, E.Constant)
        
    | _ -> error env "wrong Decl line" 
  in
  sexps env xs

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)

(* coupling: must add constructor in dispatcher above if add one here *)
and expr env (enum, l, xs) =
  (match enum, xs with
  | CallExpr, _loc::_typ::
      (Paren (ImplicitCastExpr, _l2, 
             _loc2::_typ2::Angle _::
               (Paren (DeclRefExpr, _l3,
                      _loc3::_typ3::T (TUpperIdent "Function")::T (THexInt _)
                        ::T (TString s)::_typ4::[]))::[]))
      ::_args ->
      if env.phase = Uses
      then 
        let s = str env s in
        add_use_edge env (s, E.Function)

  (* todo: unexpected form of call? function pointer call? *)
  | CallExpr, _ ->
      ()



  | DeclRefExpr, _loc::_typ::T (TUpperIdent "EnumConstant")::_address
      ::T (TString s)::_rest ->
      if env.phase = Uses
      then add_use_edge env (s, E.Constant)

  | DeclRefExpr, _loc::_typ::_lval::T (TUpperIdent "Var")::_address
      ::T (TString s)::_rest ->
      if env.phase = Uses
      then
        if List.mem s !(env.locals)
        then ()
        else 
          let s = str env s in
          add_use_edge env (s, E.Global)

  | DeclRefExpr, _loc::_typ::_lval::T (TUpperIdent "ParmVar")::_rest ->
      ()

  | DeclRefExpr, _loc::_typ::T (TUpperIdent "Function")::_address
      ::T (TString s)::_rest 
  | DeclRefExpr, _loc::_typ::T (TLowerIdent "lvalue")
      ::T (TUpperIdent "Function")::_address
      ::T (TString s)::_rest 
      ->
      if env.phase = Uses
      then 
        let s = str env s in
        add_use_edge env (s, E.Function)

  | DeclRefExpr, _loc::_typ::T (TLowerIdent "lvalue")
      ::T (TUpperIdent "CXXMethod")::_rest
      -> pr2_once "TODO: CXXMethod"
     
  | DeclRefExpr, _ -> error env "DeclRefExpr to handle"



  | MemberExpr, [_loc;_typ;_(*lval*);T (TDot|TArrow);
                 T (TLowerIdent fld|TUpperIdent fld);
                 _address;(Paren (enum2, l2, xs))]
  | MemberExpr, [_loc;_typ;T (TDot|TArrow);
                 T (TLowerIdent fld | TUpperIdent fld);
                 _address;(Paren (enum2, l2, xs))]
  | MemberExpr, [_loc;_typ;_(*lval*);T (TLowerIdent "bitfield");T(TDot|TArrow);
                 T (TLowerIdent fld | TUpperIdent fld);
                 _address;(Paren (enum2, l2, xs))] ->
      if env.phase = Uses
      then
        let loc = env.clang2_file, l2 in
        let typ_expr = 
          Type_clang.extract_canonical_type_of_sexp loc (Paren(enum2, l2, xs))
        in
        (match typ_expr with
        (* because TDot|TArrow above, need Pointer too *)
        | Typ.StructName s | Typ.Pointer (Typ.StructName s) ->
            (* no lookup for now *)
            add_use_edge env (spf "S__%s.%s" s fld, E.Field)

        (* with some struct anon this can happen apparently, cf umalloc.c *)
        | Typ.Typename _ | Typ.Pointer (Typ.Typename _) ->
            (* TODO *)
            ()

        | Typ.TypeofStuff | Typ.Pointer (Typ.TypeofStuff) ->
            (* use canonical type, should never get there *)
            error env ("impossible")

        | Typ.UnionName s  | Typ.Pointer (Typ.UnionName s) ->
            ()
        | Typ.AnonStuff | Typ.Pointer (Typ.AnonStuff) ->
            ()

        | (Typ.Builtin _
          |Typ.Function _
          |Typ.EnumName _
          |Typ.Other _

          |Typ.Pointer _
          ) ->
            error env (spf "unhandled typ: %s" (Common.dump typ_expr))
        )

  (* anon field *)
  | MemberExpr, _loc::_typ::_lval::T (TDot|TArrow)::
      _address::(Paren (enum2, l2, xs))::[] ->
      if env.phase = Uses
      then ()

  | MemberExpr, _ -> error env "MemberExpr to handle"
  | _ -> raise Impossible
  );
  sexps env xs

(* ---------------------------------------------------------------------- *)
(* Types *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) root files =
  if null files 
  then failwith "no .clang2 files, run pfff -uninclude_clang";

  let g = G.create () in
  G.create_initial_hierarchy g;

  let chan = open_out (Filename.concat root "pfff.log") in
  let local_renames = Hashtbl.create 101 in

  let env = {
    g;
    phase = Defs;
    current = unknown_location;

    c_file_readable = "__filled_later__";
    clang2_file = "__filled_later__";

    current_c_file_line = ref 1;
    current_c_file = ref "__filled_later__";
    target_c_file = "__filled_later__";

    clang_line = -1;
    cnt = ref 0;
    root = root;
    at_toplevel = true;
    local_rename = Hashtbl.create 0;
    locals = ref [];

    log = (fun s ->
        output_string chan (s ^ "\n");
        flush chan;
    );
    pr2_and_log = (fun s ->
      (*if verbose then *)
      pr2 s;
      output_string chan (s ^ "\n");
      flush chan;
    );
  } in

  G.add_node unknown_location g;
  G.add_edge (G.not_found, unknown_location) G.Has g;
  
  (* step1: creating the nodes and 'Has' edges, the defs *)
  env.pr2_and_log "\nstep1: extract defs";
  files +> Console.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let ast = parse file in
      let h = Hashtbl.create 101 in
      Hashtbl.add local_renames file h;
      extract_defs_uses { env with 
        phase = Defs; 
        clang2_file = file;
        local_rename = h;
      } ast
   ));

  (* step2: creating the 'Use' edges *)
  env.pr2_and_log "\nstep2: extract Uses";
  files +> Console.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let ast = parse file in
      extract_defs_uses { env with 
        phase = Uses; 
        clang2_file = file;
        local_rename = Hashtbl.find local_renames file;
      } ast
    ));
  g
