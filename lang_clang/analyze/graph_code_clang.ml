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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for Clang ASTs. See graph_code.ml and
 * main_codegraph.ml for more information.
 * 
 * 
 * schema:
 *  Root -> Dir -> File (.c|.h) -> Type (struct | enum | union) TODO use Class?
 *                                 -> Field
 *                                 -> Constant (enum)
 *                              -> Function | Prototype
 *                              -> Type (for Typedef)
 *                              -> Global | GlobalExtern
 *       -> Dir -> SubDir -> ...
 * 
 * todo: 
 *  - Use for fields
 * 
 * procedure:
 *  $ make V=1 > make_trace.txt
 *  $ ~/pfff/pfff -analyze_make_trace make_trace.txt > compile_commands.json
 *  $ ~/pfff/pfff -gen_clang compile_commands.json 
 *  $ ~/pfff/pfff -uninclude_clang
 *  $ ~/pfff/codegraph -lang clang2 -build .
 * 
 * alt:
 *  $ cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON 
 *  $ mv compile_commands.json old.json
 *  $ ~/pfff/pfff -sanitize_compile_commands old.json > compile_commands.json
 *  ...
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

  (* for error reports *)
  current_clang2_file: Common.filename;
  line: int;

  at_toplevel: bool;
  (* we don't need to store also the params as they are marked specially
   * as ParamVar in the AST.
   *)
  locals: string list ref;
  
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

let error env s =
  failwith (spf "%s:%d: %s" env.current_clang2_file env.line s)

let str_of_angle_loc env loc =
  Location_clang.str_of_angle_loc env.line loc env.current_clang2_file

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
          | _ when env.current_clang2_file =~ ".*EXTERNAL" -> ()
          | _ ->
              env.pr2_and_log (spf "DUPE entity: %s" (G.string_of_node node));
              let nodeinfo = G.nodeinfo node env.g in
              let orig_file = nodeinfo.G.pos.Parse_info.file in
              env.log (spf " orig = %s" orig_file);
              env.log (spf " dupe = %s" env.current_clang2_file);
          )
      (* todo: have no Use for now for those so skip errors *) 
      | E.Prototype | E.GlobalExtern -> ()
      | E.Field -> ()
      | _ ->
          failwith (spf "Unhandled category: %s" (G.string_of_node node))
      )
    else begin
      try
        let nodeinfo = { Graph_code.
          pos = { Parse_info.
            str = "";
            charpos = -1;
            line = -1; column = -1;
            file = env.current_clang2_file;
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
    | _ when env.current_clang2_file =~ ".*EXTERNAL" -> ()
    | _ ->
        env.pr2_and_log (spf "Lookup failure on %s (in %s)"
                            (G.string_of_node dst)
                            (env.current_clang2_file))
    )
      
let builtin_types = Common.hashset_of_list [
  "char";
  "int";"short";"long";
  "float";"double";
  "void";
  "unsigned";"signed";

  "const";"restrict";"volatile";

  "noreturn";"__attribute__";
  (* clang *)
  "__int128";
  "__va_list_tag";
  (* todo: ugly, because of stdbool.h skip? but now that use ExpansionLoc,
   * don't need that anymore? apparently still need it :(
   *)
  "_Bool";

  (* otherwise get wierd edges from EXTERNAL to the source. e.g. in byacc *)
  "__builtin_va_list";
]
let add_type_deps env typ =
  match typ with
  | T (TString s) ->
      if env.phase = Uses then begin
        try 
          let xs = Parse_clang.tokens_of_string s in
          let rec aux xs =
            match xs with
            | [] -> ()
            | TLowerIdent "struct"::(TLowerIdent s | TUpperIdent s)::rest ->
                add_use_edge env ("S__"^s, E.Type);
                aux rest
            | TLowerIdent "union"::(TLowerIdent s | TUpperIdent s)::rest ->
                add_use_edge env ("U__"^s, E.Type);
                aux rest
            | TLowerIdent "enum"::(TLowerIdent s | TUpperIdent s)::rest ->
                add_use_edge env ("E__"^s, E.Type);
                aux rest
            | (TLowerIdent s | TUpperIdent s)::rest ->
                (if Hashtbl.mem builtin_types s
                then ()
                else add_use_edge env ("T__"^s, E.Type)
                );
                aux rest
            | x::xs ->
                aux xs
          in
          aux xs
        with Lexer_clang.Lexical s ->
          error env s
      end
  | _ ->
      error env "wrong type format"

(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)
let rec extract_defs_uses env ast =
  let readable = 
    Common.filename_without_leading_path env.root env.current_clang2_file in
  let readable =
    if readable =~ "\\(.*\\).clang2"
    then Common.matched1 readable
    else readable
  in

  if env.phase = Defs then begin
    let dir = Common2.dirname readable in
    G.create_intermediate_directories_if_not_present env.g dir;
    let node = (readable, E.File) in
    env.g +> G.add_node node;
    env.g +> G.add_edge ((dir, E.Dir), node) G.Has;
  end;
  let env = { env with current = (readable, E.File) } in
  match ast with
  | Paren (TranslationUnitDecl, l, _loc::xs) ->
      List.iter (sexp_toplevel env) xs
  | _ -> 
      error env "not a TranslationDecl"
  

and sexp_toplevel env x =
  match x with
  | Paren (enum, l, xs) ->
      let env = { env with line = l } in

      (* dispatch *)
      (match enum with
      | FunctionDecl | VarDecl
      | TypedefDecl | RecordDecl | EnumDecl 
      | FieldDecl | EnumConstantDecl
        -> decl env (enum, l, xs)
      | CallExpr | DeclRefExpr
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
  | T tok ->
      ()

and sexp env x =
  sexp_toplevel { env with at_toplevel = false} x

and sexps env xs = List.iter (sexp env) xs

(* ---------------------------------------------------------------------- *)
(* Decls *)
(* ---------------------------------------------------------------------- *)

and decl env (enum, l, xs) =
  let env =
    match enum, xs with
    | FunctionDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::typ::rest->
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
              env.current_clang2_file =~ ".*\\.c\\.clang2"
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

    | VarDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::typ::rest ->
        let kind =
          match rest with
          | T (TLowerIdent "extern")::_ -> E.GlobalExtern
          | _ -> E.Global
        in
        let env =
          if env.at_toplevel 
          then add_node_and_edge_if_defs_mode env (s, kind)
          else begin 
            env.locals := s::!(env.locals);
            env
          end
        in
        add_type_deps env typ;
        env
        

    (* I am not sure about the namespaces, so I prepend strings *)
    | TypedefDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::typ::_rest ->
        let env = add_node_and_edge_if_defs_mode env ("T__" ^ s, E.Type) in
        add_type_deps env typ;
        env
        
    | EnumDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::_rest ->
        add_node_and_edge_if_defs_mode env ("E__" ^ s, E.Type)
          
    | RecordDecl, _loc::(T (TLowerIdent "struct"))
        ::(T (TLowerIdent s | TUpperIdent s))::_rest ->
        add_node_and_edge_if_defs_mode env ("S__" ^ s, E.Type)
    | RecordDecl, _loc::(T (TLowerIdent "union"))
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

    | FieldDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::typ::_rest ->
        let env = add_node_and_edge_if_defs_mode env (s, E.Field) in
        add_type_deps env typ;
        env

    | FieldDecl, loc::_rest ->
        add_node_and_edge_if_defs_mode env 
          (spf "F__anon__%s" (str_of_angle_loc env loc), E.Field)
    | EnumConstantDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::_rest ->
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
        else add_use_edge env (s, E.Global)

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

let build ?(verbose=true) dir skip_list =
  let root = Common.realpath dir in
  let all_files = Lib_parsing_clang.find_source2_files_of_dir_or_files [root] in
  if null all_files 
  then failwith "no .clang2 files, run pfff -uninclude_clang";

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files skip_list root all_files in
  (* step0: reorder files *)
  let files = Skip_code.reorder_files_skip_errors_last skip_list root files in

  let g = G.create () in
  G.create_initial_hierarchy g;

  let chan = open_out (Filename.concat (Sys.getcwd()) "pfff.log") in
  let local_renames = Hashtbl.create 101 in

  let env = {
    g;
    phase = Defs;
    current = unknown_location;

    current_clang2_file = "__filled_later__";

    line = -1;
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
  files +> Common_extra.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let ast = parse file in
      let h = Hashtbl.create 101 in
      Hashtbl.add local_renames file h;
      extract_defs_uses { env with 
        phase = Defs; 
        current_clang2_file = file;
        local_rename = h;
      } ast
   ));

  (* step2: creating the 'Use' edges *)
  env.pr2_and_log "\nstep2: extract Uses";
  files +> Common_extra.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let ast = parse file in
      extract_defs_uses { env with 
        phase = Uses; 
        current_clang2_file = file;
        local_rename = Hashtbl.find local_renames file;
      } ast
    ));
  g
