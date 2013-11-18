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
 * This assumes 'clang-check --ast-dump' generate realpath path.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = {
  hfile: 
    (Common.filename, (Ast.enum * string * variant) Common.hashset) Hashtbl.t;
  hfile_data:
    (Common.filename, sexp list) Hashtbl.t;

  (* for readable_filename *)
  root: Common.dirname;
  current_c_file: Common.filename ref;

  (* for error report *)
  current_clang_file: Common.filename;
}
 and variant = Proto | Def | Misc

let unknown_loc = "unknown_loc"

(*****************************************************************************)
(* Helper *)
(*****************************************************************************)
let str_of_angle_loc env l loc =
  (* for the includer, we must use enc.current_c_file, not
   * env.current_clang_file because we actually want to dedupe
   *)
  Location_clang.str_of_angle_loc l loc !(env.current_c_file)

let update_current_c_file env (enum, l, xs) =
  (* less: dupe with below *)
  let file_opt = 
    Loc.readable_filename_location_of_paren_opt 
      env.root env.current_clang_file (enum, l, xs) 
  in
  file_opt +> Common.do_option (fun f ->
    env.current_c_file := f;
    if not (Hashtbl.mem env.hfile f)
    then begin
      Hashtbl.add env.hfile f (Hashtbl.create 10);
      Hashtbl.add env.hfile_data f [];
    end;
  )
  
(*****************************************************************************)
(* Accumulating *)
(*****************************************************************************)
let add_if_not_already_there env (enum, s, v) sexp =
  let c_file = !(env.current_c_file) in
  let hset = 
    try 
      Hashtbl.find env.hfile c_file 
    with Not_found ->
      failwith (spf "Not_found:%s" c_file)
  in
  if Hashtbl.mem hset (enum, s, v)
  then ()
  else begin
    Hashtbl.replace env.hfile_data c_file
      (sexp::Hashtbl.find env.hfile_data c_file);
    Hashtbl.add hset (enum, s, v) true
  end

(*****************************************************************************)
(* Visiting *)
(*****************************************************************************)

let rec process env ast =
  match ast with
  | Paren (TranslationUnitDecl, l, _loc::xs) ->
      xs +> List.iter (fun sexp -> dispatch_sexp env sexp)
  | _ -> failwith (spf "%s: not a TranslationDecl" env.current_clang_file)

and dispatch_sexp env exp =
  match exp with
  | Paren (enum, l, xs) ->
      (match enum with
      | FunctionDecl | VarDecl
      | TypedefDecl | RecordDecl | EnumDecl
      | LinkageSpecDecl
          (* BlockDecl ?? *)
        -> decl env (enum, l, xs)
      | TodoAst s ->
          pr2_once ("TODO:Uninclude_clang: " ^ s);
          (* still need to recurse to at least adjust current_c_file *)
          sexp env exp
      | _ -> 
          failwith (spf "%s:%d: not a toplevel decl" env.current_clang_file l)
      )
  | _ -> failwith (spf "%s:not a Paren sexp" env.current_clang_file)


and decl env (enum, l, xs) =
  update_current_c_file env (enum, l, xs);
  let sexp = Paren (enum, l, xs) in
  (* a bit similar to graph_code_clang decl, but without embeded defs like
   * fields or enum constants as we care only about toplevel decls here.
   *)
  (match enum, xs with
  | FunctionDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::_typ_char::rest ->
      let variant = 
        if rest +> List.exists (function 
        | Paren (CompoundStmt, _, _) -> true
        | _ -> false
        )
        then Def
        else Proto
      in
      add_if_not_already_there env (FunctionDecl, s, variant) sexp
  | TypedefDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::_typ_char::_rest ->
      add_if_not_already_there env (TypedefDecl, s, Misc) sexp
  | EnumDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::_rest ->
      add_if_not_already_there env (EnumDecl, s, Misc) sexp
  | RecordDecl, _loc::(T (TLowerIdent "struct"))
      ::(T (TLowerIdent s | TUpperIdent s))::rest ->
      let kind = if rest = [] then Proto else Misc in
      add_if_not_already_there env (RecordDecl, s, kind) sexp
  | RecordDecl, _loc::(T (TLowerIdent "union"))
      ::(T (TLowerIdent s | TUpperIdent s))::_rest ->
      add_if_not_already_there env (RecordDecl, s, Misc) sexp

  (* todo: what about extern *)
  | VarDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::_typ_char::_rest ->
      (* todo: extern? *)
      let variant = Misc in
      add_if_not_already_there env (VarDecl, s, variant) sexp

  (* usually there is a typedef just behind those anon decl. We need a
   * stable string for the hfile key, so let's use the line loc for now.
   * todo: for enum, could also take the name of the first constant?
   *)
  | RecordDecl, loc::(T (TLowerIdent "union"))::_rest ->
      add_if_not_already_there env 
        (RecordDecl, "union__anon" ^ str_of_angle_loc env l loc, Misc) sexp
  | EnumDecl, loc::_rest ->
      add_if_not_already_there env 
        (EnumDecl, "enum__anon" ^ str_of_angle_loc env l loc, Misc) sexp
  | RecordDecl, loc::(T (TLowerIdent "struct"))::_rest ->
      add_if_not_already_there env 
        (RecordDecl, "struct__anon" ^ str_of_angle_loc env l loc, Misc) sexp

  | LinkageSpecDecl, _loc::(T (TUpperIdent "C"))::xs ->
      List.iter (dispatch_sexp env) xs
  | _ ->
      failwith (spf "%s:%d:wrong Decl line" env.current_clang_file l)
  );
  sexps env xs

and sexp env x =
  match x with
  | Paren (enum, l, xs) ->
      update_current_c_file env (enum, l, xs);
      sexps env xs
  | Angle xs | Anchor xs | Bracket xs ->
      sexps env xs
  | Brace (toks, _) -> ()
  | T tok -> ()

and sexps env xs = List.iter (sexp env) xs

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let uninclude ?(verbose=true) dir skip_list dst =
  let root = Common.realpath dir in
  let all_files = Lib_parsing_clang.find_source_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files skip_list root all_files in
  let env = {
    hfile = Common.hash_of_list [unknown_loc, Hashtbl.create 101];
    hfile_data = Common.hash_of_list [unknown_loc, []];
    current_c_file = ref unknown_loc;
    current_clang_file = "__filled_later__";
    root;
  } in
  

  (* step1: extract files info *)
  files +> Console.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let ast = Parse_clang.parse file in
      process { env with current_clang_file = file } ast
    )
  );

  (* step2: generate clang2 files *)
  env.hfile_data +> Common.hash_to_list +> List.iter (fun (file, xs) ->
    let file = spf "%s/%s.clang2" dst  file in
    pr2 (spf "generating %s" file);
    let dir = Filename.dirname file in
    Common.command2 (spf "mkdir -p %s" dir);
    let xs = List.rev xs in
    Common2.write_value (Paren (TranslationUnitDecl, 0, 
                              Loc.unknown_loc_angle::xs)) file
  )
