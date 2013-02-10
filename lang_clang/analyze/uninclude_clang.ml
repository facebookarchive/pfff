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

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = {
  hfile: 
    (Common.filename, (Ast.enum * string) Common.hashset) Hashtbl.t;
  hfile_data:
    (Common.filename, sexp list) Hashtbl.t;

  current_c_file: Common.filename ref;
  current_clang_file: Common.filename;
}

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec process env ast =
  match ast with
  | Paren (TranslationUnitDecl, l, _loc::xs) ->
      xs +> List.iter (fun sexp ->
        (match sexp with
        | Paren (enum, l, xs) ->
            (* dispatch *)
            (match enum with
            | FunctionDecl
              -> decl env (enum, l, xs)
            | _ -> 
                failwith (spf "%s:%d: not a toplevel decl" 
                             env.current_clang_file l)
            )
        | _ -> failwith (spf "%s:%d:not a Paren sexp" 
                            env.current_clang_file l)
        )
      )
  | _ -> failwith (spf "%s: not a TranslationDecl" env.current_clang_file)

and decl env (enum, l, xs) =
  (match enum, xs with
  | _ -> ()
  );
  sexp env (Paren (enum, l, xs))

and sexp env x =
  match x with
  | Paren (enum, l, xs) ->
      let file_opt = 
        Loc.location_of_paren_opt env.current_clang_file (enum, l, xs) in
      file_opt +> Common.do_option (fun f ->
        env.current_c_file := f;

        if not (Hashtbl.mem env.hfile f)
        then begin
          Hashtbl.add env.hfile f (Hashtbl.create 10);
          Hashtbl.add env.hfile_data f [];
        end;
      );
      sexps env xs

  | Angle xs | Anchor xs | Bracket xs ->
      sexps env xs
  | T tok -> ()

and sexps env xs = List.iter (sexp env) xs

let uninclude ?(verbose=true) dir skip_list dst =
  let root = Common.realpath dir in
  let all_files = Lib_parsing_clang.find_source_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files skip_list root all_files in
  let env = {
    hfile = Hashtbl.create 101;
    hfile_data = Hashtbl.create 101;
    current_c_file = ref "__unknown__location";
    current_clang_file = "__filled_later__";
  } in

  (* step1: extract files info *)
  files +> Common_extra.progress ~show:verbose (fun k ->
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
  )
