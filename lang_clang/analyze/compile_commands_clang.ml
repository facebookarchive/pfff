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

module J = Json_type

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Clang needs to know how to compile source files to know how to call
 * cpp on them. clang-check and other clang tools rely on a 
 * compile_commands.json files describing such settings.
 * See http://clang.llvm.org/docs/JSONCompilationDatabase.html
 * 
 * One can generate this compile_commands.json by:
 *  - provide a fake gcc/clang frontend script recording all compile
 *    commands and then running the actual compiler
 *  - processing the xcodebuild trace, 
 *    http://docs.oclint.org/en/dev/usage/oclint-xcodebuild.html
 *  - intercept system calls while compiling a project, the coverity approach
 *    which apparently has just started to be imitated
 *    https://github.com/rizsotto/Bear
 *  - analyzing the make trace a posteriori
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type compile_commands = Json_type.t

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

(* Some 'compile_commands.json' files contain multiple times the same entry
 * for a filename, because files can be compiled with different arguments,
 * e.g. to be built as an object for a dynamic vs static library.
 * This causes then 'clang-check --ast-dump' to generate wrong .clang
 * files with multiple times a TranslationUnitDecl. So let's filter
 * those duplicate entries.
 *)
let sanitize_compile_commands jsonfile =
  let json = Json_in.load_json jsonfile in
  let hdone = Hashtbl.create 101 in
  let json = 
    (match json with
    | J.Array xs ->
        J.Array (xs +> List.filter (fun json ->
        (match json with
        | J.Object ([
            "directory", d;
            "command", c;
            "file", J.String filename;
          ]) ->
            if Hashtbl.mem hdone filename
            then begin
              pr2 (spf "skipping entry %s" filename);
              false
            end else begin
              Hashtbl.add hdone filename true;
              true
            end
        | _ -> failwith "wrong compile_commands.json format"
        )
        ))
    | _ -> failwith "wrong compile_commands.json format"
    )
  in
  pr (Json_out.string_of_json json)

let analyze_make_trace file =
  let relevant_lines = 
    Common.cat file +> Common.map_filter (fun s ->
      let xs = Common.split "[ \t]+" s in
      match xs with
      | ("clang"|"gcc"|"cc")::xs when List.mem "-c" xs ->
          xs +> Common.find_some_opt (fun file ->
            if file =~ ".*\\.[cm]$"
            then Some (file, s)
            else None
          )
      | ("clang"|"gcc"|"cc")::xs when List.mem "-o" xs -> None
      | ("flex" | "bison"
        |"ar"
        |"sh" | "sed"
        )::_rest -> 
          None
      | _ -> 
          pr2 ("unknown compilation command: " ^ s);
          None
    )
  in
  relevant_lines +> List.map (fun (file, s) ->
    let path = Common.realpath file in
    let cmd = Str.global_replace (Str.regexp_string file) path s in
    J.Object [
      "directory", J.String (Common.realpath ".");
      "command", J.String cmd;
      "file", J.String path;
    ]
  ) +> (fun xs -> J.Array xs)


