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
 *  - if the project uses cmake, run 'cmake CMAKE_EXPORT_COMPILE_COMMANDS=on'
 *  - provide a fake gcc/clang frontend script recording all compile
 *    commands and then running the actual compiler
 *  - analyzing the make compilation trace a posteriori
 *  - processing the xcodebuild trace, 
 *    http://docs.oclint.org/en/dev/usage/oclint-xcodebuild.html
 *  - intercept system calls while compiling a project, a la coverity
 *    * https://github.com/rizsotto/Bear (just started)
 *    * https://github.com/pgbovine/CDE/
 *    * https://github.com/rprichard/sourceweb/tree/master/btrace
 *    * ??
 * 
 * procedure to analyze a project:
 *  $ make V=1 > make_trace.txt
 *  $ ~/pfff/pfff_test -analyze_make_trace make_trace.txt >compile_commands.json
 *  $ ~/pfff/pfff -gen_clang compile_commands.json 
 *  ...
 * 
 * alternative when project uses cmake:
 *  $ cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON 
 *  $ mv compile_commands.json old.json
 *  $ ~/pfff/pfff -sanitize_compile_commands old.json > compile_commands.json
 *  $ ~/pfff/pfff -gen_clang ...
 *  $ ...
 * 

 * todo: analyze the -I and use realpath on its argument => easier
 * to merge trace from subdirectories.
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
let sanitize_compile_commands json =
  let hdone = Hashtbl.create 101 in
  match json with
  | J.Array xs ->
      J.Array (xs +> List.filter (fun json ->
        (match json with
        | J.Object ([
            "directory", _d;
            "command", _c;
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

(* analyzing the make compilation trace a posteriori *)
let analyze_make_trace file =
  let dir = ref None in

  let relevant_lines = 
    Common.cat file +> Common.map_filter (fun s ->
      let xs = Common.split "[ \t]+" s in
      match xs with
      | ("clang"|"gcc"|"cc")::xs when List.mem "-c" xs ->
          xs +> Common.find_some_opt (fun file ->
            if file =~ ".*\\.[cm]$"
            then Some (file, s, !dir)
            else None
          )
      | ("clang"|"gcc"|"cc")::xs when List.mem "-o" xs -> None

      | ("flex" | "bison" | "yacc"
        |"ar"
        |"sh" | "sed" | "mv"
        )::_rest -> 
          None

      | ("8c" | "8^c")::xs ->
          xs +> Common.find_some_opt (fun file ->
            let s = Str.global_replace (Str.regexp "8c\\|8^c") "cc" s in
            if file =~ ".*\\.c$"
            then Some (file, s ^ " " ^ (Common.join " " [

              (* for libs *)
              "-Wno-incompatible-library-redeclaration";
              "-Wno-missing-declarations";
              "-Wno-incompatible-pointer-types";
              "-Wno-main-return-type";
              "-Wno-dangling-else";
              "-Wno-incompatible-pointer-types-discards-qualifiers";
              "-Wno-implicit-function-declaration";
              "-Wno-parentheses";
              "-Wno-logical-op-parentheses";
              "-Wno-gnu-designator";
              "-Wno-comment";
              "-Wno-switch";

              (* for kernel *)
              "-Wno-typedef-redefinition";

              "-fno-builtin"; "-nostdinc";
              "-Qunused-arguments"
            ]),
                       !dir)
            else None
          )

      (* special for plan9-userspace make_target *)
      | ["DIR:";s] ->
          dir := Some s;
          None
      | _ -> 
          pr2 ("unknown compilation command: " ^ s);
          None
    )
  in
  relevant_lines +> List.map (fun (file, s, dir) ->
    let final_file = 
      match dir with
      | None -> file
      | Some f -> 
          if file =~ "^/" 
          then file
          else Filename.concat f file
    in
    let directory =
      match dir with
      | None -> "."
      | Some f -> f
    in
    
    let path = Common.realpath final_file in
    (*let cmd = Str.global_replace (Str.regexp_string file) path s in *)
    J.Object [
      "directory", J.String (Common.realpath directory);
      "command", J.String s;
      "file", J.String path;
    ]
  ) +> (fun xs -> J.Array xs)
  +> sanitize_compile_commands
