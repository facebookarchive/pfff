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
 *  - analyzing the make trace a posteriori
 *  - processing the xcodebuild trace, 
 *    http://docs.oclint.org/en/dev/usage/oclint-xcodebuild.html
 *  - intercept system calls while compiling a project, the coverity approach
 *    which apparently has just started to be imitated
 *    https://github.com/rizsotto/Bear
 *)


(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type compile_commands = Json_type.t

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let analyze_make_trace file =
  raise Todo
