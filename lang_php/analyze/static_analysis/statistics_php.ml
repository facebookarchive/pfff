(* Yoann Padioleau
 * 
 * Copyright (C) 2009, 2010, 2011 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * PHP does not have the notion of a main(), and so some PHP files contain
 * only function definitions while other contain toplevel statements.
 * It can be useful to know which kind a PHP file is. For instance
 * in the endpoints and scripts reaper we want to identify the files
 * under certain directories which are the starting points. This is
 * usually files with many toplevel statements which are not just
 * "directives" (e.g. ini_set(...) or require_xxx() or xxx_init()).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* todo? move this in h_program-lang/ ? quite similar to
 * statistics_code.mli ? but want the kinds of the toplevel funcalls,
 * which is probably quite PHP specific.
 *)
type stat = {
  mutable functions: int;
  mutable classes: int;

  mutable toplevels_funcalls: int;
  mutable toplevels_assign: int;
  mutable toplevels_other: int;
  mutable toplevels_include_requires: int;

  mutable toplevels_funcalls_kinds: (string * int) list;
  (* toplevels_assign_kinds? *)
}

type php_file_kind =
  | LibFile
  | IncluderFile
  | ScriptOrEndpointFile

let default_stat () = {
  functions = 0;
  classes = 0;

  toplevels_funcalls = 0;
  toplevels_assign = 0;
  toplevels_other = 0;
  toplevels_include_requires = 0;

  toplevels_funcalls_kinds = [];
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let string_of_stat stat =
spf "
  functions = %d;
  classes = %d;

  toplevels_funcalls = %d;
  toplevels_assign = %d;
  toplevels_other = %d;
  toplevels_include_requires = %d;

  toplevels_funcalls_kinds = 
%s
"
  stat.functions stat.classes 
  stat.toplevels_funcalls stat.toplevels_assign 
  stat.toplevels_other stat.toplevels_include_requires
  (stat.toplevels_funcalls_kinds +> List.map (fun (s, d) ->
    spf "   %s -> %d\n" s d) +> Common.join "")

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let stat_of_program ast =
  let (funcs, classes, topstmts) = 
    Lib_parsing_php.functions_methods_or_topstms_of_program ast in

  let stat = { (default_stat ()) with
    functions = List.length funcs;
    classes = List.length classes;
  }
  in
  raise Todo


let kind_of_file_using_stat stat =
  raise Todo
