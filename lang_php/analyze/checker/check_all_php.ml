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

module E = Error_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

(* coupling: if modify this, also modify lint_php.ml in pfff/facebook/... *)
let check_file ?(find_entity=None) env file =

  let ast = Parse_php.parse_program file in

  (* we need to unsugar self/parent earlier now (we used to do it only
   * before Check_functions_php) because check_and_annotate_program
   * needs to tag if something is passed by reference, which requires
   * now to lookup static methods, which requires the self/parent unsugaring
   *)
  let ast = Unsugar_php.unsugar_self_parent_program ast in

  (* even if find_entity=None, check_and_annotate_program can find
   * interesting bugs on local variables. There will be false positives
   * though when variables are passed by reference.
   *)
  Check_variables_php.check_and_annotate_program find_entity ast;
  Check_includes_php.check env file ast;
  Check_cfg_php.check_program ast;
  (* not ready yet: Check_dfg_php.check_program ?find_entity ast; *)

  (* work only when have a find_entity; requires global view of the code *)
  (match find_entity with
  | None -> ()
  | Some find_entity ->
      Check_functions_php.check_program find_entity ast;
      Check_classes_php.check_program   find_entity ast;
  );
  ()
