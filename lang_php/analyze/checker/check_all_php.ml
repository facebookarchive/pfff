(* Yoann Padioleau
 *
 * Copyright (C) 2010-2012 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * A driver for our different PHP checkers.
 * 
 * todo:
 *  - type checker
 *  - dataflow based unused var
 *  - record checker (fields)
 *  - protocol checker, statistical static analysis a la Engler
 *  - ...
 *)

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

(* coupling: if modify this, also modify lint_php.ml in pfff/facebook/... *)
let check_file ?(verbose=true) ?(find_entity=None) env file =

 Common.save_excursion Flag_analyze_php.verbose_checking verbose (fun() ->
  (* We need to unsugar self/parent earlier now (we used to do it only
   * before Check_functions_php) because check_and_annotate_program
   * needs to tag if something is passed by reference, which requires
   * now to lookup static methods, which requires the self/parent unsugaring.
   * 
   * todo: unsugar traits by inlining/mixing their code in the class
   * using them.
   *)
  let ast = Parse_php.parse_program file 
    +> Unsugar_php.unsugar_self_parent_program
  in

  (* even if find_entity=None, check_and_annotate_program can find
   * interesting bugs on local variables. There will be false positives
   * though when variables are passed by reference but it's better than
   * nothing.
   *)
  Check_variables_php.check_and_annotate_program find_entity ast;

  Check_includes_php.check env file ast;
  Check_cfg_php.check_program ast;
  (* not ready yet: Check_dfg_php.check_program ?find_entity ast; *)
  Check_misc_php.check ast;

  (* work only when have a find_entity; requires a global view of the code *)
  find_entity +> Common.do_option (fun find_entity ->
    Check_functions_php.check_program find_entity ast;
    Check_classes_php.check_program   find_entity ast;
  );
  ()
 )
