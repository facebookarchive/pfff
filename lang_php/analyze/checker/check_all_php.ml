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

let check_file ?find_entity file =

  let ast = Parse_php.parse_program file in
  Lib_parsing_php.print_warning_if_not_correctly_parsed ast file;

  Check_variables_php.check_and_annotate_program ?find_entity ast;
  Check_cfg_php.check_program ?find_entity ast;

  (* TODO:
     Checking_php.check_program ast;
     Check_scope_use_php.check_program ast;
  *)
  ()
