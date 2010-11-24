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
open Error_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * Structure similar to other layer generator.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* ugly: there is some duplication and coupling with the Error_php.error
 * type.
 *)
let properties = [
  "eUndefinedFunction",    "blue";
  "eUnableToDetermineDef", "blue2";

  "eTooManyArguments", "blue3";
  "eNotEnoughArguments", "blue4";

  "eTooManyArguments2", "blue3" ;
  "eTooFewArguments2",  "blue4" ;
  "eWrongKeywordArgument", "yellow";

  "eUseOfUndefinedVariable", "red" ;
  "eUnusedVariable", "purple";

  "eUseOfUndefinedMember", "cyan";
]

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let info_of_error_and_kind err =

  let kind = 
    match err with
  | UndefinedFunction _ -> "eUndefinedFunction"
  | UnableToDetermineDef _ ->"eUnableToDetermineDef"

  | TooManyArguments _ ->"eTooManyArguments"
  | NotEnoughArguments _ ->"eNotEnoughArguments"

  | TooManyArguments2 _ ->"eTooManyArguments2"
  | TooFewArguments2  _ ->"eTooFewArguments2"
  | WrongKeywordArgument _ ->"eWrongKeywordArgument"

  | UseOfUndefinedVariable _ ->"eUseOfUndefinedVariable"
  | UnusedVariable _ ->"eUnusedVariable"

  | UseOfUndefinedMember _ ->"eUseOfUndefinedMember"
  in
  E.info_of_error err +> Common.fmap (fun info ->
    info, kind
  )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let gen_layer ~root ~output errors = 

  let infos = errors +> Common.map_filter info_of_error_and_kind in

  let layer = Layer_code.simple_layer_of_parse_infos ~root infos properties in
  pr2 ("generating layer in " ^ output);
  Layer_code.save_layer layer output;
  ()


