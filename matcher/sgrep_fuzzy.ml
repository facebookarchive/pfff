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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* See https://github.com/facebook/pfff/wiki/Sgrep 
*)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

type pattern = Ast_fuzzy.tree list

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)
let (parse: string -> pattern) = fun str ->
  raise Todo

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let sgrep ~hook pattern ast =

  (* visit AST and try to match pattern on it *)
  let rec visit trees =
    let x = trees in
    let matches_with_env =
      Matching_fuzzy.match_trees_trees pattern x
    in
    if matches_with_env = []
    then (* recurse *)
      ()
    else begin
    (* could also recurse to find nested matching inside the matched code
     * itself
     *)
      let matched_tokens = Ast_fuzzy.ii_of_trees x in
      matches_with_env +> List.iter (fun env ->
        hook env matched_tokens
      )
    end
  in
  visit ast


