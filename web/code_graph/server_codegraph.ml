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

module DM = Dependencies_matrix_code
module DMBuild = Dependencies_matrix_build

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* todo: 'config_path list' at some point *)
type path = string list

(*****************************************************************************)
(* Builder *)
(*****************************************************************************)

let build gopti_ref path =

  (* TODO: a/b -> Expand "a"; Expand "a/b" *)
  let config_path = 
    path +> List.map (fun s -> DM.Expand (s, Database_code.Dir)) in
  DMBuild.threshold_pack := 70;
  let config, gopti = DMBuild.config_of_path config_path !gopti_ref in
  let m, gopti =
    DMBuild.build config (None) gopti in
  gopti_ref := gopti;
  m
