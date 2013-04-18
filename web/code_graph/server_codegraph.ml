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

let build gopti path =

  (* TODO: a/b -> Expand "a"; Expand "a/b" *)
  let config_path = 
    path +> List.map (fun s -> DM.Expand (s, Database_code.Dir)) in
  DM.threshold_pack := 4000;
  let config, _goptiTODO = DM.config_of_path config_path gopti in
  
  let m, _goptiTODO =
    DM.build config (None) gopti in
  m
