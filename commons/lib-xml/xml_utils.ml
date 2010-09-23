(* Yoann Padioleau
 * 
 * Copyright (C) 2010 Facebook. All Rights Reserved.
 *)

open Xml_types

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let rec iter_rec f xml = 
  match xml with
  | Element (s, attrs, body) ->
      f (s, attrs, body);
      List.iter (iter_rec f) body;
  | PCData s -> ()

