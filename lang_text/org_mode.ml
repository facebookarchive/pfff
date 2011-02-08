(* Yoann Padioleau
 * 
 * Copyright (C) 2010 Facebook
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)

open Common 

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type org_line =
  | Header of int * string (* full string, including starting stars *)
  (* todo? could also highlight the http refs and other markup ? *)
  | Comment of string
  | Other of string

type org = org_line list

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

let parse file =
  let xs = Common.cat file in
  xs +> List.map (fun s ->
    match () with
    | _ when s =~ "^\\([*]+\\)" ->
        let header = Common.matched1 s in
        Header (String.length header, s)
    | _ when s =~ "^#.*" ->
        Comment s
    | _ -> 
        Other s
  )


(*****************************************************************************)
(* Highlighting *)
(*****************************************************************************)

let highlight org =
  raise Todo
