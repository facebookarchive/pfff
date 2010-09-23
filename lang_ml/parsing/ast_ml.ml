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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

type pinfo = 
    (* Present both in the AST and list of tokens *)
    | OriginTok  of Common.parse_info
  (* with tarzan *)

type info = { 
  mutable pinfo : pinfo; 
  }

and tok = info

(* ------------------------------------------------------------------------- *)
(* Names  *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Type expressions *)
(* ------------------------------------------------------------------------- *)
(* core language, module language, class language *)


(* ------------------------------------------------------------------------- *)
(* Value expressions *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Toplevel phrases *)
(* ------------------------------------------------------------------------- *)

and toplevel = unit

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

(* todo: should put this generic code in h_program-lang *)

let rewrap_str s ii =  
  {ii with pinfo =
    (match ii.pinfo with
    | OriginTok pi -> OriginTok { pi with Common.str = s;}
    )
  }

let parse_info_of_info ii = 
  match ii.pinfo with
  | OriginTok pinfo -> pinfo

let str_of_info  ii = (parse_info_of_info ii).Common.str 
let file_of_info ii = (parse_info_of_info ii).Common.file
let line_of_info ii = (parse_info_of_info ii).Common.line
let col_of_info  ii = (parse_info_of_info ii).Common.column
let pos_of_info  ii = (parse_info_of_info ii).Common.charpos

let pinfo_of_info ii = ii.pinfo

let is_origintok ii = 
  match ii.pinfo with
  | OriginTok pi -> true


