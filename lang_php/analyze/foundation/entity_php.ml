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

module Ast = Ast_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * In different structures such as the callgraph we need to refer to a
 * certain PHP entity, like a specific function. Usually those specific
 * pieces of code we want to refer to are functions or classes. So we need
 * a way to represent such place. We can use the location, which is the
 * fullid described below but it takes quite some space (a string, 2
 * ints). Enter the 'id' type.
 * 
 * Update: we now also have "nested" ids which are ids about nested
 * entities like method (of a class) or nested functions. Could have
 * definied an id type and subid ... ? but seems tedious. Better to
 * have only 'id' and a 'id_kind' and associated tables that stores subasts
 * and other info (see children_id and enclosing_id tables in
 * database_php.ml)
 * 
 * todo? One pb is that we lose some precision. Sometimes we would like
 * to enforce a certain kind of id. So all this scheme is good ? At the
 * same time a function can be called from many stuff, another func, a
 * toplevel stmt, a method, an expression in a variable declaration, etc
 * so maybe simpler to have indeed a single 'id'.
 * 
 * todo? hmmm but maybe we could have a entity_finder2: 
 *  type entity_finder2 = { functions: ...; classes: ... }
 * which would be close to what julien is using in his code when he
 * needs some global analysis.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* primary key (prefered) method *) 
type id = Id of int


(* also primary key, more readable, but less optimial spacewise *)
type fullid = filepos
 and filepos = {
  file: filename;
  line: int;
  column: int;
}
 (* with tarzan *)

(* todo? copy-paste/redundant with 
 *  - database_code.ml, highlight_code.ml
 *  - Ast_php.entity (but without the actual ast content)
 *  - view_widgets_source.ml ?
 *  - typing_c environment type 
 *  - type_annotater_c.ml namedef type
 * 
 *)
type id_kind =
  (* toplevels, which can also be nested *)
  | Function
  | Class
  | StmtList 

  (* only at nested level, inside a class *)
  (* StaticMethod could be considered a Function, because in PHP they
   * mostly use static methods because PHP (5.2) didn't have namespace and
   * so they abuse classes for modules.
   *)
  | Method | ClassConstant | ClassVariable | XhpDecl

  | IdMisc

(* See comment in the .mli for more information about entity_finder.
 * 
 * update: I now return a list of entities instead of a single entity
 * and raising a Not_found or Multi_found. The rational is that
 * the caller knows better what to do when there are multiple
 * matching definitions for an entity. For instance for 
 * better error message having just Multi_found is not enough.
 * The caller needs for instance to show two witnesses.
 *)

type entity_finder = (id_kind * string) -> Ast_php.entity list

type method_identifier = (string * string)

(*****************************************************************************)
(* String_of *)
(*****************************************************************************)
let str_of_id (Id x) = 
  spf "id:%d" x

let str_of_fullid x = 
  spf "%s:%d:%d" x.file x.line x.column

let fullid_regexp = 
 "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)$"

let fullid_of_string s = 
  if s =~ fullid_regexp
  then
    let (filename, line, column) = matched3 s in
    { file = filename;
      line = s_to_i line;
      column = s_to_i column;
    }
  else failwith ("not a full_id:" ^ s)


let string_of_id_kind = function
  | Function -> "function"
  | Class -> "class"
  | StmtList  -> "stmtlist"

  | Method -> "method"
  | ClassConstant -> "classConstant" | ClassVariable -> "classVariable"
  | XhpDecl -> "xhpDecl"
  | IdMisc -> "idmisc"

(*****************************************************************************)
(* Method identifier helpers *)
(*****************************************************************************)

let method_identifier_of_string s =
  raise Todo

let string_of_method_identifier (s1, s2) =
  raise Todo

(*****************************************************************************)
(* Meta *)
(*****************************************************************************)

let vof_filename = Ocaml.vof_string

let rec vof_fullid v = vof_filepos v
and vof_filepos { file = v_file; line = v_line; column = v_column } =
  let bnds = [] in
  let arg = Ocaml.vof_int v_column in
  let bnd = ("column", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_int v_line in
  let bnd = ("line", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_filename v_file in
  let bnd = ("file", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let (filepos_of_parse_info: Parse_info.parse_info -> filepos) = fun pi -> 
  { 
    file = pi.Parse_info.file;
    line = pi.Parse_info.line;
    column = pi.Parse_info.column;
  }
