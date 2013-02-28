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

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

let builtin_types = Common.hashset_of_list [
  "char";
  "int";"short";"long";
  "float";"double";
  "void";
  "unsigned";"signed";

  "const";"restrict";"volatile";

  "noreturn";"__attribute__";
  (* clang *)
  "__int128";
  "__va_list_tag";
  (* todo: ugly, because of stdbool.h skip? but now that use ExpansionLoc,
   * don't need that anymore? apparently still need it :(
   *)
  "_Bool";

  (* otherwise get wierd edges from EXTERNAL to the source. e.g. in byacc *)
  "__builtin_va_list";
]
