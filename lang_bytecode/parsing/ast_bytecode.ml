(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This is mostly a wrapper around javalib (see externals/javalib).
 * 
 * Pursuing both the source-code (lang_java/) and byte-code (lang_bytecode/)
 * path allows us to double check each other and avoid closing doors 
 * for ourselves depending on the task.
 *)


(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

type ast = JClassLow.jclass


