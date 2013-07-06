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
 * This is mostly a wrapper around javalib (see external/javalib).
 * 
 * Pursuing both the source-code (lang_java/) and byte-code (lang_bytecode/)
 * paths allows us to double check each other and avoid closing doors 
 * for ourselves depending on the task.
 * 
 * Getting some simple analysis on the bytecode, e.g. graph_code_bytecode.ml
 * is also a good first step towards using more advanced Java static
 * analysis tools like Jchord. Indeed getting familiar with the
 * bytecode format used by this tools is good to understand their limitations,.
 * Moreover if codegraph on a codebase does not generate any error
 * we are sure we have all the necessary source and are ready to run
 * other tools.
 * 
 * The bytecode also offers a simple baseline to compare with if we do stuff
 * at the source code level later. Indeed when we have mistakes at
 * the source code level, it is good to know if we have the same problem
 * at the bytecode level, in which case it's probably because some
 * code was not included or on the opposite code should not be analyzed.
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

type ast = JClassLow.jclass


