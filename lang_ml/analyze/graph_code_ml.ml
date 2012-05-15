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
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * Graph of dependencies for OCaml. See graph_code.ml and main_codegraph.ml
 * for more information.
 * 
 * todo? if give edge weight, then need to modulate depending on
 * the type of the reference. 2 references to a function in another
 * module is more important than 10 references to some constructors.
 * Type|Exception > Function|Class|Global > Constructors|constants ?
 * 
 * todo? there is no edge weight? But is it useful in an ocaml context?
 * We can't have mutually dependent files or directories; the ocaml compiler
 * imposes a layering, so the in-edge will be enough information to give
 * more weight to some nodes. Thx to this layering the connected components
 * module of gephi also does some good work.
 *)
