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

module Db = Database_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * Programs are hierarchies of entities (package/module/class/...)
 * linked to each other through different dependencies (call, extend, 
 * use, etc). This module is the basis for codegraph, a tool
 * to help visualize code dependencies or code relationships. It
 * provides the core data structure of codegraph, an (hyper)graph of
 * all the entities in a program linked either via a 'has-a' relation,
 * which represent the hierarchies, or 'use-a', which represent
 * the dependencies.
 * 
 * This file could have been named dependency_code.ml or
 * relation_code.ml 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type edge =
  | Has
  (* todo? refine by having different cases? Use of `Call|`Extend|...? *)
  | Use

(* todo? how handle duplicate entities? prepend then a ___number suffix? *)
type node = string * Db.entity_kind

(* 
 * note: file information are in readable path format 
 *)
type graph = {
  g: (node, Parse_info.info, edge) Ograph_simple.ograph_mutable;
}

