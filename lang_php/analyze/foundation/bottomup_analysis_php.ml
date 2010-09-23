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

(* fixpoint. Often needed for inter procedural analysis.
 *
 * TODO: use the graph stuff I was using for detecting dangerous
 * CPP macro ? or use the white/black/grey tech of tarjan that julien
 * told me about!
 * 
 * TODO: make it more generic ?
*)


(*
http://en.wikipedia.org/wiki/Topological_sorting

What to do when have cycles (because recursive functions or mutually
recursive functions) ? The topological sort assumed a DAG, not a graph.

Well can do a first pass that produces a DAG by computing the 
connected components:

http://en.wikipedia.org/wiki/Strongly_connected_component


iterate for all name_defs

let h_already, iterate with white


*)
