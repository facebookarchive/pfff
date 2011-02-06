(*s: cyclomatic_php.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009, 2010, 2011 Facebook
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
(*e: Facebook copyright *)

open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * References:
 *  - A Complexity Measure, by  Thomas J. McCabe, IEEE Transactions on
 *    software engineering, Dec 1976
 *  - http://en.wikipedia.org/wiki/Cyclomatic_complexity
 * 
 * From the wikipedia page:
 * "Cyclomatic complexity (or conditional complexity) is a software metric
 * (measurement). It was developed by Thomas J. McCabe, Sr. in 1976 and
 * is used to indicate the complexity of a program. It directly measures
 * the number of linearly independent paths through a program's source
 * code. 
 * 
 * The complexity is then defined as
 * 
 * M = E - N + 2P
 * 
 * where
 * 
 * M = cyclomatic complexity
 * E = the number of edges of the graph
 * N = the number of nodes of the graph
 * P = the number of connected components 
 * 
 * ...
 * For a single program (or subroutine or method), P is always equal to 1.
 * "
 * 
 * The goal of having a set of "linear independent paths" 
 * is to have a set of "foundational paths" from which all other paths
 * can be described (by a linear combination of those foundational paths).
 * 
 * Note that the cyclomatic complexity does not care about
 * the complexity of expressions or function calls. For instance
 * 'if(e1 || e1 || e3) { ...} will have the same cyclomatic complexity 
 * as 'if(e1) { ... }' because they have a similar CFG.
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let cyclomatic_complexity_flow ?(verbose=false) flow = 

  let n = flow#nb_nodes in
  let e = flow#nb_edges in
  let p = 1 in

  let m = e - n +  2 * p  in

  if verbose 
  then pr2 (spf "N = %d, E = %d" n e);

  m

let cyclomatic_complexity_func ?verbose func =
  let flow = Controlflow_build_php.cfg_of_func func in
  cyclomatic_complexity_flow flow

let cyclomatic_complexity_method ?verbose meth =
  let flow = Controlflow_build_php.cfg_of_method meth in
  cyclomatic_complexity_flow flow


(*e: cyclomatic_php.ml *)
