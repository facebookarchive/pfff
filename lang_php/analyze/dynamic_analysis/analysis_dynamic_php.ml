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

(* 
 * There are a few tools to dynamically analyze PHP code (for instance
 * phptrace), but in the end they are just thin wrappers over one
 * real thing: Xdebug. 
 * 
 * Xdebug has many options, including the possiblity
 * to trace function calls, trace their argument and return values. 
 * This makes it possible to do analysis such as type inference.
 * This is hard statically, but trivial dynamically.
 * 
 * Here is the command line to enable xdebug
 * 
 * php -d xdebug.auto_trace=1 
 *     -d xdebug.trace_options=1 
 *     -d xdebug.trace_output_dir=`pwd` 
 *     -d xdebug.trace_output_name=dumpfile 
 *     -d xdebug.trace_format=0 
 *     ...
 *     -d xdebug.collect_params=1 
 *     -d xdebug.collect_return=1 
 *    "$@"
 * See also xdebug.ml
 * 
 * related work:
 *  - phptracer, use xdebug
 *  - phpunit module for tracing, uses xdebug
 *  - rogerc extension ? use phpunit which use xdebug
 *  
 *  - xhprof ? 
 * 
 * current analysis:
 *  - TODO type infer 
 *  - TODO callgraph 
 *  - TODO coverage ?
 * 
 *)


(*****************************************************************************)
(* Type *)
(*****************************************************************************)
