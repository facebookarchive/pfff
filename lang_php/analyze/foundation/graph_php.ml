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

open Ast_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * An adapter over ocamlgraph 
 * 
 * Alternatives:
 *  - camllib by bjeannet
 *  - reimplement myself the graph basic algorithms using ograph_simple
 *    as underling data
 *)

