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

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let create_graph ?(show_progress=false) ?(strict=false) files db =

  Common.save_excursion Abstract_interpreter_php.extract_paths true (fun()->
  Common.save_excursion Abstract_interpreter_php.strict strict (fun()->
    Abstract_interpreter_php.graph := Map_poly.empty;

    files +> Common_extra.progress ~show:show_progress (fun k ->
     List.iter (fun file ->
       k();
       raise Todo
     )
    )
  ));
  !Abstract_interpreter_php.graph

