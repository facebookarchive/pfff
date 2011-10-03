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

open Runtime_php

module R = Runtime_php

(*
let h_builtins = Hashtbl.create 101

let register s f =
  raise Todo
*)



let echo v = 
  match v with
  | RString s -> 
      print_string s

  | RInt i -> print_int i
  | _ -> raise Todo


