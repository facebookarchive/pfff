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


type todo = unit

(* runtime value. See also parsing_php/type_php.ml  *)
type rval = 

  | RBool of bool
  | RInt of int
  | RFloat of float
  | RString of string

  | RNull

  | RObject of todo
  (* PHP hash/arrays are very versatile and have weird semantic *)
  | RHash of todo 


(* copy on write semantic ? for everything ? for int/bool too ? *)
type variable = {
  mutable v : rval;
}

(* see also checkModule *)
type env = { 
  functions: (string, Ast_php.func_def) Hashtbl.t;

  (* parameters, local variables *)
  variables:(string, variable) Hashtbl.t;

  globals:(string, rval) Hashtbl.t;
}

type frame = todo


let default_env () = {
  globals = Hashtbl.create 1001;
  functions = Hashtbl.create 1001;
  variables = Hashtbl.create 101;
 
}
