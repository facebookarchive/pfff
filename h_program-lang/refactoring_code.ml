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
(* Types *)
(*****************************************************************************)

type refactoring = { 
  file: Common.filename;
  line: int;
  col: int;
  action: refactoring_kind;
}
  and refactoring_kind =
    | AddReturnType of string
    | AddTypeHintParameter of string
    | AddTypeMember of string
    | OptionizeTypeParameter

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

(* format: file;RETURN;line;col;value *)
let load file =
  Common.cat file +> List.map (fun s ->
    let xs = Common.split ";" s in
    match xs with
    | [file;action;line;col;value] when
          line =~ "[0-9]+" && col =~ "[0-9]+" && 
          (List.mem action ["RETURN";"PARAM";"MEMBER"; "MAKE_OPTION_TYPE"]) ->
        { file;
          line = int_of_string line;
          col = int_of_string col;
          action =
            (match action with
            | "RETURN" -> AddReturnType value
            | "PARAM" -> AddTypeHintParameter value
            | "MEMBER" -> AddTypeMember value
            | "MAKE_OPTION_TYPE" -> OptionizeTypeParameter
            | _ -> raise Impossible
            )
        }

    | _ -> failwith ("wrong format for refactoring action: " ^ s)
  )

