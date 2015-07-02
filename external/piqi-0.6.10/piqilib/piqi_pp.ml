(*
   Copyright 2009, 2010, 2011, 2012, 2013, 2014 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)


open Piqi_common 


let prettyprint_ast ch ast =
  Piq_gen.to_channel ch ast


let rec prettyprint_list ch ast_list =
  let rec aux = function
    | [] -> ()
    | [x] -> prettyprint_ast ch x
    | h::t ->
          prettyprint_ast ch h;
          output_char ch '\n';
          aux t
  in aux ast_list


(* simplify piqi ast: *)
let simplify_piqi_ast (ast:piq_ast) =
  let tr = Piq_ast.transform_ast in
  (* map typedef.x -> x *)
  let rm_typedef =
    tr [] (
      function
        | `named {Piq_ast.Named.name = "typedef"; Piq_ast.Named.value = v} -> [v]
        | x -> [x]
    )
  (* del .../mode.required *)
  (* map .../field/mode x -> x *)
  and tr_field_mode path =
    tr path (
      function
        | `named {Piq_ast.Named.name = "mode"; Piq_ast.Named.value = `name "required"} -> []
        | `named {Piq_ast.Named.name = "mode"; Piq_ast.Named.value = (`name _) as x} -> [x]
        | x -> [x]
    )
  (* map extend/.piqi-any x -> x *)
  (* TODO: this method of specifying extensions will be eventually deprecated *)
  and tr_extend_piq_any =
    tr ["extend"] (
      function
        | `named {Piq_ast.Named.name = "piqi-any"; Piq_ast.Named.value = v} -> [v]
        | x -> [x]
    )
  (* map extend/.what x -> x *)
  and tr_extend_what =
    tr ["extend"] (
      function
        | `named {Piq_ast.Named.name = "what"; Piq_ast.Named.value = v} -> [v]
        | x -> [x]
    )
  (* map ../record.x -> x *)
  (* map ../name.x -> x *)
  and rm_param_extra path =
    tr path (
      function
        | `named {Piq_ast.Named.name = "record"; Piq_ast.Named.value = v} -> [v]
        | `named {Piq_ast.Named.name = "name"; Piq_ast.Named.value = v} -> [v]
        | x -> [x]
    )
  (* strip :<type> from a field's default value *)
  and tr_field_default_value path =
    tr path (
      function
        | `typed {Piq_ast.Typed.value = v} -> [v]
        | x -> [x]
    )
  in
  let (|>) a f = f a in
  let simplify_function_param param ast =
    ast
    |> rm_param_extra ["function"; param]
    |> tr_field_mode ["function"; param; "field"]
    |> tr_field_default_value ["function"; param; "field"; "default"]
  in
  ast
  |> rm_typedef
  |> tr_field_mode ["record"; "field"]
  |> tr_field_default_value ["record"; "field"; "default"]
  |> tr_extend_piq_any
  |> tr_extend_what
  (* functions *)
  |> simplify_function_param "input"
  |> simplify_function_param "output"
  |> simplify_function_param "error"


let compare_piqi_items a b =
  let name_of = function
    | `name x -> x
    | `named x -> x.Piq_ast.Named.name
    | `typename x -> x
    | `typed x -> x.Piq_ast.Typed.typename
    | _ -> assert false
  in
  let rank x =
    match name_of x with
      | "module" -> 0
      | "protobuf-package" -> 1
      | "include" -> 2
      | "import" -> 3
      | "typedef" -> 4
      | "function" -> 5
      | "extend" -> 6
      | _ -> 100
  in
  rank a - rank b


let sort_piqi_items (ast:piq_ast) =
  match ast with
    | `list l ->
        let l = List.stable_sort compare_piqi_items l in
        `list l
    | _ -> assert false


let prettify_piqi_ast ast =
  let ast = sort_piqi_items ast in
  simplify_piqi_ast ast


let prettyprint_piqi_ast ch ast =
  let ast = prettify_piqi_ast ast in
  match ast with
    | `list l -> prettyprint_list ch l
    | _ -> assert false


let prettyprint_piqi ch (piqi:T.piqi) =
  let ast = Piqi.piqi_to_ast piqi in
  prettyprint_piqi_ast ch ast

