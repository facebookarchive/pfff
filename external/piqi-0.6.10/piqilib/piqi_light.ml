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


(*
 * Light syntax for Piqi DDL.
 *)


module C = Piqi_common
open C
open Iolist


let gen_typename x = ios x ^^ ios "()"


let gen_name_type name typename =
  match name, typename with
    | Some n, None -> ios n
    | None, Some t -> gen_typename t
    | Some n, Some t -> ios n ^^ ios " :: " ^^ gen_typename t
    | _ -> assert false


let gen_default_ast ast =
  let str = Piq_gen.to_string ast ~nl:false in
  if String.contains str '\n' (* multiline? *)
  then
    let lines = Piq_gen.split_text str in
    let lines = List.map ios lines in
    iol [
      ios " ="; indent;
        iod "\n" lines;
      unindent;
    ]
  else
    iol [ ios " = "; ios str; ]


let gen_default = function
  | None -> iol []
  | Some default ->
      let ast = Piqobj.piq_of_piqi_any default in
      gen_default_ast ast


let gen_field_mode = function
    | `required -> "-"
    | `optional -> "?"
    | `repeated -> "*"


let gen_field x =
  let open F in
  let field_mode = gen_field_mode x.mode in
  iol [
    ios field_mode; ios " "; gen_name_type x.name x.typename;
    gen_default x.default;
  ]


let gen_record x =
  let open R in
  let fields = List.map gen_field x.field in
  iol [
    eol; ios "  {"; indent;
      iod "\n" fields;
    unindent;
    eol; ios "  }";
  ]


let gen_option x =
  let open O in
  iol [
    ios "| "; gen_name_type x.name x.typename
  ]


let gen_enum x =
  let open E in
  let options = List.map gen_option x.option in
  iol [
    indent;
      iod "\n" options; (* XXX: print on the same line? *)
    unindent;
  ]


let gen_variant x =
  let open V in
  let options = List.map gen_option x.option in
  iol [
    indent;
      iod "\n" options; (* XXX: try to print on the same line? *)
    unindent;
  ]


let gen_list x =
  let open L in
  iol [
    ios " [ "; gen_typename x.typename; ios " ]"
  ]


let gen_alias x =
  let open A in
  let typename =
    match x.typename with
      | Some n ->
          gen_typename n
      | None ->
          (* generate name for built-in types *)
          let piqtype = ((some_of x.piqi_type) :> T.piqtype) in
          ios "." ^^ ios (C.piqi_typename piqtype)
  in
  iol [
    ios " "; typename;
  ]


let gen_typedef_repr = function
  | `record t -> gen_record t
  | `variant t -> gen_variant t
  | `enum t -> gen_enum t
  | `list t -> gen_list t
  | `alias t -> gen_alias t


let gen_def x =
  let name = typedef_name x in
  let repr = gen_typedef_repr x in
  iol [
    ios "type "; ios name; ios " ="; repr;
  ]


let gen_sep l =
  if l <> [] then ios "\n\n" else iol []


let gen_defs (defs:T.typedef list) =
  let l = List.map gen_def defs in
  iol [
    iod "\n\n" l; gen_sep l
  ]


let gen_import x =
  let open Import in
  let name =
    match x.name with
      | None -> iol []
      | Some x -> ios " as " ^^ ios x
  in
  iol [
    ios "import "; ios x.modname; name;
  ]


let gen_imports l =
  let l = List.map gen_import l in
  iol [
    iod "\n" l; gen_sep l
  ]


let gen_includes l =
  let open Includ in
  let l = List.map (fun x -> ios "include " ^^ ios x.modname) l in
  iol [
    iod "\n" l; gen_sep l
  ]


(* boot code *)

let field_def =
  if !Sys.interactive
  then Obj.magic 1 (* don't do anything in interactive (toplevel) mode *)
  else Piqi.find_embedded_piqtype "field"

let option_def =
  if !Sys.interactive
  then Obj.magic 1 (* don't do anything in interactive (toplevel) mode *)
  else Piqi.find_embedded_piqtype "option"


let gen_extension_item x =
  let ast = Piqobj.piq_of_piqi_any x in
  (* NOTE: recognizing and printing only fields and options *)
  match ast with
  | `named {Piq_ast.Named.name = "field"; Piq_ast.Named.value = ast} ->
      let x = Piqi.mlobj_of_ast field_def T.parse_field ast in
      let res = gen_field x in
      [res]
  | `named {Piq_ast.Named.name = "option"; Piq_ast.Named.value = ast} ->
      let x = Piqi.mlobj_of_ast option_def T.parse_option ast in
      let res = gen_option x in
      [res]
  | _ -> []


let gen_extension_target = function
  | `typedef x | `name x -> [x]
  | `field x ->  [] (* "field=" ^ x *)
  | `option x -> [] (* "option=" ^ x *)
  | `import x -> [] (* "import=" ^ x *)
  | `func x ->   [] (* "function=" ^ x *)


let gen_extension x =
  let open Extend in
  (* TODO: break long list of extended names to several lines *)
  let names = U.flatmap (fun x -> gen_extension_target x) x.what in
  let items = U.flatmap gen_extension_item (x.piqi_with @ x.quote) in

  (* don't print any extensions other than fields and options *)
  if names <> [] && items <> []
  then
    let res = iol [
      ios "extend "; iod " " (List.map ios names); indent;
        iod "\n" items;
      unindent;
    ]
    in
    [res]
  else
    []


let gen_extensions l =
  let l = U.flatmap gen_extension l in
  iol [
    iod "\n\n" l; gen_sep l
  ]


let gen_param name = function
  | None -> []
  | Some x ->
      let repr =
        match x with
          | `name x -> ios " " ^^ gen_typename x
          | (#T.typedef as x) -> gen_typedef_repr x
      in
      let res = iol [
        ios name; ios " ="; repr;
      ]
      in [res]


let gen_function f =
  let open T.Func in
  let params = List.concat [
      gen_param "input" f.input;
      gen_param "output" f.output;
      gen_param "error" f.error;
    ]
  in
  iol [
    ios "function "; ios f.name; indent;
      iod "\n" params;
    unindent;
  ]


let gen_functions l =
  let l = List.map gen_function l in
  iol [
    iod "\n\n" l; gen_sep l
  ]


let gen_module = function
  | Some x -> ios "module " ^^ ios x ^^ ios "\n\n"
  | None -> iol []


let gen_piqi ch (piqi:T.piqi) =
  let open P in
  let piqi = some_of piqi.original_piqi in
  let code =
    iol [
      (* XXX: gen_module _orig_piqi.modname; *)
      gen_module piqi.modname;
      gen_imports piqi.import;
      gen_includes piqi.includ;
      (* NOTE: can't use resolved or extended typedef here, because we are
       * printing inludes and extensions separately *)
      gen_defs piqi.typedef;
      gen_extensions piqi.extend;
      gen_functions piqi.func;
    ]
  in
  Iolist.to_channel ch code

