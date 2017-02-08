(*
   Copyright 2009, 2010, 2011, 2012, 2013 Anton Lavrik

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
 * generation of Ocaml type definitions
 *)

module C = Piqic_common
open C
open Iolist


let gen_builtin_type context piqi_type =
  match piqi_type with
    | `any ->
        if context.is_self_spec
        then C.scoped_name context "any"
        else "Piqi_piqi.any"
    | t ->
        C.gen_builtin_type_name t


let gen_typedef_type ?import context typedef =
  let ocaml_name = C.typedef_mlname typedef in
  match import with
    | None ->  (* local typedef *)
        C.scoped_name context ocaml_name
    | Some import ->
        let ocaml_modname = some_of import.Import.ocaml_name in
        (ocaml_modname ^ "." ^ ocaml_name)


(* XXX: check type compatibility *)
let rec gen_type context typename = 
  let import, parent_piqi, typedef = C.resolve_typename context typename in
  let context = C.switch_context context parent_piqi in
  match typedef with
    | `alias a ->
        let ocaml_name = some_of a.A.ocaml_name in
        (* skip cyclic type abbreviations *)
        let ocaml_type = gen_alias_type context a in
        if ocaml_name = ocaml_type (* cyclic type abbreviation? *)
        then ocaml_type
        else gen_typedef_type context typedef ?import
    | _ ->  (* record | variant | list | enum *)
        gen_typedef_type context typedef ?import


and gen_alias_type context a =
  let open Alias in
  match a.ocaml_type, a.typename with
    | Some x, _ -> x
    | None, None ->
        (* this is an alias for a built-in type (piqi_type field must be defined
         * when neither of type and ocaml_type fields are present) *)
        gen_builtin_type context (some_of a.piqi_type)
    | None, Some typename ->
        gen_type context typename


let ios_gen_type context typename =
  ios (gen_type context typename)


let gen_field_type context f =
  let open F in
  match f.typename with
    | None -> ios "bool"; (* flags are represented as booleans *)
    | Some typename ->
        let deftype = ios_gen_type context typename in
        match f.mode with
          | `required -> deftype
          | `optional when f.default <> None && (not f.ocaml_optional) ->
              deftype (* optional + default *)
          | `optional -> deftype ^^ ios " option"
          | `repeated ->
              deftype ^^
              if f.ocaml_array
              then ios " array"
              else ios " list"


let gen_field context f = 
  let open F in
  iol [
    ios "mutable ";  (* NOTE: defining all fields as mutable at the moment *)
    ios (C.mlname_of_field context f);
    ios ": ";
    gen_field_type context f;
    ios ";"
  ]


(* generate record type in record module; see also gen_record' *)
let gen_record_mod context r =
  let modname = String.capitalize (some_of r.R.ocaml_name) in
  let fields = r.R.field in
  let fdefs = (* field definition list *)
    if fields <> []
    then List.map (gen_field context) fields
    else [ios "_dummy: unit;"]
  in
  let fdefs =
    if !C.flag_gen_preserve_unknown_fields
    then fdefs @ [ios "piqi_unknown_pb: (int * Piqirun.t) list;"]
    else fdefs
  in
  (* record def constructor *)
  let rcons = [
    ios "type t = "; ios "{";
    ioi (newlines fdefs);
    ios "}";
  ]
  in iol [
    ios modname; ios ":";
    ioi [
      ios "sig";
      ioi rcons;
      ios "end"; ios " = "; ios modname;
    ]
  ]


let gen_option context o =
  let open Option in
  match o.ocaml_name, o.typename with
    | ocaml_name, Some typename -> (
        let import, parent_piqi, typedef = C.resolve_typename context typename in
        match ocaml_name, typedef with
          | None, `variant x ->
              (* NOTE: for some reason, ocaml complains about fully qualified
               * polymorphic variants in recursive modules, so we need to use
               * non-qualified names in this case *)
              if import = None  (* local typedef? *)
              then ios (some_of x.V.ocaml_name)
              else ios_gen_type context typename
          | None, `enum x ->
              if import = None  (* local typedef? *)
              then ios (some_of x.E.ocaml_name)
              else ios_gen_type context typename
          | _ ->
              (* same as C.mlname_of_option but avoid resoving the same type
               * again *)
              let mlname =
                match ocaml_name with
                  | Some n -> n
                  | None -> C.typedef_mlname typedef
              in
              let n = C.gen_pvar_name mlname in
              n ^^ ios " of " ^^ ios_gen_type context typename
        )
    | Some n, None ->
        C.gen_pvar_name n
    | None, None ->
        assert false


let gen_alias context a =
  let open Alias in
  let ocaml_name = some_of a.ocaml_name in
  let ocaml_type = gen_alias_type context a in
  if ocaml_name = ocaml_type (* cyclic type abbreviation? *)
  then [] (* avoid generating cyclic type abbreviations *)
  else [iol [
    ios ocaml_name; ios " = "; ios ocaml_type;
  ]]


let gen_list context l =
  let open L in
  iol [
    ios (some_of l.ocaml_name); ios " = ";
      ios_gen_type context l.typename;
      if l.ocaml_array
      then ios " array"
      else ios " list";
  ]


let gen_options context options =
  let options_code = List.map (gen_option context) options in
  ioi [
    ios "[";
    ioi (prefix "| " options_code |> newlines);
    ios "]"
  ]


let gen_variant context v =
  let open Variant in
  iol [
    ios (some_of v.ocaml_name); ios " =";
    gen_options context v.option;
  ]


let gen_enum context e =
  let open Enum in
  iol [
    ios (some_of e.ocaml_name); ios " =";
    gen_options context e.option;
  ]


let gen_record context r =
  let name = some_of r.R.ocaml_name in
  let modname = String.capitalize name in
  iol [ ios name; ios " = "; ios (modname ^ ".t") ]


let gen_typedef context typedef =
  match typedef with
    | `record t -> [gen_record context t]
    | `variant t -> [gen_variant context t]
    | `enum t -> [gen_enum context t]
    | `list t -> [gen_list context t]
    | `alias t -> gen_alias context t


let gen_mod_typedef context typedef =
  match typedef with
    | `record r ->
        [gen_record_mod context r]
    (* XXX: generate modules for variants? *)
    | _ -> []


let gen_typedefs context (typedefs:T.typedef list) =
  let top_modname = C.top_modname context in
  (* generated typedefs that must be wrapped into ocaml modules *)
  let def_mods = U.flatmap (gen_mod_typedef context) typedefs in
  (* generated the rest of typedefs wrapped into into an ocaml module *)
  let other_defs = U.flatmap (gen_typedef context) typedefs in
  let top_mod = iol [
    ios top_modname;
    ios ":";
    ioi [
      ios "sig";
      ioi (prefix "type " other_defs |> newlines);
      ios "end"; ios " = "; ios top_modname;
    ]
  ]
  in
  let code = iol [
    ios "module rec "; top_mod;
    iol (prefix "and " def_mods);
  ]
  in
  iol [
    code;
    eol;
  ]


let gen_import context import =
  let open Import in
  let index = C.resolve_import context import in
  let piqi = index.i_piqi in
  iod " " [
    ios "module "; ios (some_of import.ocaml_name); ios "=";
        ios (some_of piqi.P.ocaml_module);
    eol; eol
  ]


let gen_imports context l =
  let l = List.map (gen_import context) l in
  iol l


let default_visitor _ = ()


(* depth-first graph traversal *)
let dfs
    ?(pre_visit = default_visitor)
    ?(cycle_visit = default_visitor)
    ?(post_visit = default_visitor)
    (nodes: 'a list)
    (get_adjacent_vertixes: ('a -> 'a list)) =
  let black = ref [] in (* visited nodes, i.e. after last_visit *)
  let grey = ref [] in (* nodes between first_visit and last_visit *)
  let set_color node = function
    | `black -> black := node::!black
    | `grey -> grey := node::!grey
  in
  let get_color node =
    if List.memq node !black
    then `black
    else if List.memq node !grey
    then `grey
    else `white
  in
  let rec aux node =
    match get_color node with
      | `black -> () (* already processed -- nothing to do *)
      | `grey -> (* found a cycle -- run a handler and return *)
          cycle_visit node
      | `white ->
          set_color node `grey;
          pre_visit node; (* run a pre-visit handler *)

          List.iter aux (get_adjacent_vertixes node);

          set_color node `black;
          post_visit node (* run a post-visit handler *)
  in
  List.iter aux nodes


(* topological sort of a graph *)
let tsort
    ?(cycle_visit = (fun _ -> failwith "found a cycle!"))
    (nodes: 'a list)
    (get_adjacent_vertixes: ('a -> 'a list)) : 'a list =
  let stack = ref [] in
  let post_visit node =
    stack := node::!stack
  in
  dfs nodes get_adjacent_vertixes ~post_visit ~cycle_visit;
  List.rev !stack


(* NOTE: for some reason, ocaml complains about fully qualified polymorphic
 * variants in recursive modules, so instead of relying on OCaml, we need to
 * preorder variants ourselves without relying on OCaml to figure out the order
 * automatically *)
let order_variants context l =
  (* topologically sort local variant defintions *)
  let cycle_visit def =
    C.error ("cyclic OCaml variant definition: " ^ typedef_name def)
  in
  let get_adjacent_vertixes = function
    | `variant v ->
        (* get the list of included variants *)
        U.flatmap (fun o ->
          match o.O.typename with
            | Some typename when o.O.ocaml_name = None ->
                let import, parent_piqi, typedef = C.resolve_typename context typename in
                (match typedef with
                  | ((`variant _) as typedef)
                  | ((`enum _) as typedef) ->
                      if import <> None (* imported? *)
                      then [] (* omit any imported definitions *)
                      else [typedef]
                  | _ -> []
                )
            | _ -> []
        ) v.V.option
    | _ -> []
  in
  tsort l get_adjacent_vertixes ~cycle_visit


(* make sure we define aliases for built-in ocaml types first; some aliases
 * (e.g. float) can override the default OCaml type names which results in
 * cyclic type definitions without such ordering *)
let order_aliases l =
  let rank def =
    match def with
      | `alias x ->
          if C.is_builtin_alias x
          then
            (* aliases of built-in OCaml types go first *)
            if x.A.ocaml_type <> None then 1 else 2
          else 100
      | _ ->
          assert false
  in
  let compare_alias a b =
    rank a - rank b
  in
  List.stable_sort compare_alias l


let order_typedefs context typedefs =
  (* we apply this specific ordering only to variants, to be more specific --
   * only to those variants that include other variants by not specifying tags
   * for the options *)
  let variants, rest =
    List.partition (function
      | `variant _ | `enum _ -> true
      | _ -> false)
    typedefs
  in
  let aliases, rest =
    List.partition (function
      | `alias _ -> true
      | _ -> false)
    rest
  in
  (* return the updated list of definitions with sorted variants and aliases *)
  (order_aliases aliases) @ (order_variants context variants) @ rest


let gen_piqi context =
  let piqi = context.piqi in
  let typedefs = order_typedefs context piqi.P.typedef in
  iol [
    gen_imports context piqi.P.import;
    gen_typedefs context typedefs;
    eol; eol
  ]

