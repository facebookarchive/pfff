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
 * generation of OCaml -> Protocol Buffers encoders
 *)

module C = Piqic_common
open C
open Iolist


let gen_type ?(is_packed=false) context typename =
  let import, parent_piqi, typedef = C.resolve_typename context typename in
  let packed_prefix = C.gen_packed_prefix is_packed in
  let parent_mod = C.gen_parent_mod import in
  iol [
    parent_mod;
    packed_prefix; ios "gen__";
    ios (C.typedef_mlname typedef)
  ]


let gen_builtin_type context piqi_type ocaml_type wire_type is_packed =
  match piqi_type with
    | `any ->
        (* TODO, XXX: why do we need an extra closure here? *)
        if context.is_self_spec
        then
          (* XXX: handle the case when ocaml_type <> None *)
          ios "(fun code x -> gen__any code x)"
        else
          ios "(fun code x -> Piqi_piqi.gen__any code x)"
    | _ ->
        let packed_prefix = C.gen_packed_prefix is_packed in
        let typename = C.gen_builtin_type_name piqi_type ?ocaml_type in
        let wire_typename = C.gen_wire_type_name piqi_type wire_type in
        (* XXX: packed isn't used in cc mode, we can safely remove generation of
         * reference1 *)
        iol [
          (if is_packed then gen_cc "(reference1 " else gen_cc "(reference ");
            ios "Piqirun.";
            ios typename;
            ios "_to_"; packed_prefix;
            ios wire_typename;
          gen_cc ")";
        ]


let rec gen_alias_type ?wire_type ?(is_packed=false) context a =
  let open A in
  match a.typename with
    | None ->  (* this is a built-in type, so piqi_type must be defined *)
        let piqi_type = some_of a.piqi_type in
        gen_builtin_type context piqi_type a.ocaml_type wire_type is_packed
    | Some typename ->
        let import, parent_piqi, typedef = C.resolve_typename context typename in
        match typedef with
          | `alias a when wire_type <> None ->
              (* need special handing in case when higher-level alias overrides
               * protobuf_wire_type *)
              let context = C.switch_context context parent_piqi in
              gen_alias_type context a ?wire_type ~is_packed
          | _ ->
              gen_type context typename ~is_packed


let gen_field context rname f =
  let open Field in
  let fname = C.mlname_of_field context f in
  let ffname = (* fully-qualified field name *)
    iod "." [ios "x"; ios rname; ios fname]
  in 
  let code = C.gen_code f.code in
  let mode = C.gen_field_mode context f in
  let fgen =
    match f.typename with
      | Some typename ->
          (* field generation code *)
          iod " " [ 
            ios "Piqirun.gen_" ^^ ios mode ^^ ios "_field";
              code;
              gen_type context typename ~is_packed:f.protobuf_packed;
              ffname
          ]
      | None ->
          (* flag generation code *)
          iod " " [
            gen_cc "reference_if_true ";
            ios "Piqirun.gen_flag"; code; ffname;
          ]
  in (fname, fgen)


let gen_record context r =
  (* fully-qualified capitalized record name *)
  let rname = String.capitalize (some_of r.R.ocaml_name) in
  (* order fields by are by their integer codes *)
  let fields = List.sort (fun a b -> compare a.F.code b.F.code) r.R.field in
  let fgens = (* field generators list *)
    List.map (gen_field context rname) fields
  in
  (* field names *)
  let fnames, _ = List.split fgens in

  let esc x = ios "_" ^^ ios x in

  (* field generator code *)
  let fgens_code = List.map
    (fun (name, gen) -> iol [ios "let "; esc name; ios " = "; gen; ios " in"; eol])
    fgens
  in
  let unknown_fields =
    if !C.flag_gen_preserve_unknown_fields
    then [iol [ios "(Piqirun.gen_parsed_field_list "; ios "x."; ios rname; ios ".piqi_unknown_pb)"]]
    else [ios "[]"]
  in (* gen_<record-name> function delcaration *)
  iol [
    ios "gen__"; ios (some_of r.R.ocaml_name); ios " code x =";
    ioi [
      gen_cc "refer x;\n";
      iol fgens_code;
      ios "Piqirun.gen_record code (";
        iod " :: " ((List.map esc fnames) @ unknown_fields);
      ios ")";
    ]
  ]


let gen_const c =
  let open Option in
  iol [
    ios "| "; C.gen_pvar_name (some_of c.ocaml_name); ios " -> ";
      C.gen_code c.code; ios "l"; (* ocaml int32 literal *)
  ]


let gen_enum_consts l =
  let consts = List.map gen_const l in
  iol [
    ios "(match x with";
    ioi (newlines consts);
    ios ")"
  ]


let gen_unpacked_enum e =
  let open Enum in
  iol [
    ios "gen__"; ios (some_of e.ocaml_name); ios " code x =";
    ioi [
      gen_cc "refer x;\n";
      ios "Piqirun.int32_to_signed_varint code "; gen_enum_consts e.option;
    ]
  ]


let gen_packed_enum e =
  let open Enum in
  iol [
    ios "packed_gen__"; ios (some_of e.ocaml_name); ios " x =";
    ioi [
      gen_cc "refer x;\n";
      ios "Piqirun.int32_to_packed_signed_varint "; gen_enum_consts e.option;
    ]
  ]


let gen_enum e =
  (* generate two functions: one for generating normal value; another one -- for
   * packed value *)
  iol [
    gen_unpacked_enum e; eol;
    ios "and ";
    gen_packed_enum e;
  ]


let gen_option context o =
  let open Option in
  let name = C.mlname_of_option context o in
  let code = C.gen_code o.code in
  match o.typename with
    | None ->  (* this is a flag, i.e. option without a type *)
        iol [
          ios "| "; C.gen_pvar_name name; ios " -> ";
          gen_cc "refer x; ";
          ios "Piqirun.gen_bool_field "; code; ios " true";
        ]
    | Some typename ->
        let import, parent_piqi, typedef = C.resolve_typename context typename in
        match o.ocaml_name, typedef with
          | None, `variant _ | None, `enum _ ->
              (* handle variant and enum subtyping cases by lifting their labels
               * and clauses to the top level -- in fact, relying on OCaml here
               * by using #<included variant or enum type> construct *)
              let scoped_typename = Piqic_ocaml_types.gen_typedef_type context typedef ?import in
              iol [
                ios "| (#"; ios scoped_typename; ios " as x) -> ";
                  gen_type context typename; ios " "; code; ios " x";
              ]
          | _ ->
              iol [
                ios "| "; C.gen_pvar_name name; ios " x -> ";
                  gen_type context typename; ios " "; code; ios " x";
              ]


let gen_variant context v =
  let open Variant in
  let options = List.map (gen_option context) v.option in
  let typename = Piqic_ocaml_types.gen_typedef_type context (`variant v) in
  iol [
    ios "gen__"; ios (some_of v.ocaml_name); ios " code (x:"; ios typename; ios ") =";
    ioi [
      gen_cc "refer x;\n";
      ios "Piqirun.gen_record code [(match x with";
      ioi (newlines options);
      ios ")]";
    ]
  ]


let gen_unpacked_alias context a =
  let open Alias in
  iol [
    ios "gen__"; ios (some_of a.ocaml_name); ios " code x = ";
      gen_alias_type context a ?wire_type:a.protobuf_wire_type;
      ios " code";
      C.gen_convert_value context a.ocaml_type "_to_" a.typename (ios " x");
  ]


let gen_packed_alias context a =
  let open Alias in
  iol [
    ios "packed_gen__"; ios (some_of a.ocaml_name); ios " x = ";
      gen_alias_type context a ?wire_type:a.protobuf_wire_type ~is_packed:true;
      C.gen_convert_value context a.ocaml_type "_to_" a.typename (ios " x");
  ]


let gen_alias context a =
  let open Alias in
  if C.can_be_protobuf_packed context (`alias a)
  then
    (* if a value can be packed, we need to generate two functions: one for
     * generating regular (unpacked) representation, and another one for
     * generating packed form *)
    iol [
      gen_unpacked_alias context a; eol;
      ios "and ";
      gen_packed_alias context a;
    ]
  else
    gen_unpacked_alias context a


let gen_list context l =
  let open L in
  let repr = C.gen_list_repr context l in
  iol [
    ios "gen__"; ios (some_of l.ocaml_name); ios " code x = ";
      gen_cc "reference ";
        (* Piqirun.gen_(packed_)?(list|array|array32|array64) *)
        ios "(Piqirun.gen_"; repr; ios " (";
          gen_type context l.typename ~is_packed:l.protobuf_packed;
        ios ")) code x";
  ]


(* generate gen__<typename> functions *)
let gen_typedef_2 context typedef =
  match typedef with
    | `alias t -> gen_alias context t
    | `record t -> gen_record context t
    | `variant t -> gen_variant context t
    | `enum t -> gen_enum t
    | `list t -> gen_list context t


(* generate gen_<typename> functions *)
let gen_typedef_1 x =
  let name = ios (C.typedef_mlname x) in
  iol [
    ios "let gen_"; name; ios " x = ";
      ios "gen__"; name; ios " (-1) x";
  ]


let gen_typedefs context typedefs =
  if typedefs = []
  then iol []
  else
    let defs_2 = List.map (gen_typedef_2 context) typedefs in
    let defs_1 = List.map gen_typedef_1 typedefs in
    iol [
      gen_cc "let next_count = Piqloc.next_ocount\n";
      (* NOTE: providing special handling for boxed objects, since they are not
       * references and can not be uniquely identified. Moreover they can mask
       * integers which are used for enumerating objects *)
      gen_cc "let refer obj =
        let count = next_count () in
        if not (Obj.is_int (Obj.repr obj))
        then Piqloc.addref obj count\n";
      gen_cc "let reference f code x = refer x; f code x\n";
      gen_cc "let reference1 f x = refer x; f x\n";
      gen_cc "let reference_if_true f code x =
        if x
        then reference f code x
        else f code x\n\n";

      ios "let rec "; iod "and " (newlines (newlines defs_2));
      eol;
      iol (newlines defs_1);
      eol; eol;
    ]


let gen_piqi context =
  gen_typedefs context context.piqi.P.typedef

