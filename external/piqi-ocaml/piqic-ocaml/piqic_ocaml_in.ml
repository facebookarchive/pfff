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
 * generation of Protocol Buffers -> OCaml decoders
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
    packed_prefix; ios "parse_";
    ios (C.typedef_mlname typedef)
  ]


let gen_builtin_type context piqi_type ocaml_type wire_type is_packed =
  match piqi_type with
    | `any ->
        if context.is_self_spec
        then ios "parse_any" (* XXX: handle the case when ocaml_type <> None *)
        else ios "Piqi_piqi.parse_any"
    | _ ->
        let packed_prefix = C.gen_packed_prefix is_packed in
        let typename = C.gen_builtin_type_name piqi_type ?ocaml_type in
        let wire_typename = C.gen_wire_type_name piqi_type wire_type in
        iol [
          gen_cc "(fun x -> let count = next_count() in refer count (";
            ios "Piqirun.";
            ios typename;
            ios "_of_"; packed_prefix;
            ios wire_typename;
          gen_cc " x))";
        ]


(* copy-pasted Piqic_ocaml_out.gen_alias_type -- not sure how to avoid this *)
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


(* TODO: parse defaults once at boot time rather than each time when we need to
 * parse a field *)
let gen_default = function
  | None -> iol []
  | Some piqi_any ->
      let pb = some_of piqi_any.Any.protobuf in
      iol [ios "~default:"; ioq (String.escaped pb) ]


let esc x = ios "_" ^^ ios x


let gen_field_cons context rname f =
  let open Field in
  let fname = C.mlname_of_field context f in
  let ffname = (* fully-qualified field name *)
    iol [ios rname; ios "."; ios fname]
  in 
  (* field construction code *)
  iol [ ffname; ios " = "; esc fname; ios ";" ]


let gen_field_parser context f =
  let open Field in
  let fname = C.mlname_of_field context f in
  let mode = C.gen_field_mode context f in
  let code = C.gen_code f.code in
  let fcons =
  match f.typename with
    | Some typename ->
        (* field constructor *)
        iod " " [
          (* "parse_(required|optional|repeated)_field" function invocation *)
          ios "Piqirun.parse_" ^^ ios mode ^^ ios "_field";
            code;
            gen_type context typename ~is_packed:f.protobuf_packed;

            (* when parsing packed repeated fields, we should also accept
             * fields in unpacked representation; therefore, specifying an
             * unpacked field parser as another parameter *)
            if f.protobuf_packed
            then gen_type context typename
            else iol [];

            ios "x";
            gen_default f.default;
        ]
    | None ->
        (* flag constructor *)
        iod " " [ 
          gen_cc "incr_count_if_true (";
            ios "Piqirun.parse_flag"; code; ios "x";
          gen_cc ")";
        ]
  in
  (* field parsing code *)
  iol [ ios "let "; esc fname; ios ", x = "; fcons; ios " in"; eol ]


let gen_record context r =
  (* fully-qualified capitalized record name *)
  let rname = String.capitalize (some_of r.R.ocaml_name) in
  (* order fields by are by their integer codes *)
  let fields = List.sort (fun a b -> compare a.F.code b.F.code) r.R.field in
  let fconsl = (* field constructor list *)
    if fields <> []
    then List.map (gen_field_cons context rname) fields
    else [ios rname; ios "._dummy = ();"]
  in
  let fconsl =
    if !C.flag_gen_preserve_unknown_fields
    then fconsl @ [iol [ios rname; ios ".piqi_unknown_pb = x;"]]
    else fconsl
  in
  let fparserl = (* field parsers list *)
    List.map (gen_field_parser context) fields
  in
  let rcons = (* record constructor *)
    iol [
      iol fparserl;
      ios "Piqirun.check_unparsed_fields x;"; eol;
      ios "{";
      ioi (newlines fconsl);
      ios "}";
    ]
  in (* parse_<record-name> function delcaration *)
  iol [
    ios "parse_"; ios (some_of r.R.ocaml_name); ios " x =";
    ioi [
      ios "let x = Piqirun.parse_record x in"; eol;
      gen_cc "let count = next_count() in refer count (\n";
      rcons;
      gen_cc ")\n";
    ]
  ]


let gen_const c =
  let open Option in
  let name = C.gen_pvar_name (some_of c.ocaml_name) in
  iol [ios "| "; C.gen_code c.code; ios "l -> "; name]


let gen_enum e ~is_packed =
  let open Enum in
  let consts = List.map gen_const e.option in
  let packed_prefix = C.gen_packed_prefix is_packed in
  iol [
    packed_prefix; ios "parse_"; ios (some_of e.ocaml_name); ios " x =";
    ioi [
      gen_cc "let count = next_count() in refer count (";
        ios "match Piqirun.int32_of_"; packed_prefix; ios "signed_varint x with";
        ioi [
          iol (newlines consts);
          ios "| x -> Piqirun.error_enum_const x";
        ];
      gen_cc ")\n";
    ]
  ]


let gen_enum e =
  (* generate two functions: one for parsing normal value; another one -- for
   * packed value *)
  iol [
    gen_enum e ~is_packed:false; eol;
    ios "and ";
    gen_enum e ~is_packed:true
  ]


let gen_option context varname o =
  let open Option in
  let name = C.mlname_of_option context o in
  let code = C.gen_code o.code in
  match o.typename with
    | None ->  (* this is a flag, i.e. option without a type *)
        iol [
          ios "| "; code; ios " when x = Piqirun.Varint 1 -> ";
            (* NOTE: providing special handling for boxed values, see "refer" *)
            gen_cc "let count = next_count() in refer count ";
            C.gen_pvar_name name;
        ]
    | Some typename ->
        let import, parent_piqi, typedef = C.resolve_typename context typename in
        match o.ocaml_name, typedef with
          | None, `variant _ | None, `enum _ ->
              (* handle variant and enum subtyping cases by lifting their labels
               * and clauses to the top level -- in fact, relying on OCaml here
               * by using #<included variant or enum type> construct *)
              iol [
                ios "| "; code; ios " -> ";
                  ios "("; gen_type context typename; ios " x :> "; ios varname; ios ")"
              ]
          | _ ->
              iol [
                ios "| "; code; ios " ->";
                indent (ioi [
                  ios "let res = ";
                    gen_cc "let count = curr_count() in refer count (";
                    gen_type context typename; ios " x";
                    gen_cc ")";
                    ios " in"; eol;
                    C.gen_pvar_name name; ios " res";
                ])
              ]


let gen_variant context v =
  let open Variant in
  let name = some_of v.ocaml_name in
  let scoped_name = C.scoped_name context name in
  let options = List.map (gen_option context scoped_name) v.option in
  iol [
    ios "parse_"; ios name; ios " x =";
    ioi [
      ios "let code, x = Piqirun.parse_variant x in"; eol;
      gen_cc "let count = next_count() in refer count (\n";
      ios "match code with";
      ioi [
        iol (newlines options);
        ios "| _ -> Piqirun.error_variant x code";
      ];
      gen_cc ")\n";
    ]
  ]


let gen_alias context a ~is_packed =
  let open Alias in
  let packed_prefix = C.gen_packed_prefix is_packed in
  iol [
    packed_prefix; ios "parse_"; ios (some_of a.ocaml_name); ios " x = ";
    C.gen_convert_value context a.ocaml_type "_of_" a.typename (
      iol [
        gen_alias_type context a ?wire_type:a.protobuf_wire_type ~is_packed;
        ios " x";
      ]
    )
  ]


let gen_alias context a =
  let open Alias in
  if C.can_be_protobuf_packed context (`alias a)
  then
    (* if a value can be packed, we need to generate two functions: one for
     * parsing regular (unpacked) representation, and another one for
     * parsing packed form *)
    iol [
      gen_alias context a ~is_packed:false; eol;
      ios "and ";
      gen_alias context a ~is_packed:true
    ]
  else
    gen_alias context a ~is_packed:false


let gen_list context l =
  let open L in
  let repr = C.gen_list_repr context l in
  iol [
    ios "parse_"; ios (some_of l.ocaml_name); ios " x ="; eol;
      gen_cc "  let count = next_count() in refer count (\n";
        (* Piqirun.parse_(packed_)?(list|array|array32|array64) *)
        ios "  Piqirun.parse_"; repr;
          ios " ("; gen_type context l.typename ~is_packed:l.protobuf_packed; ios ")";

          (* when parsing packed repeated fields, we should also accept
           * fields in unpacked representation; therefore, specifying an
           * unpacked field parser as another parameter *)
          if l.protobuf_packed
          then iol [
            ios " ("; gen_type context l.typename; ios ")";
          ]
          else iol [];

          ios " x"; eol;
      gen_cc "  )\n";
  ]


let gen_typedef context typedef =
  match typedef with
    | `record t -> gen_record context t
    | `variant t -> gen_variant context t
    | `enum t -> gen_enum t
    | `list t -> gen_list context t
    | `alias t -> gen_alias context t


let gen_typedefs context typedefs =
  if typedefs = []
  then iol []
  else
    let defs = List.map (gen_typedef context) typedefs in
    iol [
      gen_cc "let next_count = Piqloc.next_icount\n";
      gen_cc "let curr_count () = !Piqloc.icount\n";
      (* NOTE: providing special handling for boxed objects, since they are not
       * references and can not be uniquely identified. Moreover they can mask
       * integers which are used for enumerating objects *)
      gen_cc "let refer ref obj =
        if not (Obj.is_int (Obj.repr obj))
        then Piqloc.addrefret ref obj
        else obj\n";
      gen_cc "let incr_count_if_true ((obj, _) as res) =
        if obj then ignore(next_count());
        res\n\n";

      ios "let rec "; iod "and " (newlines (newlines defs));
      eol
    ]


let gen_piqi context =
  gen_typedefs context context.piqi.P.typedef

