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
 * Generate Protocol Buffers .proto file from .piqi definitions
 *)


module C = Piqi_common
open C
open Iolist
 

(* XXX: move to some common module *)
let gen_code code =
  ios (Int32.to_string (some_of code)) 
  (*
  ios (string_of_int (some_of code))
  *)


(* new implicit imports added when unrolling aliases *)
let new_imports = ref []


(* generate fully-qualified proto names using parents *)
let gen_name ?parent name =
  let name = some_of name in
  match parent with
    | Some (`import x) ->
        trace "piqi_to_proto: adding implicit import \"%s\"\n" x.Import.modname;
        new_imports := x :: !new_imports;

        let piqi = some_of x.Import.piqi in
        (match piqi.P.protobuf_package with
          | None -> name (* no .proto package => use flat .proto namespace *)
          | Some package ->
              (* build fully-qualified name *)
              "." ^ package ^ "." ^ name
        )
    | _ ->  (* local name *)
        name


(* indication if the module that is being processed is a Piqi self-spec *)
let is_self_spec = ref false



let recalc_import_parent ?parent current_parent =
  match current_parent with
    | Some (`import _) -> current_parent
    | _ -> parent (* previous import parent or None *)


let recalc_name ?name proto_name =
  match name with
    | Some name -> name
    | None -> some_of proto_name


let rec typename ?parent (t:T.piqtype) =
  let gen_name current_parent proto_name =
    let parent = recalc_import_parent ?parent current_parent in
    gen_name ?parent proto_name
  in
  match t with
    | `int -> "sint32"
    | `float -> "double"
    | `bool -> "bool"
    | `string -> "string"
    | `binary -> "bytes"
    | `record t -> gen_name t.R.parent t.R.protobuf_name
    | `variant t -> gen_name t.V.parent t.V.protobuf_name
    | `enum t -> gen_name t.E.parent t.E.protobuf_name
    | `list t -> gen_name t.L.parent t.L.protobuf_name
    | `alias t ->
        (* unwind aliases to their original type *)
        gen_alias_typename t ?parent
    | `any ->
        if !is_self_spec
        then "any" (* there is a defined type in Piqi self-spec called "any" *)
        else
          (* NOTE: protoc has a bug: when compiling C++ stubs it doesn't
           * convert proto names into fully qualified C++ names,
           * e.g. wire_type is converted to piqi::piqi::wire_type -- and not
           * ::piqi::piqi::wire_type. Non-fully qualified name in case of
           * repeated namespace parts confuses g++
           *
           * For this reason, adding "_org" suffix to the top namespace for now.
           *)
          ".piqi_org.piqi.any"


and gen_alias_typename ?parent x =
  let open Alias in
  match x.protobuf_type with
    | Some x -> x
    | None ->
        let parent = recalc_import_parent ?parent x.parent in
        typename (some_of x.piqtype) ?parent


let gen_piqtype ?parent t =
  ios (typename t ?parent)


let gen_piqtype' ?parent = function
  | None -> ios "bool"
  | Some t -> gen_piqtype t ?parent


let typedef_proto_name = function
  | `record t -> some_of t.R.protobuf_name
  | `variant t -> some_of t.V.protobuf_name
  | `enum t -> some_of t.E.protobuf_name
  | `alias t -> some_of t.A.protobuf_name
  | `list t -> some_of t.L.protobuf_name
  | _ ->
      (* this function will be called only for named types (i.e. typedefs) *)
      assert false


let protoname_of name piqtype =
  match name, piqtype with
    | Some n, _ -> ios n
    | None, Some t ->
        ios (typedef_proto_name t)
    | _ -> assert false


let protoname_of_field f =
  let open F in protoname_of f.protobuf_name f.piqtype


let protoname_of_option o =
  let open O in protoname_of o.protobuf_name o.piqtype


let gen_proto_custom proto_custom =
  let l = List.map (fun x -> iol [eol; ios x]) proto_custom in
  iol l


let string_of_mode = function
    | `required -> "required"
    | `optional -> "optional"
    | `repeated -> "repeated"


let make_default_io x =
  iol [ ios " [default = "; x; ios "]" ]


let make_default s =
  make_default_io (ios s)


(* NOTE: not generating defaults for structured objects *)
let rec gen_default_obj (x:Piqobj.obj) =
  match x with
    (* built-in types *)
    | `int x ->
        make_default (Int64.to_string x)
    | `uint x ->
        make_default (Printf.sprintf "%Lu" x)
    | `float x ->
        (* NOTE: This is correct, since Ocaml follows the same notation for
         * floats as in Protobuf's ".proto" files: nan, * inf, -inf *)
        make_default (Piq_gen.string_of_float x)
    | `bool true -> make_default "true"
    | `bool false -> make_default "false"
    | `string x ->
        (* NOTE: Piqi escapes is a subset of C escapes used by Protobuf *)
        make_default_io (ioq (Piq_lexer.escape_string x))
    | `binary x ->
        make_default_io (ioq (Piq_lexer.escape_binary x))
    | `enum x ->
        let o = x.Piqobj.Enum.option in
        let o = o.Piqobj.Option.t in
        make_default (some_of o.O.protobuf_name)
    | `alias x ->
        gen_default_obj x.Piqobj.Alias.obj (* recurse *)
    | `any _ | `record _ | `variant _ | `list _ ->
        (* NOTE: Protobuf doesn't support defaults for complex structures *)
        warning x "dropping default value as .proto doesn't support structured defaults";
        iol []


(* using the same technique as in Piqi.resolve_field_default *)
let gen_default x =
  let open F in
  match x.default, x.piqtype with
    | Some any, Some piqtype ->
        let ast = Piqobj.piq_of_piqi_any any in
        let piqobj = Piqobj_of_piq.parse_obj piqtype ast in
        gen_default_obj piqobj
    | _, _ -> iol [] (* there is no default *)


let gen_field parent f =
  let open F in
  let packed =
    if f.wire_packed
    then " [packed = true]"
    else ""
  in
  let fdef = iod " " (* field definition *)
    [
      ios (string_of_mode f.mode);
      gen_piqtype' f.piqtype ?parent;
      protoname_of_field f; ios "=";
        gen_code f.code ^^ gen_default f ^^ ios packed ^^ ios ";";
    ]
  in
  fdef


let gen_record ?name ?parent r =
  let open R in
  let parent = recalc_import_parent ?parent r.parent in
  let name = recalc_name ?name r.protobuf_name in
  (* field definition list *) 
  let fdefs = List.map (gen_field parent) r.field in
  let rdef = iol
    [
      ios "message "; ios name;
      ios " {"; indent;
        iod "\n" fdefs;
        gen_proto_custom r.protobuf_custom;
        unindent; eol;
      ios "}"; eol;
    ]
  in rdef


let gen_const prefix c =
  let open O in
  let name = ios (some_of c.protobuf_name) in
  let prefixed_name =
    match prefix with
      | None -> name
      | Some p -> ios p ^^ name
  in
  iod " " [
    prefixed_name; ios "="; gen_code c.code ^^ ios ";";
  ]


let gen_enum e =
  let open E in
  let enum_name = some_of e.protobuf_name in
  let prefix = e.protobuf_prefix in
  let const_defs = List.map (gen_const prefix) e.option in
  let make_enum_def name =
    iol [
      ios "enum "; ios name; ios " {"; indent;
        iod "\n" const_defs;
        gen_proto_custom e.protobuf_custom;
        unindent; eol;
      ios "}";
    ]
  in
  if e.is_func_param (* embedded function parameter *)
  then
    (* we need to generate an enclosing message for enum function parameter *)
    iol [
        ios "message "; ios enum_name;
        ios " {"; indent;
          make_enum_def enum_name; ios ";"; eol;
          ios "required "; ios enum_name; ios " elem = 1;";
        unindent; eol;
        ios "}"; eol;
    ]
  else
    make_enum_def enum_name ^^ eol


let gen_option parent o =
  let open Option in
  iod " " [
    ios "optional"; gen_piqtype' o.piqtype ?parent;
      protoname_of_option o; ios "="; gen_code o.code ^^ ios ";";
  ]


let gen_variant ?name ?parent v =
  let open Variant in
  let parent = recalc_import_parent ?parent v.parent in
  let name = recalc_name ?name v.protobuf_name in
  (* field definition list *) 
  let vdefs = List.map (gen_option parent) v.option in
  let vdef = iol
    [
      ios "message "; ios name;
      ios " {"; indent;
        iod "\n" vdefs;
        gen_proto_custom v.protobuf_custom;
        unindent; eol;
      ios "}"; eol;
    ]
  in vdef


let gen_list ?name ?parent l =
  let open L in
  let parent = recalc_import_parent ?parent l.parent in
  let name = recalc_name ?name l.protobuf_name in
  let packed =
    if l.wire_packed
    then " [packed = true]"
    else ""
  in
  let ldef = iol
    [
      ios "message "; ios name;
      ios " {"; indent;
        ios "repeated "; gen_piqtype (some_of l.piqtype) ?parent; ios " elem = 1"; ios packed; ios ";";
        gen_proto_custom l.protobuf_custom;
        unindent; eol;
      ios "}"; eol;
    ]
  in ldef


let gen_noalias_def ?name ?parent t =
  match t with
    | `record t -> gen_record t ?name ?parent
    | `variant t -> gen_variant t ?name ?parent
    | `enum t -> gen_enum t
    | `list t -> gen_list t ?name ?parent


let rec gen_def ?name ?parent ?is_func_param def =
  match def with
    | `alias t ->
        gen_alias t ?name ?parent ?is_func_param
    | `record _ | `variant _ | `enum _ | `list _ as x  ->
        let res = gen_noalias_def x ?name ?parent
        in [res]


and gen_alias ?name ?parent ?(is_func_param=false) a =
  let open A in
  let is_func_param = is_func_param || a.is_func_param in
  let parent = recalc_import_parent ?parent a.parent in
  let name = recalc_name ?name a.protobuf_name in
  match some_of a.piqtype with
    | `record _ | `variant _ | `alias _ | `list _ as def ->
        (* generate the original definition with the new name *)
        (* XXX: make such generation optional, just to be able to have less
         * Protobuf-generated code in the end *)
        gen_def def ~name ~is_func_param ?parent
    | _ -> (* alias of a pritmitive type or enum *)
        if is_func_param
        then
          (* for primitive function parameters, generate a record that includes
           * one field of that type *)
          let res =
            iol [
              ios "message "; ios name;
              ios " {"; indent;
              ios "required "; gen_piqtype (some_of a.piqtype) ?parent; ios " elem = 1;";
              unindent; eol;
              ios "}"; eol;
            ]
          in [res]
        else
          (* skip as there's no way to represent redefinitions on of primitive
           * type in protobuf *)
          []


let gen_defs (defs:T.typedef list) =
  let defs = U.flatmap gen_def defs in
  iod "\n" defs


let gen_import modname =
  iol [
    ios "import "; ioq (modname ^ ".piqi.proto"); ios ";";
    eol;
  ]


let gen_imports l =
  let modnames = List.map (fun x -> some_of x.Import.orig_modname) l in
  (* using C.uniq to prevent importing a module more than once, otherwise protoc
   * will fail to compile *)
  let l = List.map gen_import (U.uniq modnames) in
  iol l


let gen_piqi (piqi:T.piqi) =
  let open P in
  let package =
    match piqi.P.protobuf_package with
      | None -> iol [] (* no package name *)
      | Some n -> iol [ios "package "; ios n; ios ";"; eol; eol]
  in

  (* add import "piqi.org/piqtype.piqi.proto" if 'any' piqtype is used *)
  is_self_spec := C.is_self_spec piqi;

  let defs = gen_defs piqi.P.resolved_typedef in
  let piqi_import =
    if C.depends_on_piqi_any piqi && not !is_self_spec
    then
      iol [
        ios "import \"piqi.org/piqi.piqi.proto\";";
        eol;
      ]
    else iol []
  in
  let proto_custom =
    List.map (fun x -> iol [ios x; eol; eol]) piqi.P.protobuf_custom
  in
  iol [
    package;
    iol proto_custom;
    piqi_import;
    gen_imports ((List.rev !new_imports) @ piqi.P.resolved_import);
    eol;
    defs;
    eol;
  ]


(*
 * set proto names if not specified by user
 *)

(* proto name of piqi name *)
let proto_name n =
  let n = Piqi_name.get_local_name n in (* cut module path *)
  U.dashes_to_underscores n


let protoname n =
  Some (proto_name n)


(* variant of protoname for optional names *)
let protoname' n =
  match n with
    | None -> None
    | Some n -> protoname n


let proto_custom_warning l =
  List.iter (fun x ->
    C.warning x ".proto-custom is deprecated; use .protobuf-custom instead"
  ) l


let proto_name_warning = function
  | None -> ()
  | Some x ->
      C.warning x ".proto-name is deprecated; use .protobuf-name instead"


let protoname_field x =
  let open Field in (
    if x.proto_name <> None
    then (
      proto_name_warning x.proto_name;
      if x.protobuf_name = None
      then x.protobuf_name <- x.proto_name;
    );
    if x.protobuf_name = None then x.protobuf_name <- protoname' x.name;
  )


let protoname_record x =
  let open Record in (
    if x.proto_name <> None
    then (
      proto_name_warning x.proto_name;
      if x.protobuf_name = None
      then x.protobuf_name <- x.proto_name;
    );
    if x.proto_custom <> []
    then (
      proto_custom_warning x.proto_custom;
      x.protobuf_custom <- x.protobuf_custom @ x.proto_custom;
    );

    if x.protobuf_name = None then x.protobuf_name <- protoname (some_of x.name);
    List.iter protoname_field x.field;
  )


let protoname_option x =
  let open Option in (
    if x.proto_name <> None
    then (
      proto_name_warning x.proto_name;
      if x.protobuf_name = None
      then x.protobuf_name <- x.proto_name;
    );

    if x.protobuf_name = None then x.protobuf_name <- protoname' x.name;
  )


let protoname_variant x =
  let open Variant in (
    if x.proto_name <> None
    then (
      proto_name_warning x.proto_name;
      if x.protobuf_name = None
      then x.protobuf_name <- x.proto_name;
    );
    if x.proto_custom <> []
    then (
      proto_custom_warning x.proto_custom;
      x.protobuf_custom <- x.protobuf_custom @ x.proto_custom;
    );

    if x.protobuf_name = None then x.protobuf_name <- protoname (some_of x.name);
    List.iter protoname_option x.option;
  )


let protoname_enum x =
  let open Enum in (
    if x.proto_name <> None
    then (
      proto_name_warning x.proto_name;
      if x.protobuf_name = None
      then x.protobuf_name <- x.proto_name;
    );
    if x.proto_custom <> []
    then (
      proto_custom_warning x.proto_custom;
      x.protobuf_custom <- x.protobuf_custom @ x.proto_custom;
    );

    if x.protobuf_name = None then x.protobuf_name <- protoname (some_of x.name);
    List.iter protoname_option x.option;
  )


let protoname_alias x =
  let open Alias in (
    if x.proto_name <> None
    then (
      proto_name_warning x.proto_name;
      if x.protobuf_name = None
      then x.protobuf_name <- x.proto_name;
    );

    if x.protobuf_name = None then x.protobuf_name <- protoname (some_of x.name);
  )


let protoname_list x =
  let open L in (
    if x.proto_name <> None
    then (
      proto_name_warning x.proto_name;
      if x.protobuf_name = None
      then x.protobuf_name <- x.proto_name;
    );
    if x.proto_custom <> []
    then (
      proto_custom_warning x.proto_custom;
      x.protobuf_custom <- x.protobuf_custom @ x.proto_custom;
    );

    if x.protobuf_name = None then x.protobuf_name <- protoname (some_of x.name);
  )


let protoname_typedef = function
  | `record x -> protoname_record x
  | `variant x -> protoname_variant x
  | `enum x -> protoname_enum x
  | `alias x -> protoname_alias x
  | `list x -> protoname_list x


let protoname_defs (defs:T.typedef list) =
  List.iter protoname_typedef defs


(* generate warning when deprecated fiels are used and provde some backwards
 * compatibiliy *)
let check_transform_piqi piqi =
  (* handle deprecated .proto-package *)
  (match piqi.P.proto_package with
    | None -> ()
    | Some proto_package ->
        C.warning proto_package ".proto-package is deprecated; use .protobuf-package instead";
        if piqi.P.protobuf_package = None
        then piqi.P.protobuf_package <- Some proto_package
  );
  (* handle deprecated top-level .proto-custom *)
  let proto_custom = piqi.P.proto_custom in
  if proto_custom <> []
  then (
    proto_custom_warning proto_custom;
    piqi.P.protobuf_custom <- piqi.P.protobuf_custom @ proto_custom;
  )


let rec protoname_piqi (piqi:T.piqi) =
  let open P in
  begin
    check_transform_piqi piqi;
    protoname_defs piqi.P.resolved_typedef;
    protoname_defs piqi.P.imported_typedef;
    protoname_imports piqi.P.resolved_import;
  end


and protoname_imports imports = List.iter protoname_import imports


and protoname_import import =
  let open Import in
  begin
    protoname_piqi (some_of import.piqi)
  end


let piqi_to_proto (piqi: T.piqi) ch =
  (* add built-in type definitions to the current module *)
  piqi.P.resolved_typedef <- !C.builtin_typedefs @ piqi.P.resolved_typedef;

  (* set proto names which are not specified by user *)
  protoname_piqi piqi;
  let code = gen_piqi piqi in
  Iolist.to_channel ch code


let piqi_to_proto_file () =
  let ich = Main.open_input !Main.ifile in
  let piqi = Piqi.load_piqi !Main.ifile ich in

  if not (Filename.check_suffix !Main.ifile ".piqi")
  then piqi_error "error: input file name must have '.piqi' extension";

  let ofile =
    if !Main.ofile <> ""
    then !Main.ofile
    else (Piqi_file.chop_piqi_extensions !Main.ifile) ^ ".piqi.proto"
  in
  let och = Main.open_output ofile in

  piqi_to_proto piqi och


let usage = "Usage: piqi to-proto [options] <.piqi file>\nOptions:"

let speclist = Main.common_speclist @
  [
    Main.arg__strict;
    Main.arg_o;
  ]


let run () =
  Main.parse_args () ~speclist ~usage;
  piqi_to_proto_file ()

 
let _ =
  Main.register_command run "to-proto" "convert %.piqi to %.piqi.proto"

