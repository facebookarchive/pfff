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
 * This module contain functionality that is used by various parts of
 * piqi compilers.
 *)

module T = Piqic_piqi


module Record = T.Record
module Field = T.Field
module Variant = T.Variant
module Option = T.Option
module Enum = T.Enum
module Alias = T.Alias


module Import = T.Import
module Func = T.Func
module Any = T.Any


module R = Record
module F = Field
module V = Variant
module O = Option
module E = Enum
module A = Alias
module L = T.Piqi_list
module P = T.Piqi


(* utils *)
module Utils =
  struct
    (* substitute character [x] with [y] in string [s] *)
    let string_subst_char s x y =
      if not (String.contains s x)
      then s
      else
        (* preserve the original string *)
        let s = String.copy s in
        for i = 0 to (String.length s) - 1
        do
          if s.[i] = x
          then s.[i] <- y
        done; s


    let dashes_to_underscores s =
      string_subst_char s '-' '_'


    let list_of_string s =
      let n = String.length s in
      let rec aux i =
        if i < n
        then s.[i] :: (aux (i+1))
        else []
      in aux 0


    let string_of_list l =
      let s = String.create (List.length l) in
      let rec aux i = function
        | [] -> ()
        | h::t ->
            s.[i] <- h; aux (i+1) t
      in
      aux 0 l; s


    let string_startswith s prefix =
      let len = String.length prefix in
      let rec aux i =
        if i = len
        then true
        else
          if s.[i] <> prefix.[i]
          then false
          else aux (i+1)
      in
      try
        aux 0
      with _ ->
        false


    (* list flatmap *)
    let flatmap f l =
      List.concat (List.map f l)


    (* NOTE: naive, non-tail recursive. Remove duplicates from the list using
     * reference equality, preserves the initial order *)
    let rec uniqq = function
      | [] -> []
      | h::t ->
          let t = uniqq t in
          if List.memq h t then t else h :: t


    (* leave the first of the duplicate elements in the list instead of the last *)
    let uniqq l =
      List.rev (uniqq (List.rev l))


    let rec uniq = function
      | [] -> []
      | h::t ->
          let t = uniq t in
          if List.mem h t then t else h :: t


    (* leave the first of the duplicate elements in the list instead of the last *)
    let uniq l =
      List.rev (uniq (List.rev l))


    (* analogous to Filename.dirname but with forward slashes only *)
    let get_module_name x =
      try
        let pos = String.rindex x '/' in
        let res = String.sub x 0 pos in
        Some res
      with
        Not_found -> None


    (* analogous to Filename.basename but with forward slashes only *)
    let get_local_name x =
      try
        let pos = String.rindex x '/' in
        String.sub x (pos + 1) ((String.length x) - pos - 1)
      with
        Not_found -> x


    let is_scoped_name name = String.contains name '/'


    let split_name x =
      get_module_name x, get_local_name x


    let normalize_list l =
      let isupper c = (c >= 'A' && c <= 'Z') in
      let tolower c =  Char.chr (Char.code c + 32) in
      let rec aux hump accu = function
        | [] -> List.rev accu
        | h::t when h = '_' || h = '-' ->
            aux true ('-'::accu) t
        | h::t when isupper h && not hump -> (* first hump character *)
            aux true ((tolower h)::'-'::accu) t
        | h::t when isupper h && hump -> (* another hump character *)
            aux hump ((tolower h)::accu) t
        | h::t when h = '.' || h = ':' || h = '/' ->
            aux true (h::accu) t
        | h::t -> (* end of hump *)
            aux false (h::accu) t
      in
      match l with
        | [] -> []
        | h::_ -> aux (isupper h) [] l


    (* check if the name is normal, i.e. no uppercase characters and no hyphens *)
    let is_normal_name s =
      let len = String.length s in
      let rec aux i =
        if i = len
        then true (* the name is normal *)
        else
          match s.[i] with
            | 'A'..'Z' | '_' -> false
            | _ -> aux (i+1)
      in
      aux 0


    (* convert an arbitary valid name to lowercase name which words are separated by
     * dashes; for example "CamelCase" will become "camel-case"; already lowercased
     * names will remain intact *)
    let normalize_name s =
      if is_normal_name s
      then s
      else string_of_list (normalize_list (list_of_string s))
  end

module U = Utils


(* a datastructure for output construction *)
module Iolist =
  struct
    type iolist =
        Ios of string
      | Iol of iolist list
      | Ioc of char
      | Eol
      | Indent of iolist

    (* iolist construction *)
    let (^^) a b =
      match a, b with
        | Ios _, Iol b -> Iol (a::b)
        | Ios " ", Eol -> Eol
        | _, _ -> Iol [a;b]

    let eol = Eol
    let ios x = Ios x
    let iol l = Iol l
    let ioc c = Ioc c
    let indent x = Indent x

    let iod delim l = (* iol with elements separated by delim *)
      let insert_delim accu x =
        match x with
          | Iol [] -> accu
          | _ -> accu ^^ (ios delim) ^^ x
      in
      match l with
        | [] -> Iol []
        | h::t ->
            List.fold_left insert_delim h t

    let ioi l = (* indented list *)
      indent (iol l)

    let ioq x = (* double-quoted string *)
      iol [ios "\""; ios x; ios "\""]

    let (|>) x f = f x

    let newlines l =
      let newline x =
        match x with
          | Indent _ -> x
          | _ -> x ^^ Eol
      in
      List.map newline l

    let prefix s l =
      let prefix x =
        ios s ^^ x
      in
      List.map prefix l

    (* iolist output *)
    let to_buffer0 buf l =
      let add_eol () =
          Buffer.add_char buf '\n';
      in
      let add_indent level =
        for i = 1 to level * 2
        do
          Buffer.add_char buf ' '
        done
      in
      let rec aux level (newline, unindent) = function
        | Eol ->
            if not unindent  (* don't print another newline after unindent *)
            then add_eol ();
            (true, false)
        | Ios s ->
            if newline
            then add_indent level;
            Buffer.add_string buf s;
            let newline =
              if s = ""
              then false
              else (s.[String.length s - 1] = '\n')
            in
            (newline, false)
        | Iol l ->
            List.fold_left (fun accu x -> aux level accu x) (newline, unindent) l
        | Ioc c ->
            if newline
            then add_indent level;
            Buffer.add_char buf c;
            (false, false)
        | Indent x ->
            if not newline  (* don't print a newline before indent *)
            then add_eol ();
            let newline, unindent = aux (level + 1) (true, unindent) x in
            if not newline
            then add_eol (); (* don't print another newline before unindent *)
            (true, true)
      in
      ignore (aux 0 (true, false) l)

    let to_buffer l =
      let buf = Buffer.create 4096 in
      to_buffer0 buf l;
      buf

    let to_string l =
      let buf = to_buffer l in
      Buffer.contents buf

    let to_channel ch l =
      let buf = to_buffer l in
      Buffer.output_buffer ch buf
  end
open Iolist


(* idtable implemented as map: string -> 'a *)
module Idtable =
  struct
    module M = Map.Make(String)

    type 'a t = 'a M.t

    let empty = M.empty

    let add idtable name entry =
      M.add name entry idtable

    let find idtable name =
      M.find name idtable

    let remove idtable name =
      M.remove name idtable

    let mem idtable name =
      M.mem name idtable

    let fold f accu idtable =
      M.fold f idtable accu
  end


(* indexes of Piqi module contents *)
type index = {
    i_piqi: T.piqi;
    import: Import.t Idtable.t;  (* import name -> Import.t *)
    typedef: T.typedef Idtable.t;  (* typedef name -> Typedef.t *)
}


type context = {
    (* the module being processed *)
    piqi: T.piqi;
    (* index of the piqi module being compiled *)
    index: index;

    (* indication whether the module that is being processed is a Piqi
     * self-spec, i.e. the module's name is "piqi" or it includes another module
     * named "piqi" *)
    is_self_spec: bool;

    (* original modules being compiled (imported modules ++ [piqi]) *)
    modules: T.piqi list;

    (* index of imported modules: piqi.modname -> index *)
    module_index: index Idtable.t;
}


(*
 * Commonly used functions
 *)

let some_of = function
  | Some x -> x
  | None -> assert false


let error = Piqi_common.piqi_error
let warning = Piqi_common.piqi_warning


let gen_code = function
  | None -> assert false
  | Some code -> ios (Int32.to_string code)


(* polymorphic variant name starting with a ` *)
let gen_pvar_name name =
  ios "`" ^^ ios name


(*
 * set ocaml names if not specified by user
 *)

(* command-line flags *)
let flag_normalize_names = ref true
let flag_cc = ref false
let flag_gen_preserve_unknown_fields = ref false


(* ocaml name of piqi name *)
let ocaml_name n =
  let n =
    if !flag_normalize_names
    then U.normalize_name n
    else n
  in
  U.dashes_to_underscores n


let ocaml_lcname n = (* lowercase *)
  String.uncapitalize (ocaml_name n)


let ocaml_ucname n = (* uppercase *)
  String.capitalize (ocaml_name n)


let mlname target_name n =
  match target_name with
    | Some _ ->
        target_name
    | None ->
        Some (ocaml_lcname n)


(* variant of mlname for optional names *)
let mlname_opt target_name n =
  match target_name with
    | Some _ -> target_name
    | None ->
        match n with
          | None -> None
          | Some n ->
              Some (ocaml_lcname n)


let mlname_field x =
  let open Field in (
    if x.ocaml_array && x.mode <> `repeated
    then error ".ocaml-array flag can be used only with repeated fields";

    if x.ocaml_optional && x.mode <> `optional
    then error ".ocaml-optional flag can be used only with optional fields";

    {x with ocaml_name = mlname_opt x.ocaml_name x.name}
  )


let mlname_record x =
  R.({
    x with
    ocaml_name = mlname x.ocaml_name x.name;
    field = List.map mlname_field x.field;
  })


let mlname_option (x: T.option) :T.option =
  O.({x with ocaml_name = mlname_opt x.ocaml_name x.name})


let mlname_variant (x: T.variant) :T.variant =
  V.({
    x with
    ocaml_name = mlname x.ocaml_name x.name;
    option = List.map mlname_option x.option;
  })


let mlname_enum x =
  E.({
    x with
    ocaml_name = mlname x.ocaml_name x.name;
    option = List.map mlname_option x.option;
  })


let mlname_alias x =
  A.({x with ocaml_name = mlname x.ocaml_name x.name})


let mlname_list x =
  L.({x with ocaml_name = mlname x.ocaml_name x.name})


let mlname_typedef = function
  | `record x -> `record (mlname_record x)
  | `variant x -> `variant (mlname_variant x)
  | `enum x -> `enum (mlname_enum x)
  | `alias x -> `alias (mlname_alias x)
  | `list x -> `list (mlname_list x)


let mlname_func x =
  Func.({x with ocaml_name = mlname x.ocaml_name x.name})


let mlname_import x =
  let open Import in
  if x.ocaml_name <> None
  then x
  else
    let name =
      match x.name with
        | Some n -> n
        | None -> x.modname
    in
    {x with ocaml_name = Some (ocaml_ucname name)}


let mlname_piqi (piqi:T.piqi) =
  let open P in
  let ocaml_module =
    if piqi.ocaml_module <> None
    then piqi.ocaml_module
    else
      (* NOTE: modname is always defined in "piqi compile" output *)
      let modname = some_of piqi.modname in
      let n = U.get_local_name modname in (* strip module path *)
      Some (ocaml_ucname n ^ "_piqi")
  in
  {
    piqi with
    ocaml_module = ocaml_module;
    typedef = List.map mlname_typedef piqi.typedef;
    func = List.map mlname_func piqi.func;
    import = List.map mlname_import piqi.import;
  }


let typedef_name = function
  | `record x -> x.R.name
  | `variant x -> x.V.name
  | `enum x -> x.E.name
  | `alias x -> x.A.name
  | `list x -> x.L.name


let typedef_mlname = function
  | `record t -> some_of t.R.ocaml_name
  | `variant t -> some_of t.V.ocaml_name
  | `enum t -> some_of t.E.ocaml_name
  | `alias t -> some_of t.A.ocaml_name
  | `list t -> some_of t.L.ocaml_name


(* check whether the piqi module is a self-specification, i.e. piqi.piqi or
 * piqi.X.piqi
 *
 * XXX: this check is an approximation of the orignal criteria that says a
 * module is a self-spec if it is named piqi or includes a module named piqi. We
 * can't know this for sure, because information about included modules is, by
 * design, not preserved in "piqi compile" output *)
let is_self_spec piqi =
  if !flag_cc
  then true
  else
    let basename = U.get_local_name (some_of piqi.P.modname) in
    U.string_startswith basename "piqi." || basename = "piqi"


(* check whether the piqi module depends on "piqi-any" type (i.e. one of its
 * definitions has piqi-any as field/option/alias type *)
let depends_on_piqi_any (piqi: T.piqi) =
  let typedef_depends_on_piqi_any x =
    let is_any x = (x = "piqi-any") in
    let is_any_opt = function
      | Some x -> is_any x
      | None -> false
    in
    match x with
      | `record x -> List.exists (fun x -> is_any_opt x.F.typename) x.R.field
      | `variant x -> List.exists (fun x -> is_any_opt x.O.typename) x.V.option
      | `list x -> is_any x.L.typename
      | `alias x -> is_any_opt x.A.typename
      | `enum _ -> false
  in
  List.exists typedef_depends_on_piqi_any piqi.P.typedef


let load_self_spec () =
  let self_spec_bin = T.piqi in
  let buf = Piqirun.init_from_string self_spec_bin in
  T.parse_piqi buf


let is_builtin_alias x =
  (* presence of piqi_type field means this alias typedef corresponds to one
   * of built-in types *)
  x.A.piqi_type <> None


let is_builtin_typedef typedef =
  match typedef with
    | `alias a -> is_builtin_alias a
    | _ -> false


let make_idtable l =
  List.fold_left (fun accu (k, v) -> Idtable.add accu k v) Idtable.empty l


(* index typedefs by name *)
let index_typedefs l =
  make_idtable (List.map (fun x -> typedef_name x, x) l)


let make_import_name x =
  match x.Import.name with
    | None -> x.Import.modname
    | Some n -> n


(* index imports by name *)
let index_imports l =
  make_idtable (List.map (fun x -> make_import_name x, x) l)


(* generate an index of all imports and definitions of a given module *)
let index_module piqi =
  {
    i_piqi = piqi;
    import = index_imports piqi.P.import;
    typedef = index_typedefs piqi.P.typedef;
  }


(* make an index of module name -> index *)
let make_module_index piqi_list =
  make_idtable (List.map (fun x -> some_of x.P.modname, index_module x) piqi_list)


let option_to_list = function
  | None -> []
  | Some x -> [x]


let get_used_typenames typedef =
  let l =
    match typedef with
      | `record x ->
          U.flatmap (fun x -> option_to_list x.F.typename) x.R.field
      | `variant x ->
          U.flatmap (fun x -> option_to_list x.O.typename) x.V.option
      | `alias x ->
          (* NOTE: alias typename is undefined for lowest-level built-in types *)
          option_to_list x.A.typename
      | `list x ->
          [x.L.typename]
      | `enum _ ->
          []
  in
  U.uniq l


let rec get_used_builtin_typedefs typedefs builtins_index =
  if typedefs = []
  then []
  else
    let typenames = U.uniq (U.flatmap get_used_typenames typedefs) in
    let builtin_typenames = List.filter (Idtable.mem builtins_index) typenames in
    let builtin_typedefs = List.map (Idtable.find builtins_index) builtin_typenames in
    (* get built-in types' dependencies (that are also built-in types) -- usually
     * no more than 2-3 recursion steps is needed *)
    let res = (get_used_builtin_typedefs builtin_typedefs builtins_index) @ builtin_typedefs in
    U.uniqq res


(* append the list of built-in typedefs that are actually referenced by the
 * module *)
let add_builtin_typedefs piqi builtins_index =
  (* exclude builtin typedefs that are masked by the local typedefs *)
  let typedef_names = List.map typedef_name piqi.P.typedef in
  let builtins_index = List.fold_left Idtable.remove builtins_index typedef_names in
  let used_builtin_typedefs = get_used_builtin_typedefs piqi.P.typedef builtins_index in
  (* change the module as if the built-ins were defined locally *)
  P.({
    piqi with
    typedef = used_builtin_typedefs @ piqi.P.typedef
  })


let init piqi_list =
  let named_piqi_list = List.map mlname_piqi piqi_list in

  (* the module being compiled is the last element of the list; preceding
   * modules are imported dependencies *)
  let l = List.rev named_piqi_list in
  let piqi = List.hd l in
  let imports = List.rev (List.tl l) in

  let is_self_spec = is_self_spec piqi in
  let self_spec =
    if is_self_spec
    then piqi
    else
      let piqi = load_self_spec () in
      mlname_piqi piqi
  in
  let builtin_typedefs =
    if is_self_spec
    then
      (* for self-specs, all build-in types should be defined inside
       * XXX: remove unused built-in typedefs from generated self-spec? *)
      []
    else
      List.filter is_builtin_typedef self_spec.P.typedef
  in
  let builtins_index = index_typedefs builtin_typedefs in
  let piqi = add_builtin_typedefs piqi builtins_index in
  let imports = List.map (fun x -> add_builtin_typedefs x builtins_index) imports in

  (* index the compiled module's contents *)
  let index = index_module piqi in

  (* index imported modules *)
  let mod_index = make_module_index imports in
  {
    piqi = piqi;
    index = index;

    is_self_spec = is_self_spec;

    modules = piqi_list;
    module_index = mod_index;
  }


let switch_context context piqi =
  if context.piqi == piqi
  then context  (* already current => no-op *)
  else
    let index = Idtable.find context.module_index (some_of piqi.P.modname) in
    {
      context with
      piqi = piqi;
      index = index;
    }


(* the name of the top-level module being compiled *)
let top_modname context =
  some_of context.piqi.P.ocaml_module


let scoped_name context name =
  top_modname context ^ "." ^ name


let gen_parent_mod import =
  match import with
    | None -> iol []
    | Some x ->
        let ocaml_modname = some_of x.Import.ocaml_name in
        ios ocaml_modname ^^ ios "."


let resolve_import context import =
  Idtable.find context.module_index import.Import.modname


let resolve_local_typename ?import index name =
  let typedef = Idtable.find index.typedef name in
  (import, index.i_piqi, typedef)


(* resolve type name to its type definition and the module where it was defined
 * and the import its module was imported with *)
let resolve_typename context typename =
  let index = context.index in
  match U.split_name typename with
    | None, name ->  (* local type *)
        (* NOTE: this will also resolve built-in types *)
        resolve_local_typename index name
    | Some import_name, name ->  (* imported type *)
        let import = Idtable.find index.import import_name in
        let imported_index = resolve_import context import in
        resolve_local_typename imported_index name ~import


(* unwind aliases to the lowest-level non-alias typedef or one of the built-in
 * primitive Piqi types *)
type resolved_type = [ T.typedef | T.piqi_type]

let rec unalias context typedef :(T.piqi * resolved_type) =
  match typedef with
    | `alias {A.typename = Some typename} ->
        let import, parent_piqi, aliased_typedef = resolve_typename context typename in
        let parent_context = switch_context context parent_piqi in
        unalias parent_context aliased_typedef
    | `alias {A.piqi_type = Some piqi_type} ->
        context.piqi, (piqi_type :> resolved_type)
    | _ ->
        context.piqi, (typedef :> resolved_type)


let type_mlname context typename =
  let import, parent_piqi, typedef = resolve_typename context typename in
  typedef_mlname typedef


let mlname_of context ocaml_name typename =
  match ocaml_name, typename with
    | Some n, _ -> n
    | None, Some typename  ->
        type_mlname context typename
    | _ ->
        assert false


let mlname_of_field context x =
  let open F in
  mlname_of context x.ocaml_name x.typename


let mlname_of_option context x =
  let open O in
  mlname_of context x.ocaml_name x.typename


let gen_builtin_type_name ?(ocaml_type: string option) (piqi_type :T.piqi_type) =
  match ocaml_type with
    | Some x -> x
    | None ->
        match piqi_type with
          | `int -> "int"
          | `float -> "float"
          | `bool -> "bool"
          | `string | `binary -> "string"
          | `any ->
              (* must be handled separately *)
              assert false


let can_be_protobuf_packed context typedef =
  let piqi, resolved_type = unalias context typedef in
  match resolved_type with
    | `int | `float | `bool -> true
    | `enum _ -> true  (* enum values can be packed in Protobuf *)
    | _ -> false


(* custom types handling: used by piqic_ocaml_out, piqic_ocaml_in *)
let gen_convert_value context ocaml_type direction typename value =
  match typename, ocaml_type with
    | Some typename, Some ocaml_type -> (* custom OCaml type *)
        let name = type_mlname context typename in
        iol [
          ios "(";
            ios ocaml_type;
            ios direction;
            ios name;
            ios "("; value; ios ")";
          ios ")"
        ]
    | _ ->
        value


let get_default_wire_type piqi_type =
  match piqi_type with
    | `int -> `zigzag_varint
    | `float -> `fixed64
    | `bool -> `varint
    | _ -> `block


let gen_wire_type_name piqi_type wire_type =
  let wire_type =
    match wire_type with
      | Some x -> x
      | None ->
          get_default_wire_type piqi_type
  in
  match wire_type with
    | `varint -> "varint"
    | `zigzag_varint -> "zigzag_varint"
    | `fixed32 -> "fixed32"
    | `fixed64 -> "fixed64"
    | `signed_varint -> "signed_varint"
    | `signed_fixed32 -> "signed_fixed32"
    | `signed_fixed64 -> "signed_fixed64"
    | `block -> "block"


(* this is similar to unalias, but instead of returning resolved_type it returns
 * resolved protobuf wire type *)
let rec get_wire_type context typename =
  let import, parent_piqi, typedef = resolve_typename context typename in
  let parent_context = switch_context context parent_piqi in
  get_typedef_wire_type parent_context typedef

and get_typedef_wire_type context typedef =
  match typedef with
    | `alias {A.protobuf_wire_type = Some wire_type} ->
        (* NOTE: top-level aliases override protobuf_wire_type for lower-level
         * aliases *)
        Some wire_type
    | `alias {A.typename = Some typename} ->
        get_wire_type context typename
    | `alias {A.piqi_type = Some piqi_type} ->
        Some (get_default_wire_type piqi_type)
    | _ ->
        None


(* gen wire type width in bits if it is a fixed-sized type *)
let gen_wire_type_width wt =
  match wt with
    | `fixed32 | `signed_fixed32 -> "32"
    | `fixed64 | `signed_fixed64 -> "64"
    | _ -> ""


(* calculates and generates the width of a packed wire element in bits:
 * generated value can be 32, 64 or empty *)
let gen_elem_wire_width context typename is_packed =
  let open L in
  if not is_packed
  then ""
  else
    match get_wire_type context typename with
      | None -> ""
      | Some x ->
          gen_wire_type_width x


let gen_field_mode context f =
  let open F in
  match f.mode with
    | `required -> "required"
    | `optional when f.default <> None && (not f.ocaml_optional) ->
        "required" (* optional + default *)
    | `optional -> "optional"
    | `repeated ->
        let mode =
          if f.protobuf_packed
          then "packed_repeated"
          else "repeated"
        in
        if f.ocaml_array
        then
          let typename = some_of f.typename in  (* always defined for repeated fields *)
          let width = gen_elem_wire_width context typename f.protobuf_packed in
          mode ^ "_array" ^ width
        else
          mode


let gen_packed_prefix is_packed =
  ios (if is_packed then "packed_" else "")


(* generate: (packed_)?(list|array|array32|array64) *)
let gen_list_repr context l =
  let open L in
  let packed_prefix = gen_packed_prefix l.protobuf_packed in
  let repr =
    if l.ocaml_array
    then ios "array" ^^ ios (gen_elem_wire_width context l.typename l.protobuf_packed)
    else ios "list"
  in
  packed_prefix ^^ repr


let gen_cc s =
  if !flag_cc
  then ios s
  else iol []

