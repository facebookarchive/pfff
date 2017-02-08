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


module C = Piqi_common
open C

open Piqobj_common


(*
(* "unknown field" warnings will not be printed for the fields from this list *)
let ignored_fields = ref []


let add_ignored_field x =
  ignored_fields := x :: !ignored_fields


let is_ignored_field (ast :piq_ast) =
  match ast with
    | `name x | `named {T.Named.name = x} -> (* field or flag *)
        List.mem x !ignored_fields
    | _ -> false


let load_piq_ignore_field = function
  | `word x ->
      add_ignored_field x
  | x ->
      error x "invalid .piq-ignore entry"


let load_piq_ignore_node (l :piq_ast list) =
  try
    let ignore_node = List.find
      (function
        | `named {T.Named.name = "piq-ignore"; T.Named.value = `list l} ->
            add_ignored_field "piq-ignore"; (* add piq-ignore itself *)
            List.iter load_piq_ignore_field l;
            true
        | (`named {T.Named.name = "piq-ignore"} as x) ->
            error x "invalid .piq-ignore specification"
        | _ -> false) l
    in ignore (ignore_node)
  with
    Not_found -> ()


let load_piq_ignore (ast : piq_ast) =
  ignored_fields := []; (* reset ignored fields *)
  match ast with
    | `list l -> load_piq_ignore_node l
    | _ -> ()
*)

let unknown_fields = ref []

let add_unknown_field x =
  unknown_fields := x :: !unknown_fields


let get_unknown_fields () =
  let res = List.rev !unknown_fields in
  (* reset unkown field list state *)
  unknown_fields := [];
  res


(* ------------------------------------------------------------------- *)
(* ------------------------------------------------------------------- *)
(* ------------------------------------------------------------------- *)

let depth = ref 0

(* depth, description, object *)
exception Error of int * string * Obj.t


(* TODO: trace errors *)
let trace_error obj s =
  let loc = C.location obj in
  trace "piqobj_of_piq error: %s\n" (strerr loc s)


let error obj s =
  (*
  trace_error obj s;
  *)
  raise (Error (!depth, s, Obj.repr obj))


(* TODO, XXX: handle integer overflows *)
let rec parse_int (obj: piq_ast) =
  match obj with
    | `int (x, _) -> `int (Piqloc.addrefret obj x)
    | `uint (x, _) -> `uint (Piqloc.addrefret obj x)
    | o -> error o "int constant expected"


let uint64_to_float x =
  if Int64.compare x 0L < 0 (* big unsigned? *)
  then
    let s = Printf.sprintf "%Lu" x in
    float_of_string s
  else
    Int64.to_float x


let rec parse_float (obj: piq_ast) =
  match obj with
    | `int (x, _) -> Int64.to_float x
    | `uint (x, _) -> uint64_to_float x
    | `float (x, _) -> x
    | o -> error o "float constant expected"


let parse_bool (x :piq_ast) = match x with
  | `bool x -> x
  | o -> error o "boolean constant expected"


let parse_string ?piq_format (x :piq_ast) =
  let unicode_error s =
      error s "string contains non-unicode binary data"
  in
  let check_piq_format () =
    match piq_format, x with
      | Some `word, `word _ -> (); (* ok *)
      | Some `word, `text _ ->
          error x "word literal expected instead of verbatim text"
      | Some `word, _ (* various string literals *) ->
          warning x "word literal expected instead of quoted string"
      | None, `word _ ->
          error x "quoted string literal expected instead of word"
      | Some `text, `word _ ->
          error x "verbatim text or string literal expected instead of word"
      | _ -> ()
  in
  match x with
    | `word s when !Config.piq_relaxed_parsing -> s

    | `int (_, s)
    | `uint (_, s)
    | `float (_, s) when !Config.piq_relaxed_parsing -> s
    | `bool b when !Config.piq_relaxed_parsing ->
        let s = match b with true -> "true" | false -> "false" in
        Piqloc.addrefret x s

    | `ascii_string (s, _) | `utf8_string (s, _) | `text s | `word s ->
        check_piq_format ();
        s
    | `raw_binary s ->
        if Piq_lexer.is_utf8_string s
        then s
        else unicode_error s
    | `binary (s, _) -> unicode_error s
    | o -> error o "string expected"


let parse_binary (x :piq_ast) = match x with
  | `ascii_string (s, _) | `binary (s, _) | `raw_binary s -> s
  | `utf8_string (s, _) ->
      error s "binary contains unicode characters or code points"
  | o -> error o "binary expected"


(* some common errors *)
let error_exp_list obj =
  error obj "list expected"


let check_duplicate name tail =
  match tail with
    | [] -> ()
    | l ->
        if !Config.flag_strict
        then
          let obj = List.hd l in
          error obj ("duplicate field " ^ U.quote name)
        else
          List.iter (fun obj ->
            warning obj ("duplicate field " ^ U.quote name)) l


(* truncate the string till the first newline or to max_len *)
let truncate_string s max_len =
  let max_len =
    try String.index s '\n'
    with Not_found -> max_len
  in
  if String.length s <= max_len
  then s
  else
    let s = String.sub s 0 max_len in
    s ^ " ..."


let string_of_piqast x =
  match x with
    | `name s -> s
    | `named {Piq_ast.Named.name = n} -> n
    | _ ->
        let s = Piq_gen.to_string x ~nl:false in
        truncate_string s 50


let warn_unknown_field x =
  warning x ("unknown field: " ^ string_of_piqast x)


let handle_unknown_field (x: piq_ast) =
  if !Config.flag_strict
  then
    error x ("unknown field: " ^ string_of_piqast x)
  else
    if !C.is_inside_parse_piqi
    then add_unknown_field x
    else warn_unknown_field x


let handle_unknown_variant (x: piq_ast) =
  error x ("unknown variant: " ^ string_of_piqast x)


exception Unknown_variant


let find_piqtype name =
  try
    Piqi_db.find_piqtype name
  with Not_found ->
    Piqi_common.error name ("unknown type: " ^ U.quote name)


(* idtable implemented as map: string -> 'a *)
let rec parse_obj0
      ?(piq_format: T.piq_format option)
      ~try_mode
      ~nested_variant
      (t: T.piqtype) (x: piq_ast) :Piqobj.obj =
  (* fill the location DB *)
  let r f x = reference f x in
  let rr f t x = reference (f t) x in
  match t with
    (* built-in types *)
    | `int -> parse_int x
    | `float -> `float (r parse_float x)
    | `bool -> `bool (r parse_bool x)
    | `string -> `string (reference (parse_string ?piq_format) x)
    | `binary -> `binary (r parse_binary x)
    | `any -> `any (r parse_any x)
    (* custom types *)
    | `record t -> `record (rr parse_record t x)
    | `variant t -> `variant (rr (parse_variant ~try_mode ~nested:nested_variant) t x)
    | `enum t -> `enum (rr (parse_enum ~try_mode ~nested:nested_variant) t x)
    | `list t -> `list (rr parse_list t x)
    | `alias t -> `alias (reference (parse_alias t ?piq_format ~try_mode ~nested_variant) x)

and parse_obj ?(try_mode=false) ?(nested_variant=false) ?piq_format t x =
  reference (parse_obj0 ~try_mode ~nested_variant ?piq_format t) x


and parse_typed_obj ?piqtype x = 
  match piqtype, x with
    | None, `typed {Piq_ast.Typed.typename = n; value = ast} ->
        let t = find_piqtype n in
        parse_obj t ast
    | Some t, `typed {Piq_ast.Typed.value = ast} ->
        (* XXX: if both piqtype and `typed are defined, supplied type overrides
         * object type *)
        (* XXX: produce warning if they are not equal? *)
        parse_obj t ast
    | Some t, _ ->
        (* it is not a typed object, but we can use a supplied type *)
        parse_obj t x
    | _ -> error x "typed object expected"


and try_parse_obj f t x =
  (* unwind alias to obtain its real type *)
  match C.unalias t with
    | _ when f.T.Field.piq_positional = Some false ->
        (* this field must be always labeled according to the explicit
         * ".piq-positional false" setting *)
        None
    | `record _ | `list _ when f.T.Field.piq_positional <> Some true ->
        (* all records and lists should be labeled (i.e. can't be positional)
         * unless explicitly overridden in the piqi spec by the .piq-positional
         * setting *)
        None
    | `any when f.T.Field.name <> None ->
        (* NOTE, XXX: try-parsing of labeled `any always failes *)
        None
    (* NOTE, XXX: try-parsing of unlabeled `any always succeeds *)
    | _ ->
        let depth' = !depth in
        try Some (parse_obj t x ~try_mode:true ?piq_format:f.T.Field.piq_format)
        with
          (* ignore errors which occur at the same parse depth, i.e. when
           * parsing everything except for lists and records which increment
           * depth *)
          Error (depth'', _, _) when depth'' = depth' ->
            (depth := depth'; None) (* restore the original depth *)


and parse_any x :Piqobj.any =
  (* NOTE: the object is not fully resolved during this stage; at least
   * "obj" should be obtained by parsing "piqtype.ast" at later stages (see
   * Piqi.resolve_defaults for example *)
  match x with
    | `any ref ->
        (* in internal mode, returning the exact Piqobj.any object passed via a
         * reference *)
        C.debug "Piqobj_of_piq.parse_any: recovering any from existing ref %d\n" ref;
        let any = Piqobj.get_any ref in
        (* prevent adding a location reference; if we attempt to add a location
         * referene here, we end up with a circular reference *)
        Piqloc.pause_once ();
        any

    | `typed {Piq_ast.Typed.typename = typename; value = ast} ->
        let any = Any.({
          Piqobj.default_any with
          typename = Some typename;
          piq_ast = Some ast;
        })
        in
        Piqloc.addrefret ast any

    (* read untyped JSON form (json ...) as piqi-any *)
    | `form (`word "json", [`text s]) ->
        let json_ast = !Piqobj.json_of_string s in
        let any = Any.({
          Piqobj.default_any with
          json_ast = Some json_ast;
          json_string = Some s;
        })
        in
        Piqloc.addrefret s any
    | `form (`word "json", _) ->
        error x "verbatim text literal with JSON value expected after \"json\""

    (* read untyped XML form (xml ...) as piqi-any *)
    | `form (`word "xml", [`text s]) ->
        let xml_list = !Piqobj.xml_of_string s in
        let any = Any.({
          Piqobj.default_any with
          xml_ast = Some ("undefined", xml_list);
        })
        in
        Piqloc.addrefret s any
    | `form (`word "xml", _) ->
        error x "verbatim text literal with XML value expected after \"xml\""

    | ast ->
        let any = Any.({
          Piqobj.default_any with
          piq_ast = Some ast;
        })
        in
        Piqloc.addrefret ast any


and parse_record t x =
  match x with
    | `list l ->
        incr depth;
        (* NOTE: pass locating information as a separate parameter since empty
         * list is unboxed and doesn't provide correct location information *)
        let loc = x in
        let res = do_parse_record loc t l in
        decr depth;
        res
    | o -> error_exp_list o
 (*
  * 1. parse required fields first by label, type or (sub)type = anonymous
  * 2. parse the rest in the order they are listed in the original specification
  * 
  *
  *)

and do_parse_record loc t l =
  let required_spec, other_spec =
    List.partition is_required_field t.T.Record.field in
  (* parse required fields first *)
  let fields, rem =
    List.fold_left (parse_field loc) ([], l) (required_spec @ other_spec) in
  (* issue warnings on unparsed fields *)
  List.iter handle_unknown_field rem;
  let unparsed_piq_fields_ref =
    if rem <> [] && !C.is_inside_parse_piqi
    then Some (Piqi_objstore.put rem) (* FIXME: potential memory leak *)
    else None
  in
  (* put required fields back at the top *)
  R.({t = t; field = List.rev fields; unparsed_piq_fields_ref = unparsed_piq_fields_ref})


and is_required_field t = (t.T.Field.mode = `required)


and parse_field loc (accu, rem) t =
  let fields, rem =
    match t.T.Field.piqtype with
      | None -> do_parse_flag t rem
      | Some _ -> do_parse_field loc t rem
  in
  (List.rev_append fields accu, rem)


and do_parse_flag t l =
  let open T.Field in
  let name = name_of_field t in
  debug "do_parse_flag: %s\n" name;
  (* NOTE: flags can't be positional so we only have to look for them by name *)
  let res, rem = find_flags name t.piq_alias l in
  match res with
    | [] -> [], rem
    | x::tail ->
        check_duplicate name tail;
        match x with
          | `name _ | `named {Piq_ast.Named.value = `bool true} ->
              (* flag is considered to be present when it is represented either
               * as name w/o value or named boolean true value *)
              let res = F.({t = t; obj = None}) in
              Piqloc.addref x res;
              [res], rem
          | `named {Piq_ast.Named.value = `bool false} ->
              (* flag is considered missing/unset when its value is false *)
              [], rem
          | _ ->
              (* there are no other possible representations of flags *)
              assert false


and do_parse_field loc t l =
  let open T.Field in
  let name = name_of_field t in
  debug "do_parse_field: %s\n" name;
  let field_type = some_of t.piqtype in
  let values, rem =
    match t.mode with
      | `required -> 
          let x, rem = parse_required_field t loc name field_type l in
          [x], rem
      | `optional ->
          let x, rem = parse_optional_field t name field_type t.default l in
          let res = (match x with Some x -> [x] | None -> []) in
          res, rem
      | `repeated ->
          parse_repeated_field t name field_type l
  in
  let fields =
    List.map (fun x ->
      let res = F.({t = t; obj = Some x}) in
      Piqloc.addrefret x res) values
  in
  fields, rem
  

and parse_required_field f loc name field_type l =
  let res, rem = find_fields name f.T.Field.piq_alias field_type l in
  match res with
    | [] ->
        (* try finding the first field which is successfully parsed by
         * 'parse_obj' for a given field type *)
        begin
          let res, rem = find_first_parsed f field_type l in
          match res with
            | Some x -> x, rem
            | None -> error loc ("missing field " ^ U.quote name)
        end
    | x::tail ->
        check_duplicate name tail;
        let obj = parse_obj field_type x ?piq_format:f.T.Field.piq_format in
        obj, rem


and equals_name name alt_name x =
  if x = name
  then true
  else
    match alt_name with
      | Some name -> x = name
      | None -> false


(* find field by name, return found fields and remaining fields *)
and find_fields (name:string) (alt_name:string option) field_type (l:piq_ast list) :(piq_ast list * piq_ast list) =
  let equals_name = equals_name name alt_name in
  let rec aux accu rem = function
    | [] -> List.rev accu, List.rev rem
    | (`named n)::t when equals_name n.Piq_ast.Named.name -> aux (n.Piq_ast.Named.value::accu) rem t
    | (`name n)::t when equals_name n ->
        (match C.unalias field_type with
          | `bool ->
              (* allow omitting boolean constant for a boolean field by
               * interpreting the missing value as "true" *)
              let value = Piqloc.addrefret n (`bool true) in
              aux (value::accu) rem t
          | _ ->
              error n ("value must be specified for field " ^ U.quote n)
        )
    | h::t -> aux accu (h::rem) t
  in
  aux [] [] l


(* find flags by name, return found flags and remaining fields *)
and find_flags (name:string) (alt_name:string option) (l:piq_ast list) :(piq_ast list * piq_ast list) =
  let equals_name = equals_name name alt_name in
  let rec aux accu rem = function
    | [] -> List.rev accu, List.rev rem
    | ((`name n) as h)::t when equals_name n -> aux (h::accu) rem t
    | ((`named n) as h)::t when equals_name n.Piq_ast.Named.name ->
        (* allow specifying true or false as flag values: true will be
         * interpreted as flag presence, false is treated as if the flag was
         * missing *)
        (match n.Piq_ast.Named.value with
          | `bool _ ->
              aux (h::accu) rem t
          | _ ->
              error h ("only true and false can be used as values for flag " ^ U.quote n.Piq_ast.Named.name)
        )
    | h::t -> aux accu (h::rem) t
  in
  aux [] [] l


and find_first_parsed f field_type l =
  let rec aux rem = function
    | [] -> None, l
    | h::t ->
        match try_parse_obj f field_type h with
          | None -> aux (h::rem) t
          | x -> x, (List.rev rem) @ t
  in aux [] l


and parse_optional_field f name field_type default l =
  let res, rem = find_fields name f.T.Field.piq_alias field_type l in
  match res with
    | [] ->
        (* try finding the first field which is successfully parsed by
         * 'parse_obj for a given field_type' *)
        begin
          let res, rem = find_first_parsed f field_type l in
          match res with
            | Some _ ->
                res, rem
            | None ->
                let res = Piqobj_common.parse_default field_type default in
                res, l
        end
    | x::tail ->
        check_duplicate name tail;
        let obj = Some (parse_obj field_type x ?piq_format:f.T.Field.piq_format) in
        obj, rem


(* parse repeated variant field allowing variant names if field name is
 * unspecified *) 
and parse_repeated_field f name field_type l =
  let res, rem = find_fields name f.T.Field.piq_alias field_type l in
  match res with
    | [] -> 
        (* XXX: ignore errors occuring when unknown element is present in the
         * list allowing other fields to find their members among the list of
         * elements *)
        let accu, rem =
          (List.fold_left
            (fun (accu, rem) x ->
              match try_parse_obj f field_type x with
                | None -> accu, x::rem
                | Some x -> x::accu, rem) ([], []) l)
        in List.rev accu, List.rev rem
    | l ->
        (* use strict parsing *)
        let res = List.map (parse_obj field_type ?piq_format:f.T.Field.piq_format) res in
        res, rem


and parse_variant ~try_mode ~nested t x =
  debug "parse_variant: %s\n" (some_of t.T.Variant.name);
  let value = parse_options t.T.Variant.option x ~try_mode ~nested in
  V.({t = t; option = value})


and parse_options ~try_mode ~nested options x =
  match options with
    | [] ->
        if nested
        then raise Unknown_variant
        else handle_unknown_variant x
    | o::options ->
        match parse_option o x ~try_mode with
          | Some value ->  (* success *)
              Piqloc.addrefret x value
          | None ->  (* need to try other options *)
              (match parse_nested_option o x ~try_mode with
                | Some value ->
                    Piqloc.addrefret x value
                | None ->
                    (* continue with other options *)
                    parse_options options x ~try_mode ~nested
              )


and parse_nested_option ~try_mode o x =
  (* recursively descent into non-terminal (i.e. nameless variant and enum)
   * options
   *
   * NOTE: recurse into aliased nested variants as well *)
  let open T.Option in
  match o.name, o.piqtype with
    | None, Some t ->
        let is_nested_variant =
          match C.unalias t with
            | `variant v ->
                debug "parse_nested_variant: %s\n" (some_of v.T.Variant.name);
                true
            | `enum e ->
                debug "parse_nested_enum: %s\n" (some_of e.T.Enum.name);
                true
            | _ ->
                false
        in
        if is_nested_variant
        then
          try
            let obj = parse_obj t x ~try_mode ~nested_variant:true in
            Some O.({t = o; obj = Some obj})
          with Unknown_variant ->
            None
        else
          None
    | _ ->
        None


and parse_option ~try_mode o x =
  match x with
    | `name n ->
        parse_name_option o n
    | `named {Piq_ast.Named.name = n; value = x} ->
        parse_named_option o n x
    | _ ->
        parse_option_by_type o x ~try_mode


and parse_option_by_type ~try_mode o x =
  let open T.Option in
  match o.name, o.piqtype with
    | None, None ->
        assert false
    | Some n, None ->
        (* try parsing word as a name, but only when the label is exact, i.e.
         * try_mode = false
         *
         * by doing this, we allow using --foo bar instead of --foo.bar in
         * relaxed Piq parsing and getopt modes *)
        (match x with
          |`word s when equals_name n o.piq_alias s && !Config.piq_relaxed_parsing && not try_mode ->
              Some O.({t = o; obj = None})
          | _ ->
              None
        )
    | _, Some t ->
        let do_parse () =
          let obj = Some (parse_obj t x ?piq_format:o.piq_format) in
          Some O.({t = o; obj = obj})
        in
        match C.unalias t, x with
          | `bool, `bool _

          | `int, `int _
          | `int, `uint _

          | `float, `int _
          | `float, `uint _
          | `float, `float _

          | `record _, `list _
          | `list _  , `list _ -> do_parse ()

          | `string, `text _         when o.piq_format = Some `text -> do_parse ()
          | `string, `word _         when o.piq_format = Some `word -> do_parse ()

          (* XXX, TODO: do we need it?
          | `string, `ascii_string _ when o.piq_format = Some `string -> do_parse ()
          | `string, `utf8_string _  when o.piq_format = Some `string -> do_parse ()
          *)

          | `string, `ascii_string _
          | `string, `utf8_string _
          | `string, `raw_binary _
          | `string, `text _         when o.piq_format = None -> do_parse ()

          | `string, `int _
          | `string, `uint _
          | `string, `float _
          | `string, `bool _
          | `string, `word _         when o.piq_format = None && !Config.piq_relaxed_parsing -> do_parse ()

          | `binary, `ascii_string _
          | `binary, `binary _
          | `binary, `raw_binary _ -> do_parse ()
          | _ ->
              None



and parse_name_option o name =
  let open T.Option in
  let n = C.name_of_option o in
  if equals_name n o.piq_alias name
  then
    match o.name, o.piqtype with
      | Some _, Some _ ->
          error name ("value expected for option " ^ U.quote n)
      | _ ->
          Some O.({t = o; obj = None})
  else
    None


and parse_named_option o name x =
  let open T.Option in
  let n = C.name_of_option o in
  if equals_name n o.piq_alias name
  then
    match o.name, o.piqtype with
      | _, None ->
          error x ("value can not be specified for option " ^ U.quote n)
      | _, Some t ->
          let obj = Some (parse_obj t x ?piq_format:o.piq_format) in
          Some O.({t = o; obj = obj})
  else
    None


and parse_enum ~try_mode ~nested t x =
  debug "parse_enum: %s\n" (some_of t.T.Enum.name);
  let value = parse_options t.T.Enum.option x ~try_mode ~nested in
  E.({t = t; option = value})


and parse_list t = function
  | `list l ->
      incr depth;
      let res = do_parse_list t l in
      decr depth;
      res
  | o -> error_exp_list o


and do_parse_list t l =
  let obj_type = some_of t.T.Piqi_list.piqtype in
  let contents = List.map (parse_obj obj_type ?piq_format:t.T.Piqi_list.piq_format) l in
  L.({t = t; obj = contents})


(* XXX: roll-up multiple enclosed aliases into one? *)
and parse_alias ?(piq_format: T.piq_format option) ~try_mode ~nested_variant t x =
  (* upper-level setting overrides lower-level setting *)
  let this_piq_format = t.T.Alias.piq_format in
  let piq_format =
    if this_piq_format <> None
    then this_piq_format
    else piq_format
  in
  let piqtype = some_of t.T.Alias.piqtype in
  let obj = parse_obj piqtype x ?piq_format ~try_mode ~nested_variant in
  A.({t = t; obj = obj})


(* 
 * External interface:
 *      resolve parse_obj errors into common parse error format
 *)
let wrap f x =
  depth := 0; (* reset the parser's depth *)
  (*
  load_piq_ignore x; (* load ignored fields from the toplevel list *)
  *)
  try f x
  with Error (_depth, s, obj) ->
    (* print delayed warnings in case of error *) 
    List.iter warn_unknown_field (get_unknown_fields ());
    Piqi_common.error obj s


let parse_obj t x =
  wrap (parse_obj t) x


let parse_typed_obj ?piqtype x =
  wrap (parse_typed_obj ?piqtype) x


let _ =
  Piqobj.of_piq := parse_obj

