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


module Idtable = Piqi_db.Idtable
type idtable = T.typedef Idtable.t


(* start in boot_mode by default, it will be switched off later (see below) *)
let is_boot_mode = ref true


(* resolved type definition for the Piqi language;
 * it will be appropriately initialized during boot stage (see below) *)
let piqi_lang_def :T.piqtype ref = ref `bool
(* resolved type definition for the Piqi specification *)
let piqi_spec_def :T.piqtype ref = ref `bool

(* resolved "typedef" type definition
 * Will be appropriately initialized during boot stage (see below) *)
let typedef_def :T.piqtype ref = ref `bool
let field_def :T.piqtype ref = ref `bool
let option_def :T.piqtype ref = ref `bool
let function_def :T.piqtype ref = ref `bool
let import_def :T.piqtype ref = ref `bool


(* loaded in boot () -- see below *)
let piqi_spec :T.piqi option ref = ref None
let piqi_lang :T.piqi option ref = ref None

let is_boot_piqi piqi =
  match !piqi_spec with
    | Some x -> x == piqi
    | None -> false


(* processing hooks to be run at the end of Piqi module load & processing *)
let processing_hooks = ref []

let register_processing_hook (f :idtable -> T.piqi -> unit) =
  debug "register_processing_hook(0)\n";
  (* NOTE: create an empty idtable just to make invocations below work; none of
   * the plugins actually require a valid idtable to exist at this point, so we
   * don't care *)
  let idtable = Idtable.empty in

  (* run the hook on the embedded Piqi self-specification *)
  debug "register_processing_hook(1.5)\n";
  f idtable (some_of !piqi_lang);

  debug "register_processing_hook(1.6)\n";
  f idtable (some_of !piqi_spec);

  debug "register_processing_hook(1)\n";
  (* add the hook to the list of registered hooks *)
  processing_hooks := !processing_hooks @ [f]


let add_typedef idtable (typedef:T.typedef) = 
  let name = C.typedef_name typedef in
  debug "add_typedef: %s\n" name;
  if Idtable.mem idtable name
  then (
    let prev_def = Idtable.find idtable name in
    if C.is_builtin_def prev_def
    then (
      (* allowing to override a boot def *)
      C.warning typedef ("override of built-in type definition " ^ U.quote name);
      Idtable.add idtable name typedef
    )
    else
      C.error typedef
        ("duplicate type definition " ^ U.quote name ^ "\n" ^
         error_string prev_def "first defined here")
  )
  else
    Idtable.add idtable name typedef


let add_typedefs idtable defs =
  List.fold_left add_typedef idtable defs


let add_imported_typedef idtable (typedef:T.typedef) =
  let open Import in
  let import = 
    match get_parent typedef with
      | `import x -> x
      | _ -> assert false
  in
  (* while adding imported defs to the idtable, transform definition's names to
   * contain module's namespace *)
  let name = some_of import.name ^ "/" ^ C.typedef_name typedef in
  debug "add_imported_typedef: %s\n" name;
  Idtable.add idtable name typedef


let add_imported_typedefs idtable defs =
  List.fold_left add_imported_typedef idtable defs


let find_def idtable name =
  try Idtable.find idtable name
  with Not_found ->
    error name ("unknown type " ^ U.quote name)


let is_func_param def =
  match def with
    | `record x -> x.R.is_func_param
    | `variant x -> x.V.is_func_param
    | `enum x -> x.E.is_func_param
    | `alias x -> x.A.is_func_param
    | `list x -> x.L.is_func_param


(* mark type definition as a function parameter *)
let set_is_func_param_flag def =
  match def with
    | `record x -> x.R.is_func_param <- true
    | `variant x -> x.V.is_func_param <- true
    | `enum x -> x.E.is_func_param <- true
    | `alias x -> x.A.is_func_param <- true
    | `list x -> x.L.is_func_param <- true


let resolve_typename map name :T.piqtype =
    let def = find_def map name in
    (def :> T.piqtype)


(* XXX: is there a way to avoid code duplicaton here? *)
let resolve_field_typename map f =
  let open F in
  match f.typename with
    | None -> () (* flag *)
    | Some name ->
        f.piqtype <- Some (resolve_typename map name)


let resolve_option_typename map o =
  let open O in
  match o.typename with
    | None -> ()
    | Some name ->
        o.piqtype <- Some (resolve_typename map name)


let resolve_def_typenames map = function
  | `record r ->
      List.iter (resolve_field_typename map) r.R.field
  | `variant v ->
      List.iter (resolve_option_typename map) v.V.option
  | `alias a ->
      let piqtype =
        match a.A.typename with
          | Some name ->
              resolve_typename map name
          | None ->
              (* either one or another must be defined -- this has been checked
               * earlier in check_alias *)
              let piqi_type = some_of a.A.piqi_type in
              (piqi_type :> T.piqtype)
      in
      a.A.piqtype <- Some piqtype
  | `list l ->
      l.L.piqtype <- Some (resolve_typename map l.L.typename)
  | `enum _ ->
      ()


let check_name x =
  if not (Piqi_name.is_valid_name x)
  then error x ("invalid name: " ^ U.quote x)
  else ()


let check_opt_name = function
  | None -> ()
  | Some x -> check_name x


let check_dup_names what names =
  match U.find_dups names with
    | None -> ()
    | Some (name, prev) ->
        error name
          ("duplicate " ^ what ^ " name " ^ U.quote name ^ "\n" ^
            error_string prev "first defined here")


let check_field f =
  let open Field in
  begin
    begin
    check_opt_name f.name;
    match f.name, f.typename with
      | None, None ->
          error f "name or type must be specified for a field"
      | Some _, None -> (* flag *)
          begin
            (if f.mode <> `optional
            then error f "flags must be optional");

            (if f.default <> None
            then error f "flags may not specify default");
          end
      | _ ->
          ()
    end;

    if f.default <> None && f.mode <> `optional
    then
      error f.default "default values may only be specified for optional fields"
  end


let check_def_name obj = function
  | Some x -> check_name x
  | None ->
      error obj "missing field \"name\""


let check_record r =
  check_def_name r r.R.name;
  let fields = r.R.field in
  (* XXX: protoc doesn't print any warnings on records with no fields *)
  List.iter check_field fields


let check_option o =
  let open Option in
  begin
    check_opt_name o.name;
    match o.name, o.typename with
      | None, None ->
          error o "name or type must be specified for an option"
      | _ -> ()
  end


let check_variant v =
  check_def_name v v.V.name;
  let name = some_of v.V.name in
  let options = v.V.option in
  if options = []
  then error v ("variant " ^ U.quote name ^ " doesn't specify any options");
  List.iter check_option options


let check_enum_option x =
  let open O in
  begin
    (* consts name must be defined and types must not *)
    if x.name = None
    then error x ("enum options must specify name");

    if x.typename <> None
    then error x ("enum options must not specify type");

    check_opt_name x.name;
  end


let check_enum e =
  check_def_name e e.E.name;
  let name = some_of e.E.name in
  let options = e.E.option in
  if options = []
  then error e ("enum " ^ U.quote name ^ " doesn't specify any options");
  List.iter check_enum_option options


let check_alias a =
  let open A in
  begin
    check_def_name a a.name;
    (* TODO: this check is incomplete *)
    let name = some_of a.name in
    if a.typename = None && a.piqi_type = None
    then
      error a ("alias " ^ U.quote name ^ " must specify either piqi-type or type");
  end


let check_list l =
  let open L in
  begin
    check_def_name l l.name;
  end


let check_def def =
  match def with
    | `record x -> check_record x
    | `variant x -> check_variant x
    | `enum x -> check_enum x
    | `alias x -> check_alias x
    | `list x -> check_list x


let check_resolved_alias a =
  let open A in
  begin
    (match some_of a.piqtype with
      | `alias b ->
          if a.piqi_type <> None && a.piqi_type <> b.piqi_type
          then error a "piqi-type doesn't match piqi-type in the redefined alias"
      | #T.piqi_type -> () (* piqtype was derived from piqi_type *)
      | _ when a.piqi_type <> None -> (* other definitions *)
          error a "piqi-type can not be specified for an non-alias"
      | _ -> ()
    );
  end


let check_resolved_def def =
  match def with
    | `record x ->
        let names = List.map (fun x -> name_of_field x) x.R.field in
        check_dup_names "field" names
    | `variant x ->
        let names = List.map (fun x -> name_of_option x) x.V.option in
        (* TODO: also check duplicate names among nested variants (i.e.
         * "non-terminal" nameless sub-variants) *)
        check_dup_names "option" names
    | `enum x ->
        let names = List.map (fun x -> name_of_option x) x.E.option in
        check_dup_names "enum option" names
    | `alias x ->
        check_resolved_alias x
    | _ -> ()


(* check that there are no infinite types among the definitions *)
let check_no_infinite_types ~parent defs =
  (* DFS graph traversal *)
  let black = ref [] in (* visited nodes, i.e. after last_visit *)
  let grey = ref [] in (* nodes between first_visit and last_visit *)
  let set_color node = function
    | `grey ->
        grey := node::!grey
    | color ->  (* white or black *)
        (* first, reset grey *)
        (match !grey with
          | h::t when h == node ->
              grey := t
          | _ ->
              assert false
        );
        if color = `black
        then
          black := node::!black
  in
  let get_color node =
    if List.memq node !black
    then `black
    else if List.memq node !grey
    then `grey
    else `white
  in
  let is_local_type piqtype =
    match piqtype with
      | (#T.typedef as x) ->
          C.get_parent x == parent
      | _ ->  (* considering built-in types local -- they can't be infinite *)
          true
  in
  (* we have to backtrack in order to correctly check variant type infinity, so
   * we report the infinite type error at the very end *)
  let infinite_type = ref None in

  let rec finite_path_exists piqtype =
    let finite = true in
    let infinite err =
      infinite_type := Some (piqtype, err);
      false
    in
    match get_color piqtype with
      | `black ->  (* already processed, which means the type is finite *)
          true
      | `grey ->  (* found a cycle => the type is infinite *)
          let err =
            match piqtype with
              | `alias x ->
                  (* NOTE: there is a reason why we are handling it here -- see
                   * below *)
                  "alias " ^ U.quote (some_of x.A.name) ^ " forms a loop"
              | _ ->
                  (* the path is infinite, but the error (if any) will be
                   * reported elsewhere *)
                  ""
          in
          infinite err
      | `white ->  (* unseen node *)
          set_color piqtype `grey;  (* mark the node as being processed *)
          let res =
            if not (is_local_type piqtype)
            then
              (* no need to check imported types as they've been checked already *)
              finite
            else
              match piqtype with
                | `record x ->
                    let rec check_fields = function
                      | [] ->
                          finite  (* all fields are finite *)
                      | f::tail ->
                          (match f.F.piqtype with
                            | Some piqtype when f.F.mode = `required ->
                                (* NOTE: it is OK for optional and repeated
                                 * field to form a loop; for instance, variant
                                 * -> record expansion can produce such options
                                 * naturally; similarly, some .proto files (e.g.
                                 * descriptor.proto) have repeated fields
                                 * forming a loop *)
                                if not (finite_path_exists piqtype)
                                then
                                  infinite (
                                    "record " ^ U.quote (some_of x.R.name) ^
                                    " is an infinite type (field " ^ U.quote (name_of_field f) ^ " forms a loop)"
                                  )
                                else
                                  check_fields tail
                            | _ ->
                                check_fields tail
                          )
                    in
                    check_fields x.R.field
                | `variant x ->
                    (* there's a finite path for a variant if there's a finite path
                     * for at least one of its options (option with no type means
                     * finite type by definition) *)
                    let options = x.V.option in
                    let is_variant_finite =
                      if List.exists (fun x -> x.O.piqtype = None) options
                      then true
                      else List.exists (fun x -> finite_path_exists (some_of x.O.piqtype)) options
                    in
                    if not is_variant_finite
                    then infinite ("variant " ^ U.quote (some_of x.V.name) ^ " is an infinite type (each option forms a loop)")
                    else finite
                | `list x ->
                    if not (finite_path_exists (some_of x.L.piqtype))
                    then infinite ("list " ^ U.quote (some_of x.L.name) ^ " forms a loop")
                    else finite
                | `alias x ->
                    (* NOTE: not reporting an alias loop here, so that it
                     * doesn't take precedence over loop reports for records,
                     * variants and lists when there's an alias in the middle *)
                    finite_path_exists (some_of x.A.piqtype)
                | _ ->  (* enum and primitive types are finite *)
                    finite
          in
          (* mark the node as processed if the path is finite or unprocessed is
           * the type is infinite as we are backtracking *)
          if res
          then set_color piqtype `black
          else set_color piqtype `white;
          res
  in
  let check_typedef x =
    if not (finite_path_exists (x :> T.piqtype))
    then
      (* reporting infinite type error *)
      let piqtype, err = some_of !infinite_type in
      error piqtype err
  in
  (* process non-variants first, because they may have tighter loops that would
   * otherwise be reported as variant loops *)
  let variants, non_variants =
    List.partition (function `variant _ -> true | _ -> false) defs
  in
  List.iter check_typedef (non_variants @ variants)


(* scoped extensions names should have exactly two sections separated by '.' *)
let is_valid_scoped_extension_name name =
  if not (Piqi_name.is_valid_name name ~allow:".")
  then false
  else
    match U.string_split name '.' with
      | [a; b] -> Piqi_name.is_valid_name a && Piqi_name.is_valid_name b
      | _ -> false


let check_extension_name = function
  | `name name | `typedef name | `import name | `func name ->
      if not (Piqi_name.is_valid_name name)
      then error name "invalid extension name"

  | `field name | `option name ->
      if not (is_valid_scoped_extension_name name)
      then error name "invalid scoped extension name"


let check_extension_spec spec =
  check_extension_name spec;
  match spec with
    | `name name ->
        C.warning name "use of .name for extending typedefs is deprecated; use .typedef instead"
    | _ -> ()


let check_extension x =
  let open Extend in
  begin
    if x.what = [] && x.piqi_with = []
    then error x ("extension doesn't specify any names");

    if x.quote = [] && x.piqi_with = []
    then error x ("extension doesn't specify any extensions");

    if x.quote <> []
    then C.warning (List.hd x.quote) "this style of extensions is deprecated; always use .with prefix";

    List.iter check_extension_spec x.what
  end


let debug_loc prefix =
  debug "%s out count = %d, in count = %d\n" prefix !Piqloc.ocount !Piqloc.icount


let assert_loc () =
  if (!Piqloc.ocount <> !Piqloc.icount)
  then
    let s = Printf.sprintf "internal_error: out count = %d, in count = %d\n" !Piqloc.ocount !Piqloc.icount in
    (*
    failwith s
    *)
    piqi_warning s


let add_fake_loc (obj :Piqobj.obj) =
  Piqloc.do_add_fake_loc obj ~label:"_self_piqi_default";
  match obj with
    | `enum x ->
        Piqloc.do_add_fake_loc x ~label:"_self_piqi_default_enum"
    | _ ->
        ()


let resolve_field_default x =
  let open F in
  match x.default, x.piqtype with
    | None, _ -> () (* no default *)
    | Some piqi_any, Some piqtype ->
        assert_loc ();
        debug_loc "resolve_default_value(0)";

        let any = Piqobj.any_of_piqi_any piqi_any in

        debug "resolve_field_default: %s\n" (C.name_of_field x);
        Piqobj.resolve_obj any ~piqtype;

        (* make sure we add fake locations for the default values of the
         * embedded self-specifications; currently, there's only one such value
         * which is field.mode = required *)
        if !is_boot_mode
        then add_fake_loc (some_of any.Piqobj.Any.obj);

        (* NOTE: fixing (preserving) location counters which get skewed during
         * parsing defaults *)
        Piqloc.icount := !Piqloc.ocount;
        debug_loc "resolve_default_value(1)";
        ()
    | _ ->
        assert false


let resolve_defaults = function
  | `record x ->
      List.iter resolve_field_default x.R.field
  | _ -> ()


let copy_obj (x:'a) :'a =
  Obj.obj (Obj.dup (Obj.repr x))
let copy_obj x = reference copy_obj x


let copy_obj_list l = List.map copy_obj l


let copy_variant ?(copy_parts=true) x =
  if copy_parts
  then Piqloc.addrefret x V.({x with option = copy_obj_list x.option})
  else copy_obj x


let copy_enum ?(copy_parts=true) x =
  if copy_parts
  then Piqloc.addrefret x E.({x with option = copy_obj_list x.option})
  else copy_obj x


let copy_record ?(copy_parts=true) x =
  if copy_parts
  then Piqloc.addrefret x R.({x with field = copy_obj_list x.field})
  else copy_obj x


let copy_def ~copy_parts (x:T.typedef) =
  let res =
    match x with
      | `record x -> `record (copy_record ~copy_parts x)
      | `variant x -> `variant (copy_variant ~copy_parts x)
      | `enum x -> `enum (copy_enum ~copy_parts x)
      | `alias x -> `alias (copy_obj x)
      | `list x -> `list (copy_obj x)
  in
  (* preserve location information *)
  Piqloc.addrefret x res


let copy_defs ?(copy_parts=true) defs = List.map (copy_def ~copy_parts) defs


let copy_imports l = List.map copy_obj l


let resolve_defs ~piqi idtable (defs:T.typedef list) =
  (*
  (* a fresh copy of defs is needed, since we can't alter the original ones:
   * we need to resolve types & assign codes in order to resolve_defaults *)
  *)

  (* check definitions validity *)
  List.iter check_def defs;

  (* add definitions to the map: def name -> def *)
  let idtable = add_typedefs idtable defs in

  (* resolve type names using the map *)
  List.iter (resolve_def_typenames idtable) defs;

  (* set up parent namespace to local piqi defs *)
  let parent = `piqi piqi in
  List.iter (fun def -> set_parent def parent) defs;

  (* check records, variants, enums for duplicate field/option names; check wire
   * types in aliases *)
  List.iter check_resolved_def defs;

  (* check that there are no infinite types among the definitions *)
  check_no_infinite_types defs ~parent;

  (* assign wire codes, if they are unassigned; check otherwise; check
   * correctness of .wire-packed usage *)
  Piqi_protobuf.process_typedefs defs;
  (* run some checks and expansions specific to the Piq format *)
  Piq.process_typedefs defs;

  (* return updated idtable *)
  idtable


let check_defs ~piqi idtable defs =
  ignore (resolve_defs idtable (copy_defs defs) ~piqi)


let read_piqi_common fname piq_parser :piq_ast =
  (* don't expand abbreviations until we construct the containing object *)
  let res = Piq_parser.read_all piq_parser ~expand_abbr:false in

  if res = []
  then piqi_warning ("piqi file is empty: " ^ fname);

  (* wrapping items in list to make them contents of "piqi" record *)
  let res = `list res in
  let startloc = (fname, 1, 1) in (* start location *)
  let ast = Piqloc.addlocret startloc res in
  (* now expand abbreviations *)
  Piq_parser.expand ast


let read_piqi_channel fname ch :piq_ast =
  (* XXX: handle read errors *)
  let piq_parser = Piq_parser.init_from_channel fname ch in
  read_piqi_common fname piq_parser


let read_piqi_string fname content :piq_ast =
  let piq_parser = Piq_parser.init_from_string fname content in
  read_piqi_common fname piq_parser


let open_piqi fname =
  try Pervasives.open_in_bin fname
  with Sys_error s ->
    piqi_error ("error opening piqi file: " ^ s)


let read_piqi_file fname :piq_ast =
  let ch = open_piqi fname in
  let res =
    try read_piqi_channel fname ch
    with x -> (* try ... after *)
      Pervasives.close_in ch;
      raise x
  in
  Pervasives.close_in ch;
  res


let check_modname x =
  if Piqi_name.is_valid_modname x
  then ()
  else error x ("invalid piqi module name: " ^ x)


let check_assign_module_name ?modname fname (piqi:T.piqi) =
  let open P in
  match piqi.modname, modname with
    | Some x, Some x' ->
        check_modname x;
        (* check that the requested module name corresponds to the module name
         * defined in the file *)
        if x <> x'
        then
          error piqi
            ("module loaded as " ^ U.quote x' ^
             " has different name " ^ U.quote x)
        else ()
    | Some x, None -> (* name is already defined for the module *)
        check_modname x
    | None, Some x ->
        piqi.modname <- modname
    | None, None ->
        (* basename + chop .piqi and .proto.piqi extensions *)
        let basename = Piqi_file.basename fname in
        if Piqi_name.is_valid_modname basename
        then piqi.modname <- Some basename
        else error piqi "piqi module name can not be derived from the file name"


let assign_import_name x =
  let open Import in
  match x.name with
    | Some x -> (* import name is already defined *)
        check_name x
    | None ->
        (* derive import name from the original module's name *)
        let name = Piqi_name.get_local_name x.modname in
        x.name <- Some name


let name_of_import x =
  let open Import in
  match x.name with
    | Some x -> x (* import name is already defined *)
    | None ->
        (* derive import name from the original module's name *)
        Piqi_name.get_local_name x.modname


let mlobj_to_piqobj piqtype wire_generator mlobj =
  debug_loc "mlobj_to_piqobj(0)";
  assert_loc ();
  let binobj = Piqirun.gen_binobj wire_generator mlobj in
  debug_loc "mlobj_to_piqobj(1.5)";

  (* dont' resolve defaults when reading wire *)
  let piqobj =
    U.with_bool C.is_inside_parse_piqi true
    (fun () ->
      C.with_resolve_defaults false (fun () -> Piqobj_of_protobuf.parse_binobj piqtype binobj)
    )
  in
  debug_loc "mlobj_to_piqobj(1)";
  assert_loc ();

  piqobj


let mlobj_to_ast piqtype wire_generator mlobj =
  debug_loc "mlobj_to_ast(0)";
  let piqobj = mlobj_to_piqobj piqtype wire_generator mlobj in
  debug_loc "mlobj_to_ast(1.5)";
  let ast = Piqobj_to_piq.gen_obj piqobj in
  debug_loc "mlobj_to_ast(1)";
  assert_loc ();
  ast


let mlobj_of_piqobj wire_parser piqobj =
  let binobj = Piqobj_to_protobuf.gen_binobj piqobj in
  let mlobj = Piqirun.parse_binobj wire_parser binobj in
  mlobj


let mlobj_of_ast piqtype wire_parser ast =
  debug_loc "mlobj_of_ast(0)";

  (*
  (* initialize starting location code *)
  let max_count = max !T.icount !T.ocount in
  T.icount := max_count;
  T.ocount := max_count;
  *)

  (* We have to resolve defaults while reading piqi in order to provide correct
   * location bindings. It is not possible to "fix" skewed location bindings
   * in piqtype.ml after default values get parsed. We rather decided to fix
   * location bindings here -- see resolve_defaults function for details *)
  let piqobj =
    U.with_bool C.is_inside_parse_piqi true
    (fun () ->
      C.with_resolve_defaults true (fun () -> Piqobj_of_piq.parse_obj piqtype ast)
    )
  in
  debug_loc "mlobj_of_ast(1.5)";

  let mlobj = mlobj_of_piqobj wire_parser piqobj in
  debug_loc "mlobj_of_ast(1)";
  assert_loc ();

  mlobj


let parse_piqi ast =
  (* XXX: handle errors *)
  debug "parse_piqi(0)\n";
  (* use prepared static "piqi" definition to parse the ast *)
  let res = mlobj_of_ast !piqi_lang_def T.parse_piqi ast in
  debug "parse_piqi(1)\n";
  res


let is_unknown_field custom_fields x =
  match x with
    | `named {Piq_ast.Named.name = name} | `name name ->
        if List.mem name custom_fields
        then false (* field is a custom field, i.e. "known" *)
        else true
    | _ -> true


let check_unknown_fields ?prepend unknown_fields custom_fields =
  let unknown_fields =
    List.filter (is_unknown_field custom_fields) unknown_fields
  in

  let warn x =
    (* call the function for printing prepending warning message *)
    (match prepend with
      | Some f -> f ()
      | None -> ());
    Piqobj_of_piq.warn_unknown_field x
  in

  (* print warnings *)
  List.iter warn unknown_fields


let parse_scoped_name name =
  match U.string_split name '.' with
    | [def_name; nested_name] -> def_name, nested_name
    | _ -> assert false (* this has been checked already *)


(* replace the first list element for which [f] returns true with [x]
 *
 * NOTE: non-tail recursive
 *)
let list_replace l f x =
  let rec aux = function
    | [] ->
        (* we were supposed to replace an item before we reached the end of the
         * list *)
        assert false
    | h::t ->
        if f h
        then x::t
        else h::(aux t)
  in
  aux l


let name_of_function x = x.T.Func.name


let idtable_of_defs defs =
  List.fold_left
    (fun t x -> Idtable.add t (C.typedef_name x) x)
    Idtable.empty defs


let idtable_of_imports imports =
  List.fold_left
    (fun t x -> Idtable.add t (name_of_import x) x)
    Idtable.empty imports


let idtable_of_functions funcs =
  List.fold_left
    (fun t x -> Idtable.add t (name_of_function x) x)
    Idtable.empty funcs


(* convert the map of extended elements back to list; while doing this, preserve
 * the original order *)
let list_of_idtable idtable l name_of_elem =
  List.map (fun x -> Idtable.find idtable (name_of_elem x)) l


(* find record field by name *)
let find_field r field_name scoped_name =
  try
    List.find (fun x -> name_of_field x = field_name) r.R.field
  with Not_found ->
    error scoped_name ("record doesn't have field named " ^ U.quote field_name)


(* find variant option by name *)
let find_option v option_name scoped_name =
  try
    List.find (fun x -> name_of_option x = option_name) v.V.option
  with Not_found ->
    error scoped_name ("variant doesn't have option named " ^ U.quote option_name)


(* replace record field with the new one *)
let replace_field r f field_name =
  let fields = r.R.field in
  let new_fields = list_replace fields (fun x -> name_of_field x = field_name) f in
  Piqloc.addref fields new_fields;
  let new_record = R.({r with field = new_fields}) in
  Piqloc.addref r new_record;
  new_record


(* replace variant option with the new one *)
let replace_option v o option_name =
  let options = v.V.option in
  let new_options = list_replace options (fun x -> name_of_option x = option_name) o in
  Piqloc.addref options new_options;
  let new_variant = V.({v with option = new_options}) in
  Piqloc.addref v new_variant;
  new_variant


let apply_extensions obj obj_def obj_parse_f obj_gen_f extension_entries custom_fields ~override =
  let trace' = !Piqloc.trace in
  (* Piqloc.trace := false; *)
  debug "apply_extensions(0)\n";
  let obj_ast = mlobj_to_ast obj_def obj_gen_f obj in
  let extension_asts = List.map Piqobj.piq_of_piqi_any extension_entries in

  let override l =
    if not override
    then l
    else
      let extension_labels =
        U.flatmap (function
          | `named x -> [x.Piq_ast.Named.name]
          | `name name -> [name]
          | _ -> []
        ) extension_asts
      in
      (*
      List.iter (Printf.eprintf "extesion label: %s\n") extension_labels;
      *)
      let is_overridden = function
          | `named x ->
              let res = List.mem x.Piq_ast.Named.name extension_labels in
              (*
              if res then Printf.eprintf "overridden: %s\n" x.T.Named.name;
              *)
              res
          | `name name ->
              let res = List.mem name extension_labels in
              (*
              if res then Printf.eprintf "overridden: %s\n" name;
              *)
              res
          | _ -> false
      in
      List.filter (fun x -> not (is_overridden x)) l
  in

  let extended_obj_ast =
    match obj_ast with
      | `named ({Piq_ast.Named.value = ((`list l) as _ref)} as x) -> (* typedefs -- named containers *)
          let v = `list (override l @ extension_asts) in

          let v = Piq_parser.piq_addrefret _ref v in

          let res = `named {x with Piq_ast.Named.value = v} in

          ignore (Piq_parser.piq_addrefret x res);

          res

      | (`list l) as _ref -> (* fields and options -- plain containers *)
          let v = `list (override l @ extension_asts) in
          Piq_parser.piq_addrefret _ref v

      | _ ->
          (* extensions can only be applied to named containers and all of
           * typedefs are named containers *)
          assert false
  in
  let context_str = "while applying extensions to this element ..." in
  debug "apply_extensions(1)\n";
  let extended_obj =
    try
      mlobj_of_ast obj_def obj_parse_f extended_obj_ast
    with (C.Error _) as e ->
      (* TODO, XXX: one error line is printed now, another (original) error
       * later -- it is inconsistent *)
      (
        prerr_endline (C.error_string obj context_str);
        (* re-raise the original exception after printing some context info *)
        raise e
      )
  in
  debug "apply_extensions(2)\n";
  Piqloc.trace := trace';

  (* get unparsed extension fields fields *)
  let unknown_fields = Piqobj_of_piq.get_unknown_fields () in
  check_unknown_fields unknown_fields custom_fields
    ~prepend:(fun () -> C.warning obj context_str);

  extended_obj


let apply_option_extensions idtable scoped_name extension_entries custom_fields ~override =
  let def_name, option_name = parse_scoped_name scoped_name in
  match find_def idtable def_name with
    | `variant v ->
        let option = find_option v option_name scoped_name in
        let extended_option =
          apply_extensions option !option_def T.parse_option T.gen__option
          extension_entries custom_fields ~override
        in
        let extended_variant = replace_option v extended_option option_name in
        let extended_typedef = `variant extended_variant in
        Piqloc.addref extended_variant extended_typedef;
        (* replace the original variant with the extended one *)
        Idtable.add idtable def_name extended_typedef
    | _ ->
        error scoped_name
          ("can't apply option extensions no non-variant definition " ^ U.quote def_name)


let apply_field_extensions idtable scoped_name extension_entries custom_fields ~override =
  let def_name, field_name = parse_scoped_name scoped_name in
  match find_def idtable def_name with
    | `record r ->
        let field = find_field r field_name scoped_name in
        let extended_field =
          apply_extensions field !field_def T.parse_field T.gen__field
          extension_entries custom_fields ~override
        in
        let extended_record = replace_field r extended_field field_name in
        let extended_typedef = `record extended_record in
        Piqloc.addref extended_record extended_typedef;
        (* replace the original record with the extended one *)
        Idtable.add idtable def_name extended_typedef
    | _ ->
        error scoped_name
          ("can't apply field extensions no non-record definition " ^ U.quote def_name)


let extend_import idtable name extension_entries custom_fields ~override =
  let import = 
    try Idtable.find idtable name
    with Not_found -> error name ("unknown import " ^ U.quote name)
  in
  let extended_import =
    apply_extensions import !import_def T.parse_import T.gen__import
    extension_entries custom_fields ~override
  in
  (* replace the original import with the extended one *)
  Idtable.add idtable name extended_import


let extend_imports imports extensions custom_fields =
  (* apply extensions to each import *)
  let idtable = idtable_of_imports imports in
  let idtable =
    List.fold_left
      (fun idtable (spec, override, extension_items) ->
        let name =
          match spec with
            | `import name -> name
            | _ -> assert false (* typedef and function extensions are already filtered out *)
        in
        extend_import idtable name extension_items custom_fields ~override
      )
      idtable
      extensions
  in
  (* convert the updated idtable to the list of resulting imports *)
  list_of_idtable idtable imports name_of_import


let extend_function idtable name extension_entries custom_fields ~override =
  let func =
    try Idtable.find idtable name
    with Not_found -> error name ("unknown function " ^ U.quote name)
  in
  let extended_func =
    apply_extensions func !function_def T.parse_func T.gen__func
    extension_entries custom_fields ~override
  in
  (* replace the original function with the extended one *)
  Idtable.add idtable name extended_func


let extend_functions funs extensions custom_fields =
  (* apply extensions to each function *)
  let idtable = idtable_of_functions funs in
  let idtable =
    List.fold_left
      (fun idtable (spec, override, extension_items) ->
        let name =
          match spec with
            | `func name -> name
            | _ -> assert false (* typedef and import extensions are already filtered out *)
        in
        extend_function idtable name extension_items custom_fields ~override
      )
      idtable
      extensions
  in
  (* convert the updated idtable to the list of resulting functions *)
  list_of_idtable idtable funs name_of_function


(* apply field and option extensions *)
let apply_def_extensions idtable spec extension_entries custom_fields ~override =
  match spec with
    | `name name | `typedef name ->
        let typedef = find_def idtable name in
        let extended_typedef =
          apply_extensions typedef !typedef_def T.parse_typedef T.gen__typedef
          extension_entries custom_fields ~override
        in
        (* replace the original typedef with the extended one *)
        Idtable.add idtable name extended_typedef
    | `field x ->
        apply_field_extensions idtable x extension_entries custom_fields ~override
    | `option x ->
        apply_option_extensions idtable x extension_entries custom_fields ~override
    | _ ->
        assert false (* import and function extensions are already filtered out *)


(* apply extensions to type defininitions *)
let apply_defs_extensions defs extensions custom_fields =
  (* defs extensions must be applied before field and option extensions;
   * therefore partition partition extensions into typedef, and the other types
   * of extensions; and then append them back together *)
  let defs_extensions, other_extensions =
    List.partition (fun (spec, _, _) ->
      match spec with
        | `name _ | `typedef _ -> true (* these are the same, but `name is depreceated *)
        | _ -> false
    ) extensions
  in
  let extensions = defs_extensions @ other_extensions in

  (* create a new idtable from the list of definitions *)
  let idtable = idtable_of_defs defs in

  (* iterate through extensions and apply them to correspondent definitions *)
  let idtable =
    List.fold_left
      (fun idtable (spec, override, extension_items) ->
        apply_def_extensions idtable spec extension_items custom_fields ~override
      )
      idtable extensions
  in
  (* convert the updated idtable to the list of resulting defs *)
  list_of_idtable idtable defs C.typedef_name


(* partition extensions into typedef extesions, function extensions and import
 * extensions *)
let partition_extensions extensions =
  let open Extend in
  (* get a list of (what, [extension]) pairs from all extensions *)
  let l = U.flatmap
    (fun x -> List.map (fun what -> what, x.override, (x.piqi_with @ x.quote)) x.what)
    extensions
  in
  let d, f, i =
    List.fold_left
      (fun (d, f, i) ((spec, _, _) as x) ->
        match spec with
          | `func _ -> (d, x::f, i)
          | `import _ -> (d, f, x::i)
          | `typedef _ | `name _ | `field _ | `option _ -> (x::d, f, i))
      ([], [], []) l
  in
  (List.rev d, List.rev f, List.rev i)


let get_imported_defs imports =
  let aux x = 
    let piqi = some_of x.Import.piqi in
    (* in order to avoid conflict between local defs and also defs imported
     * several times, creating a shallow copy of imported defs just to be able
     * to safely mutate the "parent" field *)
    let imported_defs = copy_defs piqi.P.resolved_typedef ~copy_parts:false in
    (* set parent namespace for imported definitions *)
    List.iter (fun def -> set_parent def (`import x)) imported_defs;
    imported_defs
  in
  U.flatmap aux imports


let make_param_name func param_name =
  (* construct the type name as a concatentation of the function name and
   * -input|output|error *)
  let func_name = func.T.Func.name in
  let type_name = func_name ^ "-" ^ param_name in
  type_name


let make_param_alias_from_name name x =
  let res =
    A.({
      (T.default_alias ()) with

      name = Some name;
      typename = Some x;
      (* we set it only to distingwish between aliases defined in-line (see
       * make_param_alias) and artificially generated aliases like this one *)
      is_func_param = true;
    })
  in
  Piqloc.addrefret x res


let make_param_record name x =
  let res = R.({x with name = Some name}) in
  let res = copy_record res in (* preserve the original fields *)
  Piqloc.addrefret x res


let make_param_variant name x =
  let res = V.({x with name = Some name}) in
  let res = copy_variant res in (* preserve the original options *)
  Piqloc.addrefret x res


let make_param_enum name x =
  let res = E.({x with name = Some name}) in
  let res = copy_enum res in (* preserve the original options *)
  Piqloc.addrefret x res


let make_param_list name x =
  (* NOTE: the original record is preserved, because it is flat *)
  let res = L.({x with name = Some name}) in
  Piqloc.addrefret x res


let make_param_alias name x =
  (* NOTE: the original record is preserved, because it is flat *)
  let res = A.({x with name = Some name}) in
  Piqloc.addrefret x res


(* convert function parameter to a type:
 *  - if the function parameter is a name, convert it to correspondent alias
 *  - if the function parameter is an anonymous record, convert it to
 *    correspondent record
 *  - do the same for anonymous variants, enums and lists
 *)
let resolve_param func param_name param =
  let type_name = make_param_name func param_name in
  (* create a location reference for the newly constructed type name *)
  Piqloc.addref param type_name;
  let def =
    match param with
      | `name x ->
          (* make an alias from name reference *)
          `alias (make_param_alias_from_name type_name x)
      | `record x ->
          `record (make_param_record type_name x)
      | `variant x ->
          `variant (make_param_variant type_name x)
      | `enum x ->
          `enum (make_param_enum type_name x)
      | `list x ->
          `list (make_param_list type_name x)
      | `alias x ->
          `alias (make_param_alias type_name x)
  in
  Piqloc.addref param def;
  def


(* return func, [(def, (def_name, set_f))], where
 *
 * [def] is a definition derived from the function's input/output/erorr
 * parameter
 *
 * [def_name] is the definition's name
 *
 * [set_f] is a setter which takes a type definition and assigns it to the
 * correspondnt function's parameter
 *)
let process_func f =
  let open T.Func in
  let set_input def = set_is_func_param_flag def; f.resolved_input <- Some def
  and set_output def = set_is_func_param_flag def; f.resolved_output <- Some def
  and set_error def = set_is_func_param_flag def; f.resolved_error <- Some def
  in
  let process_param param_name param set_f =
    match param with
      | None -> []
      | Some param ->
          let def = resolve_param f param_name param in
          let res = (def, (C.typedef_name def, set_f)) in
          [res]
  in
  let input = process_param "input" f.input set_input
  and output = process_param "output" f.output set_output
  and error = process_param "error" f.error set_error
  in
  (input @ output @ error)


let get_function_defs (non_func_defs: T.typedef list) resolved_funs =
  (* get definitions derived from function parameters *)
  let defs_pairs = List.map process_func resolved_funs in
  let defs_pairs = List.concat defs_pairs in

  let defs, name_setter_assoc_l = List.split defs_pairs in
  (* prepare the setters map *)
  let setter_map = List.fold_left
    (fun t (name, setter) -> Idtable.add t name setter)
    Idtable.empty
    name_setter_assoc_l
  in
  (* TODO: optimize this linear scan *)
  let find_existing_def name =
    List.find
      (fun def -> name = C.typedef_name def)
      non_func_defs
  in
  (* exclude duplicate aliases (duplicates can be created as a result of double
   * conversion involving external Piqi self-spec; for example, when converting:
   *   .piqi -> .json -> .piqi *)
  let defs = List.filter
    (function
      | `alias implicit_alias when implicit_alias.A.is_func_param ->
          (* implicitly created alias that matches some top-level
           * *-input|output|error definition? *)
          (try
            let def = find_existing_def (some_of implicit_alias.A.name) in
            match def with
              | `alias explicit_alias when explicit_alias.A.typename <> implicit_alias.A.typename ->
                  (* we need to keep this definition so that we could report a
                   * duplicate typedef error because, here, the implicit alias
                   * derived from a function parameter clashes with another
                   * explicitly defined one; the only exception is when the
                   * implicit alias literally points to itself which is absurd
                   * -- it shouldn't be treated as implicit definition in
                   * such case *)
                  if implicit_alias.A.typename = implicit_alias.A.name
                  then false  (* remove *)
                  else true  (* keep *)
              | _ ->
                  false  (* remove *)
          with Not_found ->
            true  (* keep *)
          )
      | _ ->
          true  (* keep *)
    )
    defs
  in
  (* returned definitions derived from function parameters and a map that will
   * be used to set definitions, once resolved, to their appropirate slots in
   * resolved functions *)
  defs, setter_map


(* return a list under top-level list element; remove modname and include
 * directives *)
let prepare_included_piqi_ast ast =
  match ast with
    | `list l ->
        List.filter
          (function
            | `named {Piq_ast.Named.name = "module"} -> false
            | `named {Piq_ast.Named.name = "include"} -> false
            | _ -> true
          )
          l
    | _ ->
        assert false


let expand_includes piqi included_piqi =
  (* get the list of included modules' ASTs *)
  let included_asts =
    U.flatmap (fun x -> prepare_included_piqi_ast (some_of x.P.ast)) included_piqi
  in
  (* transform the module's ast to include all elements from all included
   * modules *)
  let ast = some_of piqi.P.ast in
  let new_ast =
    match ast with
      | `list l ->
          let res = `list (l @ included_asts) in
          Piqloc.addrefret ast res
      | _ ->
          assert false
  in
  let res_piqi = parse_piqi new_ast in

  (* discard unknown fields -- they have been processed and reported separately
   * for each individual module *)
  ignore (Piqobj_of_piq.get_unknown_fields ());

  res_piqi.P.ast <- Some ast;
  res_piqi


let is_extension modname =
  String.contains (Piqi_name.get_local_name modname) '.'


let find_piqi_file from_piqi modname =
  if from_piqi.P.is_embedded <> None
  then  (* embedded piqi => modname is already relative to the referee *)
    (* check if the module already loaded -- this is a typical case when
     * dependencies are also embedded *)
    let fname =
      try
        ignore (Piqi_db.find_piqi modname);
        modname ^ ".piqi"
      with Not_found ->
        Piqi_file.find_piqi modname
    in
    (modname, fname)
  else  (* regular module loaded from a .piqi file *)
    (* add the referee dirname as a .piqi search path in front of the curent
     * working directory, which is always the first element of the search path
     * list *)
    let dir = Filename.dirname (some_of from_piqi.P.file) in
    let found_dir, found_fname = Piqi_file.find_piqi_file modname ~extra_paths:[dir] in
    let fname = Filename.concat found_dir found_fname in

    (* if the module was found in the referee's directory, we need to rename
     * prepend the found module's name with the referee's path *)
    let is_existing_path = List.mem dir !Piqi_config.paths in
    let modname =
      if is_existing_path || found_dir <> dir
      then modname
      else
        let referee_modname = some_of from_piqi.P.modname in
        match Piqi_name.get_module_name referee_modname with
          | None ->
              modname
          | Some dirname ->
              dirname ^ "/" ^ modname
    in
    (modname, fname)


(* find all applicable extensions for a given module *)
let find_extensions piqi =
  let modname = some_of piqi.P.modname in
  let find_extension ext_name =
    try
      let modname = modname ^ "." ^ ext_name in
      let modname, fname = find_piqi_file piqi modname in
      trace "found extension: %s (%s)\n" modname fname;
      [modname]
    with Not_found ->
      []
  in
  if is_extension modname
  then [] (* extensions are not applicable to extensions *)
  else U.flatmap find_extension !Config.extensions


(* first steps of Piqi processing; this function can be called separately from
 * process_piqi *)
let pre_process_piqi ?modname ~fname ?(ast: piq_ast option) (orig_piqi: T.piqi) =
  (* report unparsed fields before we load dependencies (this is meaningless if
   * Piqi is not parsed from Piq)
   *)
  if ast <> None
  then (
    let unknown_fields = Piqobj_of_piq.get_unknown_fields () in
    let custom_fields = orig_piqi.P.custom_field in
    check_unknown_fields unknown_fields custom_fields;
  );

  (* preserve the original piqi by creating a shallow copy of it *)
  let piqi = copy_obj orig_piqi in
  piqi.P.original_piqi <- Some orig_piqi;
  piqi.P.ast <- ast;
  piqi.P.file <- Some fname;

  (* it is critical to cache loaded piqi module before we process any of its
   * dependencies; by doing this, we check for circular imports *)
  check_assign_module_name ?modname fname piqi;
  piqi


let is_processed piqi =
  piqi.P.included_piqi <> []


let is_being_processed piqi include_path =
  if is_processed piqi
  then false (* already processed *)
  else
    match piqi.P.modname with
      | None -> false
      | Some n ->
          (* we can rely only on module names to determine if the modules are
           * being processed; this is because we override the original piqi
           * object once we expand its includes -- see the "let piqi = " in
           * process_piqi() *)
          List.exists (fun x -> n = some_of x.P.modname) include_path


(* do include & extension expansion for the loaded piqi using extensions from
 * all included piqi modules *)
let rec process_piqi ?modname ?(include_path=[]) ?(fname="") ?(ast: piq_ast option) ~cache (input_piqi: T.piqi) =
  (* do nothing if it has been processed or being processed right now *)
  if is_processed input_piqi || is_being_processed input_piqi include_path
  then input_piqi
  else (

  (* as a first step, pre-process the module if it hasn't been preprocessed
   * already *)
  let piqi =
    match input_piqi.P.original_piqi with
      | None -> (* not pre-processed yet *)
          let piqi = pre_process_piqi ?modname ~fname ?ast input_piqi in
          if cache then Piqi_db.add_piqi piqi;
          trace "processing module %s\n" (some_of piqi.P.modname);
          piqi
      | Some _ ->
          trace "processing already pre-processed module %s\n" (some_of input_piqi.P.modname);
          input_piqi
  in
  let orig_piqi = some_of piqi.P.original_piqi in
  let ast = piqi.P.ast in
  trace_enter ();

  (*
   * handle includes
   *)
  let extension_includes =
    if ast = None
    then
      (* extensions are not applicable for non-Piq representation *)
      []
    else
      List.map (fun x -> Includ.({modname = x; unparsed_piq_ast = None}))
      (find_extensions piqi)
  in
  let includes = piqi.P.includ @ extension_includes in
  let included_piqi = load_includes piqi includes  ~include_path in
  let piqi =
    if included_piqi = []
    then piqi
    else (
      let extended_piqi = expand_includes piqi included_piqi in

      extended_piqi.P.original_piqi <- Some orig_piqi;
      extended_piqi.P.modname <- piqi.P.modname;
      extended_piqi.P.file <- piqi.P.file;

      (* replace previously cached unexpanded piqi module if it is already present in the
       * cache *)
      Piqi_db.replace_piqi extended_piqi;
      (* replace previous unexpanded instance of the piqi module in all its
       * includes;
       * TODO: omit this step if there's no recursive includes *)
      List.iter (fun p ->
        if List.exists (fun pp -> pp == piqi) p.P.included_piqi
        then
          p.P.included_piqi <- List.map (
            fun pp -> if pp == piqi then extended_piqi else pp
          ) p.P.included_piqi
      ) included_piqi;

      extended_piqi
    )
  in

  (* append the original (input) piqi module to the list of included piqi
   * modules *)
  let modules = included_piqi @ [piqi] in
  piqi.P.included_piqi <- modules;

  (*
   * get all extensions
   *)
  List.iter check_extension orig_piqi.P.extend;
  let defs_extensions, func_extensions, import_extensions =
    partition_extensions piqi.P.extend
  in

  (* NOTE: for extensions we're considering all custom fields from all
   * included modules *)
  let custom_fields = piqi.P.custom_field in

  (*
   * handle imports
   *)
  (* get all imports from included modules *)
  let imports = piqi.P.import in
  let extended_imports =
    if import_extensions = []
    then imports
    else extend_imports imports import_extensions custom_fields
  in
  (* preserve the original imports *)
  let resolved_imports = copy_imports extended_imports in
  load_imports piqi resolved_imports;
  piqi.P.resolved_import <- resolved_imports;
  piqi.P.extended_import <- extended_imports;

  (*
   * handle imported defs
   *)
  let imported_defs = get_imported_defs resolved_imports in
  piqi.P.imported_typedef <- imported_defs;

  (* fill idtable with their imported modules' definitions *)
  let idtable = Idtable.empty in
  let idtable = add_imported_typedefs idtable imported_defs in

  (* add built-in type defintions to the idtable *)
  let idtable =
    if not !Config.flag_no_builtin_types && not !is_boot_mode && not (C.is_self_spec piqi)
    then add_typedefs idtable !C.builtin_typedefs
    else idtable
  in
  (*
   * handle functions
   *)
  (* get all functions from this module and included modules *)
  let funs = piqi.P.func in

  (* check for duplicate function names *)
  let func_names = List.map (fun x -> x.T.Func.name) funs in
  List.iter check_name func_names;
  check_dup_names "function" func_names;

  let extended_funs =
    if func_extensions = []
    then funs
    else extend_functions funs func_extensions custom_fields
  in
  (* preserve the original functions *)
  let resolved_funs = List.map copy_obj extended_funs in
  piqi.P.resolved_func <- resolved_funs;
  piqi.P.extended_func <- extended_funs;

  (* get definitions derived from function parameters *)
  let func_defs, func_defs_map = get_function_defs piqi.P.typedef resolved_funs in
  piqi.P.func_typedef <- func_defs;

  (* add function type definitions to Piqi defs *)
  let defs = piqi.P.typedef @ func_defs in

  (* expand all extensions over all definitions *)
  (* NOTE: boot defs can not be extended *)
  let extended_defs =
    if defs_extensions = []
    then (
      (* partial check_defs () pass to make sure that at least all defs have
       * names; otherise we'll crash with assert failure in one of
       * C.typedef_name calls below before we get to run resolve_defs that
       * includes the actual checks *)
      List.iter check_def defs;
      defs
    )
    else (
        (* defs should be correct before extending them *)
        (* XXX: can they become correct after extension while being
         * incorrect before? *)
        check_defs idtable defs ~piqi;
        apply_defs_extensions defs defs_extensions custom_fields
    )
  in
  (* preserve the original defintions by making a copy *)
  let resolved_defs = copy_defs extended_defs in

  (* set resolved_func.resolved_input/output/error fields to their correspondent
   * defs that were derived from function parameters *)
  List.iter
    (fun def ->
      try
        let setter = Idtable.find func_defs_map (C.typedef_name def) in
        setter def
      with
        Not_found -> ()
    )
    resolved_defs;

  (* move out function defs from the list of extended typedefs *)
  (* TODO: optimize this linear scan *)
  let is_func_def name =
    List.exists
      (fun def -> name = C.typedef_name def)
      func_defs
  in
  let extended_func_defs, extended_defs = List.partition
    (fun def -> is_func_def (C.typedef_name def))
    extended_defs
  in

  (* if the module includes (or is itself) "piqi", use hash-based field
   * and option codes instead of auto-enumerated ones
   *
   * NOTE: code assignment is needed only for .piqi, Piqi specifications encoded
   * in other formats must already include explicitly speficied (and correct!)
   * wire codes.
   *
   * NOTE: this step must be performed before resolving defaults -- this is
   * critical for potential piqi extensions such as those used in various piqic
   *)
  if ast <> None && C.is_self_spec piqi
  then Piqi_protobuf.add_hashcodes resolved_defs;

  (* check defs, resolve defintion names to types, assign codes, resolve default
   * fields *)
  let idtable = resolve_defs idtable resolved_defs ~piqi in

  piqi.P.extended_typedef <- extended_defs;
  piqi.P.extended_func_typedef <- extended_func_defs;

  piqi.P.resolved_typedef <- resolved_defs;

  (* run registered processing hooks *)
  List.iter (fun f -> f idtable piqi) !processing_hooks;

  (* resolve defaults ANY to OBJ using field types and codes; we need to do it
   * after executing hooks, because otherwise json names will be unresolved and
   * default field resolution will fail *)
  List.iter resolve_defaults resolved_defs;

  trace_leave ();
  piqi
  )
 

(* XXX: disallow include and import of the same module or produce a warning? *)
(* NOTE: using absolute paths by this point *)
and load_piqi_file ?modname ?include_path fname =
  trace "file: %s\n" fname;
  trace_enter ();
  (*
  Piqloc.trace := true;
  *)
  let ast = read_piqi_file fname in
  (* cache only those modules that were included/imported/referenced by their
   * modname *)
  let cache = modname <> None in
  let piqi = load_piqi_ast ?modname ?include_path ~cache fname ast in
  trace_leave ();

  piqi


and load_piqi_ast ?modname ?(include_path=[]) ~cache fname (ast :piq_ast) =
  let piqi = parse_piqi ast in
  process_piqi piqi ?modname ~include_path ~cache ~ast ~fname


and load_piqi_module ?(include_path=[]) modname fname =
  (* check if the module is already loaded *)
  try
    let piqi = Piqi_db.find_piqi modname in
    (* make sure it is fully processed at this point (some modules can be loaded
     * but only pre-processed) *)
    process_piqi piqi ~include_path ~cache:false
  with Not_found ->
    load_piqi_file fname ~modname ~include_path


and load_imports piqi l =
  List.iter (load_import piqi) l;
  (* check for duplicates & looped imports including self-import loop *)
  let rec check_dups = function
    | [] -> ()
    | h::t ->
        begin
          if List.exists (fun x -> h.Import.name = x.Import.name) t
          then error h ("duplicate import name " ^ U.quote (some_of h.Import.name));

          if List.exists (fun x -> h.Import.piqi == x.Import.piqi) t
          then warning h ("duplicate import module " ^ U.quote h.Import.modname);

          check_dups t
        end
  in
  check_dups l;

  (* simple check for import loops; provides very limited diagnostic *)
  (* NOTE: import loops disallowed in Protobuf as well *)
  if List.exists (fun x -> some_of x.Import.piqi == piqi) l
  then error piqi "imported piqi modules form a loop"


and load_import from_piqi x =
  let open Import in (
    trace "import: %s\n" x.modname;
    check_modname x.modname;
    let scoped_modname, fname =
      try find_piqi_file from_piqi x.modname
      with Not_found ->
        error x.modname ("imported piqi module is not found: " ^ U.quote x.modname)
    in
    (* save the original modname and replace it with the resolved scoped modname
     * NOTE: they can be exactly the same *)
    x.orig_modname <- Some x.modname;
    x.modname <- scoped_modname;
    let piqi = load_piqi_module scoped_modname fname in
    (* save imported piqi in import.piqi field *)
    x.piqi <- Some piqi;
    assign_import_name x;
  )


and load_includes ~include_path piqi l =
  List.iter (fun x ->
    if x.Includ.modname = some_of piqi.P.modname
    then error x "piqi module includes itself"
  ) l;

  (* if there's any included extension that's also a parent, remove all included
   * extensions from the list of includes *)
  let remove_extensions l include_path =
    if List.exists (fun x ->
        let n = x.Includ.modname in
        is_extension n &&
        List.exists (fun x -> n = some_of x.P.modname) include_path
      ) l
    then
      List.filter (fun x ->
        let n = x.Includ.modname in
        let keep = not (is_extension x.Includ.modname) in
        if not keep
        then trace "removing extension include %s\n" (U.quote n);
        keep
      ) l
    else l
  in

  let new_include_path = piqi :: include_path in

  let l = remove_extensions l include_path in
  let included_piqi = List.map (load_include piqi ~include_path:new_include_path) l in

  let process_recursive_piqi p =
    trace "included piqi module %s forms a loop\n" (U.quote (some_of p.P.modname));
    let includes = remove_extensions p.P.includ new_include_path in
    (* check for all Piqi includes that have been already processed in the DFS
     * include path *)
    List.iter
      (fun x ->
        let n = x.Includ.modname in
        if List.exists (fun p -> n = some_of p.P.modname) new_include_path
        then error x ("recursive include " ^ U.quote n)
      )
      includes;
    (* process the remaining includes as if they were included by the current
     * module *)
    trace_enter ();
    let res = load_includes ~include_path piqi includes in
    trace_leave ();
    res
  in

  (* append all Piqi modules from all included Piqi modules *)
  let l = U.flatmap
    (fun x ->
      let res = x.P.included_piqi in
      if res = [] (* the module is loaded, but hasn't been processed yet *)
      then
        (* process recursive module's non-recursive includes
         *
         * NOTE: we need the module's ast anyway, so appending it to the
         * results; if it happends to be this same module, it will be filtered
         * out below
         *)
        (process_recursive_piqi x) @ [ x ]
      else
        res
    )
    included_piqi
  in

  (* remove duplicates -- one module may be included from many modules *)
  let res = U.uniqq l in

  (* finally, remove itself from the list of included modules; it could happen
   * if there is a recursion *)
  List.filter (fun x -> x != piqi) res


and load_include ~include_path from_piqi x =
  let open Includ in (
    trace "include: %s\n" x.modname;
    check_modname x.modname;
    let scoped_modname, fname =
      try find_piqi_file from_piqi x.modname
      with Not_found ->
        error x.modname ("included piqi module is not found: " ^ U.quote x.modname)
    in
    (* replace the original modname with the resolved scoped modname
     * NOTE: they can be exactly the same *)
    x.modname <- scoped_modname;
    load_piqi_module scoped_modname fname ~include_path
  )


let piqi_loader ?modname fname =
  load_piqi_file ?modname fname


let embedded_modname = "embedded/piqi-lang"


let find_embedded_piqtype name =
  Piqi_db.find_piqtype (embedded_modname ^ "/" ^ name)


(*
 * booting code; preparing the initial statically defined piqi defintions
 *)
let boot () =
  trace "boot(0)\n";
  (* process embedded Piqi self-specifications *)
  (* don't cache them as we are adding the spec to the DB explicitly below *)

  trace "boot(1)\n";
  let lang = process_piqi Piqi_boot.piqi_lang ~cache:false in
  piqi_lang := Some lang;

  trace "boot(2)\n";
  let spec = process_piqi Piqi_boot.piqi_spec ~cache:false in
  piqi_spec := Some spec;

  trace "boot(3)\n";
  (* populate the list of built-in types *)
  C.builtin_typedefs := List.filter C.is_builtin_def spec.P.resolved_typedef;

  (* add the self-spec to the DB under a special name *)
  spec.P.modname <- Some "embedded/piqi";
  Piqi_db.add_piqi spec;

  (* add the self-spec to the DB under a special name *)
  lang.P.modname <- Some embedded_modname;
  Piqi_db.add_piqi lang;

  (* resolved type definition for the Piqi language *)
  piqi_lang_def := find_embedded_piqtype "piqi";
  (* resolved type definition for the Piqi specification *)
  piqi_spec_def := Piqi_db.find_piqtype "embedded/piqi/piqi";
  (* resolved "typedef" type definition *)
  typedef_def := find_embedded_piqtype "typedef";
  field_def := find_embedded_piqtype "field";
  option_def := find_embedded_piqtype "option";
  function_def := find_embedded_piqtype "function";
  import_def := find_embedded_piqtype "import";

  (* turn boot mode off *)
  is_boot_mode := false;

  (* resume object location tracking; it was off from the beginning for a reason
   * -- see piqloc.ml for details *)
  Piqloc.resume ();
  (* also, we need to preserve some fake references we've added during boot *)
  Piqloc.preserve ();

  (* initialize Piqi loader; doing it this way, because Piqi and Piqi_db are
   * mutually recursive modules *)
  Piqi_db.piqi_loader := Some piqi_loader;

  trace "boot(1)\n";
  ()


let _ =
  (*
  if !Sys.interactive
  then () (* don't do anything in interactive (toplevel) mode *)
  else
  *)
  (
    (*
    Piqi_config.debug_level := 1;
    *)
    boot ();
  )


(* public interface: load piqi file *)
let load_piqi fname ch :T.piqi =
  trace "loading piqi file: %s\n" fname;
  trace_enter ();

  let ast = read_piqi_channel fname ch in
  let piqi = load_piqi_ast fname ast ~cache:true in

  trace_leave ();
  piqi


(* put extended embedded function parameters back to extended functions *)
let reconsitute_extended_functions funs func_typedefs =
  (* add all extended embedded typedefs into a map *)
  let idtable = add_typedefs Idtable.empty func_typedefs in
  let reconstitute_param f param_name orig_param =
    match orig_param with
      | None (* no parameter *)
      | Some (`name _) -> (* it wasn't an embedded parameter in the first place *)
          orig_param (* returning the original one *)
      | _ ->
          let type_name = make_param_name f param_name in
          let typedef = Idtable.find idtable type_name in
          (* now, anonymize the typedef to make it look an embedded typedef *)
          let embedded_typedef =
            match typedef with
              | `record x ->
                  `record {x with R.name = None}
              | `variant x ->
                  `variant {x with V.name = None}
              | `enum x ->
                  `enum {x with E.name = None}
              | `list x ->
                  `list {x with L.name = None}
              | `alias x ->
                  `alias {x with A.name = None}
          in
          Some (embedded_typedef :> T.function_param)
  in
  let reconsitute_extended_function f =
    T.Func.({
      f with
      input = reconstitute_param f "input" f.input;
      output = reconstitute_param f "output" f.output;
      error = reconstitute_param f "error" f.error;
    })
  in
  List.map reconsitute_extended_function funs


(* transform functions to remove embedded type definitions *)
let expand_function f =
  let transform_param param_name param =
    match param with
      | None ->
          None
      | Some (`name _) ->
          param
      | Some _ ->
          let type_name = make_param_name f param_name in
          Some (`name type_name)
  in
  T.Func.({
    f with
    input = transform_param "input" f.input;
    output = transform_param "output" f.output;
    error = transform_param "error" f.error;
  })


(* expand Piqi module's includes and, optionally, extensions and function *)
let expand_piqi ~extensions ~functions piqi =
  let open P in
  let orig_piqi = some_of piqi.original_piqi in

  (* always expand includes *)
  let res_piqi = {
    orig_piqi with
    includ = [];
    (* we need this to include unparsed piq asts from all included piq modules
     * not just the original module *)
    unparsed_piq_ast = piqi.unparsed_piq_ast;
  }
  in
  let res_piqi =
    if not extensions
    then res_piqi
    else (* expand extensions *)
      {
        res_piqi with

        extend = [];
        typedef = piqi.extended_typedef; (* all typedefs with extensions applied *)
        import = piqi.extended_import; (* all imports with extensions applied *)
        (* all functions with extensions applied *)
        func = reconsitute_extended_functions piqi.extended_func piqi.extended_func_typedef;
      }
  in
  let res_piqi =
    if not functions
    then res_piqi
    else (* expand functions *)
      let func_typedefs =
        if extensions
        then piqi.extended_func_typedef
        else piqi.func_typedef
      in
      {
        res_piqi with
        (* add embedded definitions from function to the list of top-level defs *)
        typedef = res_piqi.typedef @ func_typedefs;
        (* remove embedded defintions from function parameters *)
        func = List.map expand_function res_piqi.func;
      }
  in res_piqi


(* narrow down Piqi language to Piqi specficiation
 *
 * Piqi language (piqi-lang.piqi) adds the following extensions to Piqi
 * specification (piqi.piqi):
 *
 *   - includes
 *   - extensions
 *   - custom fields
 *   - embedded definition of function parameters
 *
 * In addition, in certain cases, Piqi specificaion should have fully resolved
 * defaults. For example, all required fields should be explicitly marked as
 * such instead of leaving this value out and assuming the are required by
 * default.
 *)
let lang_to_spec piqi =
  let open P in
  (* expand includes, extensions and functions *)
  expand_piqi piqi ~extensions:true ~functions:true


(* is_external_mode=true means that defaults and potentially other piqi-any
 * values will be fully expanded (converted) to their piq ast representation *)
let piqi_to_ast ?(is_external_mode=true) piqi =
  debug "piqi_to_ast(0)\n";
  Piqloc.pause (); (* we don't really need to track locations at this stage *)

  let ast =
    U.with_bool Piqobj_to_piq.is_external_mode is_external_mode
    (fun () -> mlobj_to_ast !piqi_lang_def T.gen__piqi piqi)
  in
  Piqloc.resume ();
  debug "piqi_to_ast(1)\n";
  ast


(* transform piqi ast so that type definitions embedded in function definitions
 * get compatible with Piqi-spec *)
let transform_piqi_ast (ast: piq_ast) =
  let tr = Piq_ast.transform_ast in
  (* map ../name.x -> x *)
  let rm_param_extra path =
    tr path (
      function
        | `named {Piq_ast.Named.name = "name"; value = v} -> [v]
        | x -> [x]
    )
  in
  let (|>) a f = f a in
  ast
  |> rm_param_extra ["function"; "input"]
  |> rm_param_extra ["function"; "output"]
  |> rm_param_extra ["function"; "error"]


(* the 'piqi_piqtype' and 'add_codes' options are used by piqi_compiler *)
let piqi_to_piqobj
        ?(piqi_piqtype: T.piqtype option)  (* type of a custom self-spec *)
        ?(add_codes=false)  (* whether to add field/option integer codes *)
        piqi =
  debug "piqi_to_piqobj(0)\n";
  Piqloc.pause ();

  (* we'll need them later *)
  let custom_fields = piqi.P.custom_field in

  let piqi_spec = lang_to_spec piqi in
  (* make sure that the module's name is defined *)
  let piqi_spec = P.({piqi_spec with modname = piqi.P.modname}) in

  (* include all automatically assigned hash-based codes for fiels and options;
   * this is a protection against changing the hashing algorithm: even if new
   * piqi versions use a different hashing algorith, previous self-definitions
   * will be still readable *)
  if C.is_self_spec piqi
  then Piqi_protobuf.add_hashcodes piqi_spec.P.typedef
  else if add_codes (* XXX: always add ordinal codes? *)
  then Piqi_protobuf.add_codes piqi_spec.P.typedef;

  (* piqi compile mode? TODO: this is a hack; we need a more explicit way to
   * determine whether it is a piqi compile mode *)
  if add_codes
  then piqi_spec.P.file <- piqi.P.file;

  let ast = piqi_to_ast piqi_spec ~is_external_mode:false in
  let ast = transform_piqi_ast ast in

  if !Config.debug_level > 1
  then (
    debug "BEGIN piqi_to_piqobj ast:\n\n";
    Piq_gen.to_channel stderr ast;
    debug "\n\nEND piqi_to_piqobj ast\n";
  );

  (* turn the piq ast back to piqobj *)
  let piqobj =
    match piqi_piqtype with
      | None ->
          U.with_bool C.Config.flag_no_warnings true
          (fun () -> Piqobj_of_piq.parse_obj !piqi_spec_def ast)
      | Some piqi_piqtype ->
          (* NOTE: normally, we would ignore unknown fields here as well -- they don't
           * matter because we are transforming the spec that has been validated and
           * warnings already printed (if any)...
           *
           * HOWEVER, there's a case in which we want to be very careful: this is when
           * the custom piqi spec falls behind or somehow becomes incompatible with our
           * own piqi spec; in this case, the need to print the unknown field warning or
           * even an error, because some core Piqi properties may be silently dropped as
           * a result of this conversion *)
          let piqobj =
            U.with_bool C.is_inside_parse_piqi true
            (fun () -> Piqobj_of_piq.parse_obj piqi_piqtype ast)
          in
          let unknown_fields = Piqobj_of_piq.get_unknown_fields () in
          (* TODO: don't print the same warnings twice -- we may have printed some of
           * them when we were initially loading piqi; also fix the location -- it is
           * not being reported correctly *)
          check_unknown_fields unknown_fields custom_fields
            ~prepend:(fun () ->
              C.piqi_warning "the following property is not recognized by the custom self-spec and will be ignored:"
            );
          piqobj
  in
  Piqloc.resume ();
  debug "piqi_to_piqobj(1)\n";
  piqobj


let piqi_of_piqobj ?(process=true) piqobj =
  debug "piqi_of_piqobj(0)\n";
  let piqi_ast = Piqobj_to_piq.gen_obj piqobj in
  let piqi = parse_piqi piqi_ast in

  (* mark all externally loaded modules as embedded to prevent attempts of
   * looking for their imported dependencies in the filesystem *)
  piqi.P.is_embedded <- Some true;

  let piqi =
    if process
    then
      process_piqi piqi ~cache:false
    else
      piqi
  in
  debug "piqi_of_piqobj(-)\n";
  piqi


(* -1 means don't generate field header *)
let piqi_to_pb ?(code = -1) piqi =
  debug "piqi_to_pb(0)\n";
  let piqobj = piqi_to_piqobj piqi in
  (* don't produce location references as don't care about it in general when
   * generating data *)
  Piqloc.pause ();
  let res =
    U.with_bool Piqobj_to_protobuf.is_external_mode true
    (fun () -> Piqobj_to_protobuf.gen_obj code piqobj)
  in
  Piqloc.resume ();
  debug "piqi_to_pb(1)\n";
  res


let piqi_of_pb ?(process=true) buf =
  debug "piqi_of_pb(0)\n";
  (* don't store location references as we're loading from the binary object *)
  Piqloc.pause ();

  let piqtype = !piqi_spec_def in

  (* don't resolve defaults *)
  let piqobj =
    C.with_resolve_defaults false (fun () -> Piqobj_of_protobuf.parse_obj piqtype buf)
  in
  let piqi = piqi_of_piqobj piqobj ~process in

  Piqloc.resume ();
  debug "piqi_of_pb(1)\n";
  piqi


let rec get_piqi_deps ?(only_imports=true) piqi =
  (* get all includes and includes from all included modules *)
  let include_deps =
    if only_imports
    then []
    else
      (* remove the module itself from the list of included deps (it is always
       * at the end of the list) *)
      List.filter (fun x -> x != piqi) piqi.P.included_piqi
  in
  (* get all dependencies from imports *)
  let import_deps =
    U.flatmap (fun x ->
        let piqi = some_of x.T.Import.piqi in
        U.flatmap (get_piqi_deps ~only_imports) piqi.P.included_piqi)
      piqi.P.resolved_import
  in
  (* NOTE: includes go first in the list of dependencies *)
  let l = include_deps @ import_deps @ [piqi] in
  (* remove duplicate entries *)
  U.uniqq l

