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
 * common piqi compiler interfaces used by "piqi compile" and piqic-ocaml
 *)


module C = Piqi_common
open C


let flag_strict = ref false
let arg__strict =
  let (name, _setter, descr) = Piqi_command.arg__strict in
  (* override the original setter but keep the option name and the description;
   * we do this, because although it means the same, it is applied at a later
   * stage -- we control it manually below *)
  (name, Arg.Set flag_strict, descr)


let getopt_speclist = Piqi_command.common_speclist @
  [
    arg__strict;
    Piqi_command.arg__include_extension;
  ]


let load_self_spec ?(filename="") buf =
  trace "piqi_compile: loading self-spec from %s\n" (U.quote filename);
  trace_enter ();
  let self_spec =
    try
      (* TODO: we can read piqi directly using Piqi_piqi.parse_piqi, because
       * self-spec is guaranteed to not have any incompatibilities with
       * piqi_lang including functions and other parts. This makes "piqi
       * compile" start faster than if we used "Piqi.piqi_of_pb buf" *)
      Piqi.piqi_of_pb buf (* NOTE: not caching the loaded module *)
    with exn ->
      Printf.eprintf "error: failed to load self-spec from %s:\n" (U.quote filename);
      raise exn (* try to give more details about what when wrong *)
  in
  trace_leave ();
  self_spec


let get_self_spec_piqtype ?(filename="") self_spec typename =
  let piqi_def =
    try Piqi_db.find_local_typedef self_spec.P.resolved_typedef typename
    with Not_found ->
      Printf.eprintf
        "invalid self-spec read from %s: no definition named %s\n"
        (U.quote filename) (U.quote typename);
      piqi_error "piqi compile: invalid self-spec"
  in
  (piqi_def: T.typedef :> T.piqtype)


(* make piqi/piqi-list top-level record from the list of piqi piqobjs *)
let make_piqi_list_piqobj piqi_list_piqtype (piqi_piqobj_list: Piqobj.obj list) :Piqobj.obj =
  let r =
    match piqi_list_piqtype with
      | `record r -> r
      | _ -> assert false
  in
  (* the first and the only record field should correspond to the piqi type *)
  let f = List.hd r.R.field in
  let fields = List.map (fun x -> Piqobj.Field.({t = f; obj = Some x})) piqi_piqobj_list in
  let record = Piqobj.Record.({t = r; field = fields; unparsed_piq_fields_ref = None}) in
  `record record


(* public library API; currently called only from Piqic_ocaml *)
let compile ?(extensions=[]) self_spec_bin ifile =
  trace "compile_to_pb:\n";
  trace_enter ();

  (* TODO: restore the state after we are done *)
  List.iter (fun e -> Piqi_config.add_include_extension e) extensions;

  let buf = Piqi_piqirun.init_from_string self_spec_bin in
  let self_spec = load_self_spec buf in

  let ch = Piqi_command.open_input ifile in
  let piqi = Piqi.load_piqi ifile ch in

  trace "getting all imported dependencies\n";
  let piqi_list = Piqi.get_piqi_deps piqi in

  (* get necessary piqtypes from the self-spec *)
  let piqi_piqtype = get_self_spec_piqtype self_spec "piqi" in
  let piqi_list_piqtype = get_self_spec_piqtype self_spec "piqi-list" in

  trace "converting modules to internal representation\n";
  Config.flag_strict := !flag_strict;

  (* convert all modules to internal representation *)
  let piqobj_list = List.map
    (fun piqi -> Piqi.piqi_to_piqobj piqi ~piqi_piqtype ~add_codes:true)
    piqi_list
  in

  trace "generating pb\n";
  let piqobj = make_piqi_list_piqobj piqi_list_piqtype piqobj_list in
  let res = Piqi_convert.to_pb_string (Piqi_convert.Piqobj piqobj) in
  trace_leave ();
  res

