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

(* handling of Piq-specific Piqi properties *)


module C = Piqi_common
open C


(* check correspondent between primitive Piqi type and Piq representation format
 *)
let check_piq_format obj piq_format piqtype =
  let piqtype = C.unalias piqtype in
  match piq_format, piqtype with
    | `word, `string -> ()
    | `text, `string -> ()
    | _ when C.is_typedef piqtype ->
        error obj
          ("piq-format can not be defined for non-primitive type " ^ U.quote (C.piqi_typename piqtype))
    | _ ->
        error obj
          ("invalid piq-format for type " ^ U.quote (C.piqi_typename piqtype))


let rec resolve_piq_format (piqtype: T.piqtype) =
  (* upper-level setting overrides lower-level setting *)
  match piqtype with
    | `alias x ->
        let piq_format = x.A.piq_format in
        if piq_format <> None
        then piq_format
        else
          (* try looking in lower-level aliases *)
          resolve_piq_format (some_of x.A.piqtype)
    | _ ->
        None (* piq format can not be defined for non-primitive types *)


let check_resolve_piq_format obj piq_format piqtype =
  match piq_format, piqtype with
    | Some f, Some t -> (* already defined, just check *)
        check_piq_format obj f t;
        piq_format
    | None, Some t ->
        resolve_piq_format t
    | Some t, None ->
        error obj "piq-format can not be defined when there is no type"
    | None, None ->
        None


let resolve_field_piq_format x =
  let open F in
  x.piq_format <- check_resolve_piq_format x x.piq_format x.piqtype


let resolve_option_piq_format x =
  let open O in
  x.piq_format <- check_resolve_piq_format x x.piq_format x.piqtype


let resolve_typedef_piq_format = function
  | `record r ->
      List.iter resolve_field_piq_format r.R.field
  | `variant v ->
      List.iter resolve_option_piq_format v.V.option
  | `alias a ->
      a.A.piq_format <- check_resolve_piq_format a a.A.piq_format a.A.piqtype
  | `list l ->
      l.L.piq_format <- check_resolve_piq_format l l.L.piq_format l.L.piqtype
  | `enum _ ->
      ()


let process_field_piq_positional record_piq_positional x =
  let open F in
  begin
    (match x.name, x.typename with
      | Some _, None ->  (* flag *)
          if x.piq_positional = Some true
          then error x "flags can not be positional"
      | _ -> ()
    );

    (* inherit the record-level setting when the local per-field setting is
     * missing *)
    if x.piq_positional = None
    then x.piq_positional <- record_piq_positional
  end


let process_typedef_piq_positional = function
  | `record x ->
      List.iter (process_field_piq_positional x.R.piq_positional) x.R.field
  | _ -> ()


let check_name x =
  if not (Piqi_name.is_valid_name x)
  then error x ("invalid piq alias name: " ^ U.quote x)
  else ()


let check_opt_name = function
  | None -> ()
  | Some x -> check_name x


let check_field_piq_alias x =
  check_opt_name x.F.piq_alias


let check_option_piq_alias x =
  check_opt_name x.O.piq_alias


let check_typedef_piq_alias = function
  | `record x ->
      List.iter check_field_piq_alias x.R.field
  | `variant x ->
      List.iter check_option_piq_alias x.V.option
  | _ -> ()


let process_typedefs typedefs =
  (* resolve Piq representation format settings *)
  List.iter resolve_typedef_piq_format typedefs;
  (* stuff related to .piq-positional property *)
  List.iter process_typedef_piq_positional typedefs;
  (* validate .piq-alias names
   * TODO, XXX: check for all sorts of duplicates; warn if .name masks
   * .piq-alias *)
  List.iter check_typedef_piq_alias typedefs;
  ()

