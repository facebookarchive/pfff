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


(*
 * set json- names if not specified by user
 *)


(* json name of piqi name *)
let json_name' n =
  U.dashes_to_underscores n


let json_name n =
  Some (json_name' n)


(* check name validity *)
let check_json_name s =
  let error () = error s "invalid json-name" in

  if s = ""
  then error ();

  (match s.[0] with
    | 'a'..'z' | 'A'..'Z' | '_' -> ()
    | _ -> error ()
  );

  for i = 1 to String.length s - 1
  do
    match s.[i] with
      | 'a'..'z'
      | 'A'..'Z'
      | '0'..'9'
      | '_' -> ()
      | _ -> error ()
  done


(* XXX: use name instead of json_name for foreign types? *)
let typedef_json_name = function
  | `record t -> t.R.json_name
  | `variant t -> t.V.json_name
  | `enum t -> t.E.json_name
  | `alias t -> t.A.json_name
  | `list t -> t.L.json_name
  | _ ->
      (* this function will be called only for named types (i.e. typedefs) *)
      assert false


let json_name_of name piqtype =
  match name, piqtype with
    | Some n, _ -> json_name n
    | None, Some t -> typedef_json_name t
    | _ -> assert false


let json_name_field x =
  let open Field in
  match x.json_name with
    | None -> x.json_name <- json_name_of x.name x.piqtype
    | Some n -> check_json_name n


let json_name_record x =
  let open Record in
  (match x.json_name with
     | None -> x.json_name <- json_name (some_of x.name)
     | Some n -> check_json_name n
  )


let json_name_option x =
  let open Option in
  match x.json_name with
    | None -> x.json_name <- json_name_of x.name x.piqtype
    | Some n -> check_json_name n


let json_name_variant x =
  let open Variant in
  (match x.json_name with
     | None -> x.json_name <- json_name (some_of x.name)
     | Some n -> check_json_name n
  )


let json_name_enum x =
  let open Enum in
  (match x.json_name with
     | None -> x.json_name <- json_name (some_of x.name)
     | Some n -> check_json_name n
  )


let json_name_alias x =
  let open Alias in
  match x.json_name with
    | None -> x.json_name <- json_name (some_of x.name)
    | Some n -> check_json_name n


let json_name_list x =
  let open L in
  match x.json_name with
    | None -> x.json_name <- json_name (some_of x.name)
    | Some n -> check_json_name n


let json_name_typedef = function
  | `record x -> json_name_record x
  | `variant x -> json_name_variant x
  | `enum x -> json_name_enum x
  | `alias x -> json_name_alias x
  | `list x -> json_name_list x


(* name fields and options *)
let json_name_record' x =
   List.iter json_name_field x.R.field

let json_name_variant' x =
   List.iter json_name_option x.V.option

let json_name_enum' x =
   List.iter json_name_option x.E.option

let json_name_typedef' = function
  | `record x -> json_name_record' x
  | `variant x -> json_name_variant' x
  | `enum x -> json_name_enum' x
  | _ -> ()


let json_name_defs defs =
    (* name data structures *)
    List.iter json_name_typedef defs;
    (* name fields and options *)
    List.iter json_name_typedef' defs


let json_name_piqi _idtable (piqi:T.piqi) =
  let open P in
  json_name_defs piqi.resolved_typedef


(* NOTE: this function is called only in case if a JSON-related operation is
 * performed. We don't need this startup overhead otherwise *)
let init () =
  trace "init JSON\n";
  (* create JSON names in embedded Piqi self-specification *)
  (* AND add/check JSON names when loading any other Piqi modules *)
  Piqi.register_processing_hook json_name_piqi


(**)


let read_json_obj json_parser =
  let res = Piqi_json_parser.read_next json_parser in
  res


(* for internal use only: read one parsed JSON value from its string
 * representation *)
let json_of_string s :Piqi_json_type.json =
  let json_parser = Piqi_json_parser.init_from_string s in
  let res =
    try Piqi_json_parser.read_all json_parser
    with C.Error ((_, lnum', cnum'), error) ->
      (* string location can be missing when we parse from JSON embedded in
       * Protobuf *)
      let (fname, lnum, cnum) =
        try Piqloc.find s
        with Not_found -> ("embedded", 1, -1)
      in
      (* adjust location column number: add the original column number of the
       * '#' character + 1 for the space that follows it; note that this method
       * doesn't give 100% guarantee that the offset is correct, but it is
       * accurate if all the text literal lines start at the same column *)
      let loc = (fname, lnum + lnum' - 1, cnum + cnum' + 1) in
      C.error_at loc ("error parsing embedded JSON: " ^ error)
  in
  match res with
    | [x] -> x
    | _::o::_ ->
        C.error o "string includes more than one JSON value"
    | [] ->
        C.error s "string doesn't have JSON data"

let _ =
  Piqobj.json_of_string := (fun x -> json_of_string x)

