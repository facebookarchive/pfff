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


open Piqi_common


let chop_extension fname =
  try Filename.chop_extension fname
  with _ -> fname


let chop_all_extensions fname =
  let start =
    try (String.rindex fname '/') + 1
    with Not_found -> 0
  in
  try 
    let i = String.index_from fname start '.' in
    String.sub fname 0 i
  with Not_found -> fname


let chop_piqi_extensions fname =
  if Filename.check_suffix fname ".proto.piqi"
  then Filename.chop_suffix fname ".proto.piqi"
  else
    if Filename.check_suffix fname ".piqi"
    then Filename.chop_suffix fname ".piqi"
    else fname


(* basename + chop .piqi and .proto.piqi extensions *)
let basename filename =
  let basename = Filename.basename filename in
  chop_piqi_extensions basename


let dirname = Filename.dirname


let concat = Filename.concat


let get_extension s =
  try
    let pos = String.rindex s '.' in
    String.sub s (pos + 1) (String.length s - pos - 1)
  with
    Not_found -> ""


(* reverts slashes on Windows *)
let make_os_path name =
  match Sys.os_type with
    | "Win32" ->
        U.string_subst_char name '/' '\\'
    | _ -> name


(* find piqi file in search paths given its (relative) splitted name *)
let find_piqi_file ?(extra_paths=[]) modname =
  let dir_name, base_name = Piqi_name.split_name modname in
  let dir_name =
    match dir_name with
      | None -> ""
      | Some x -> make_os_path x (* reverse slashes on Windows *)
  in
  let found_dir = ref "" and found_name = ref "" (* results references *)
  in
  let check_exact_file dir_name base_name ext =
    let file_name = Filename.concat dir_name base_name ^ ext in
    let full_file_path = Filename.concat !found_dir file_name in
    trace "trying to locate module at %s\n" (U.quote full_file_path);
    if Sys.file_exists full_file_path
    then (found_name := file_name; true)
    else false
  in
  let check_file dir_name ext =
    (* try to find the exact match first *)
    if check_exact_file dir_name base_name ext
    then true
    else
      (* flip underscores to dashes or the other way around and try again
       * (modnames can't contain both underscores and dashes at the same time)
       *)
      if String.contains base_name '_'
      then
        let base_name = Piqi_util.underscores_to_dashes base_name in
        check_exact_file dir_name base_name ext
      else if String.contains base_name '-'
      then
        let base_name = Piqi_util.dashes_to_underscores base_name in
        check_exact_file dir_name base_name ext
      else false
  in
  let check_exact_directory base_dir dir_name =
    let dir = Filename.concat base_dir dir_name in
    trace "trying to locate module directory at %s\n" (U.quote dir);
    let directory_exists =
      if Sys.os_type = "Unix"
      then
        try Sys.is_directory dir with Sys_error _ -> false
      else
        (* for some reason, the above method doesn't work for mingw-based builds
         * when they are called from cygwin environment *)
        true
    in
    if directory_exists then found_dir := base_dir;
    directory_exists
  in
  let find_directory base_dir =
    if check_exact_directory base_dir dir_name
    then Some dir_name
    else
      (* if there's no exact directory, handle handle directory normalization
       * when all '_' are converted to '-':  change underscores to dashes and
       * try again *)
      if String.contains dir_name '_'
      then
        let dir_name = U.underscores_to_dashes dir_name in
        if check_exact_directory base_dir dir_name
        then Some dir_name
        else None
      else None
  in
  let find_file base_dir =
    match find_directory base_dir with
      | None -> false
      | Some dir_name ->
          List.exists (fun ext -> check_file dir_name ext) [".piqi"; ".proto.piqi"]
  in
  if List.exists find_file (extra_paths @ !Piqi_config.paths)
  then
    !found_dir, !found_name
  else
    (trace "piqi module is not found in path: %s\n" (U.quote modname);
     raise Not_found
    )


let find_piqi modname =
  (* NOTE: now supporting only local namespace *)
  let dir, fname = find_piqi_file modname in
  Filename.concat dir fname

