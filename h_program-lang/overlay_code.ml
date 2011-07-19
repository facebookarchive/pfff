(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * Some code organization are really bad. But because it's harder
 * to convince people to change it, sometimes it's simpler to create
 * a parallel organization, an "overlay" using simple symlinks
 * that represent a better organization. One can then show
 * statistics on those overlayed code organization, adapt layers,
 * etc
 *)

(*****************************************************************************)
(* Check consistency *)
(*****************************************************************************)

let check_overlay ~dir_orig ~dir_overlay =
  let dir_orig = Common.realpath dir_orig in
  let files = 
    Common.files_of_dir_or_files_no_vcs_nofilter [dir_orig] in

  let dir_overlay = Common.realpath dir_overlay in
  let links = 
    Common.cmd_to_list (spf "find %s -type l" dir_overlay) in

  let links = links +> List.map Common.realpath in
  let files2 = 
    links +> List.map (fun file_or_dir -> 
      Common.files_of_dir_or_files_no_vcs_nofilter [file_or_dir]
    ) +> List.flatten
  in
  pr2 (spf "#files orig = %d, #links overlay = %d, #files overlay = %d"
    (List.length files) (List.length links) (List.length files2)
  );
  let h = Hashtbl.create 101 in
  files2 +> List.iter (fun file ->
    if Hashtbl.mem h file
    then pr2 (spf "this one is a dupe: %s" file);
    Hashtbl.add h file true;
  );

  let (common, only_in_orig, only_in_overlay) = 
    Common.diff_set_eff files files2 in


  only_in_orig +> List.iter (fun l ->
    pr2 (spf "this one is missing: %s" l);
  );
  only_in_overlay +> List.iter (fun l ->
    pr2 (spf "this one is gone now: %s" l);
  );
  if not (null only_in_orig && null only_in_overlay)
  then failwith "Overlay is not OK"
  else pr2 "Overlay is OK"
