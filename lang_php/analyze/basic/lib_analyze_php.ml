(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

open Ast_php

module Flag = Flag_analyze_php
module Ast = Ast_php

module V = Visitor_php
module V2 = Visitor2_php

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* In command line tools like git or mercurial, many operations works 
 * when a file, a set of files, or even dirs are passed as parameters.
 * We want the same with pfff, hence this small helper function that
 * transform such files_or_dirs into a flag set of filenames.
 *)
let find_php_files files_or_dirs = 
  let files = 
    Common.files_of_dir_or_files_no_vcs_nofilter files_or_dirs in
  files +> List.filter (fun filename ->
    let valid = 
      (filename =~ ".*\\.php$") ||
        (filename =~ ".*\\.phpt$") ||
        Lib_parsing_php.is_php_script filename
    in
    if not valid && !Flag.verbose_database
    then pr2 ("not analyzing: " ^ filename);
    valid
  )


(*****************************************************************************)
(* Extract infos *)
(*****************************************************************************)

(* very close to lib_parsing_php.ml *)

let extract_info_visitor recursor = 
  let globals = ref [] in
  let hooks = { V.default_visitor with
    V.kinfo = (fun (k, _) i -> Common.push2 i globals)
  } in
  begin
    let vout = V2.mk_visitor hooks in
    recursor vout;
    !globals
  end

let ii_of_id_ast x = 
  extract_info_visitor (fun visitor -> visitor.V2.vid_ast x)

