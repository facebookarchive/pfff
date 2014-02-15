(*s: ui_search.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2010-2012 Facebook
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
(*e: Facebook copyright *)
open Common

module G = Gui
module M = Model2

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * todo:
 *  - integrate lfs at some point!
 *  - integrate pof at some point!
 *)


(* ---------------------------------------------------------------------- *)
(* Search *)
(* ---------------------------------------------------------------------- *)

(*s: dialog_search_def *)
let dialog_search_def model = 
  let idx = (fun () -> 
    let model = Async.async_get model in
    model.Model2.big_grep_idx 
  )
  in
  let entry = 
    Completion2.my_entry_completion_eff 
      ~callback_selected:(fun _entry _str _file _e ->
        true
      )
      ~callback_changed:(fun _str ->
        ()
      )
      idx
  in

  let res =
    G.dialog_ask_generic ~title:"" 
      (fun vbox -> 
        vbox#pack (G.with_label "search:" entry#coerce);
      )
      (fun () -> 
        let text = entry#text in 
        pr2 text;
        text
      )
  in
  res +> Common.do_option (fun s -> 
    pr2 ("selected: " ^ s);
  );
  res
(*e: dialog_search_def *)

(*s: run_grep_query *)
let run_grep_query ~root s =
  (* --cached so faster ? use -w ?  
   * -I means no search for binary files
   * -n to show also line number
   *)
  let git_grep_options = 
    "-I -n"
  in
  let cmd = 
    spf "cd %s; git grep %s %s" root git_grep_options s
  in
  let xs = Common.cmd_to_list cmd in
  let xs = xs +> List.map (fun s ->
    if s =~ "\\([^:]*\\):\\([0-9]+\\):.*"
    then
      let (filename, lineno) = Common.matched2 s in
      let lineno = s_to_i lineno in
      let fullpath = Filename.concat root filename in
      fullpath, (M.Line (lineno - 1))
    else 
      failwith ("wrong git grep line: " ^ s)
  ) in
  xs
(*e: run_grep_query *)

(*s: run_tbgs_query *)
let run_tbgs_query ~root s =
  let cmd = 
    spf "cd %s; tbgs --stripdir %s" root s
  in
  let xs = Common.cmd_to_list cmd in
  let xs = xs +> List.map (fun s ->
    if s =~ "\\([^:]*\\):\\([0-9]+\\):.*"
    then
      let (filename, lineno) = Common.matched2 s in
      let lineno = s_to_i lineno in
      let fullpath = Filename.concat root filename in
      fullpath, (M.Line (lineno - 1))
    else 
      failwith ("wrong tbgs line: " ^ s)
  ) in
  xs
(*e: run_tbgs_query *)

(*e: ui_search.ml *)
