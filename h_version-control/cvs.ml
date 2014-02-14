(* Yoann Padioleau
 * 
 * Copyright (C) 2009 Yoann Padioleau
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
(* Commands *)
(*****************************************************************************)
(* ex: 
1.3          (pjd      30-Dec-05): extern u_int vm_memguard_divisor;
*)

let annotate_regexp = 
  "^\\([0-9]+\\(\\.[0-9]+\\)+\\)" ^ "[ \t]+" ^ 
  "(\\([^ \t]+\\)" ^ "[ \t]+" ^
  "\\([0-9]+\\)" ^ "-" ^
  "\\([A-Za-z]+\\)" ^ "-" ^
  "\\([0-9]+\\)" ^ "):" ^ 
  ".*$" (* the rest is line of code *)

(*
let _ = assert ("1.208        (imp      07-Jan-05): /*-" =~ annotate_regexp)
*)


let annotate2 ?(basedir="") filename = 

  let today = Common2.today () in
  let dmy = today +> Common2.floattime_to_unixtime +> Common2.unixtime_to_dmy in
  let (Common2.DMY (_,_,Common2.Year this_year)) = dmy in

  (* TODO????: compute it from file directly ? *)
  (* ??? let date = "-D \"12 Feb\"" in *)
  let date = "-D \"1 Sep\"" in
  Common.pr2_once ("using Date for CVS:" ^ date);


  (* bugfix: for eclipse files can have space and $ in filename *)
  let cmd = 
    spf "%s cvs annotate %s '%s' 2>&1" 
    (Lib_vcs.goto_dir basedir) date filename in
  (* pr2 cmd; *)
  let xs = Common.cmd_to_list cmd in

  (* have to get rid of header
     
     Annotations for memguard.h
     ***************
  *)

  (*let ys = Common.cat (Common.filename_of_db (basedir,filename)) in*)

  let annots = 
    xs +> Common.map_filter (fun s -> 
      match () with
      | () when s =~ annotate_regexp -> 
        let (rcsid, _, author, day, month_str, year) = Common.matched6 s in
        (* 05 -> 2005 or 93 -> 1993 *)
        let iyear = s_to_i year in
        let iyear = if iyear < 60 then iyear + 2000 else iyear + 1900 in
        assert (iyear <= this_year);

        Some (Lib_vcs.VersionId rcsid,
              Lib_vcs.Author author,
              Common2.mk_date_dmy 
                (s_to_i day) 
                (Common2.int_of_month (Common2.month_of_string month_str))
                iyear)
      (* header *)
      | () when s = "***************" -> None
      | () when s =~ "^Annotations for.*" -> None
      | _ -> 
          pr2 ("cvs annotate wrong line: " ^ s);
          None
    ) 
  in
  (* files lines are 1_based, so add this dummy 0 entry *)
  Array.of_list (Lib_vcs.dummy_annotation::annots)


let annotate ?basedir a = 
  Common.profile_code "Cvs.annotate" (fun () -> annotate2 ?basedir a)

(* ------------------------------------------------------------------------ *)
let annotate_raw ?(basedir="") filename = 
  let date = "-D \"12 Feb\"" in


  let cmd = 
    spf "%s cvs annotate %s '%s' 2>&1" 
    (Lib_vcs.goto_dir basedir) date filename in

  (* pr2 cmd; *)
  let xs = Common.cmd_to_list cmd in

  (* have to get rid of header
     
     Annotations for memguard.h
     ***************
  *)

  (*let ys = Common.cat (Common.filename_of_db (basedir,filename)) in*)

  let annots = 
    xs +> Common.map_filter (fun s -> 
      if s =~ annotate_regexp 
      then 
        Some s
      else begin 
        (* pr2 ("cvs annotate wrong line: " ^ s); *)
        None
      end
    ) 
  in
  (* files lines are 1_based, so add this dummy 0 entry *)
  Array.of_list (""::annots)


(* ------------------------------------------------------------------------ *)
(* ex:
date: 2005/01/21 18:09:17;  author: bmilekic;  state: Exp;
*)

let date_regexp =  
  "date: " ^
  "\\([0-9]+\\)" ^ "/" ^ (* year *)
  "\\([0-9]+\\)" ^ "/" ^ (* month *)
  "\\([0-9]+\\)" ^ " " ^ (* day *)
  ".*"

(* eclipse have this format, wierd cos both use cvs *)
let date_regexp2 =  
  "date: " ^
  "\\([0-9]+\\)" ^ "-" ^ (* year *)
  "\\([0-9]+\\)" ^ "-" ^ (* month *)
  "\\([0-9]+\\)" ^ " " ^ (* day *)
  ".*"


let find_all_date cmd = 
  let xs = Common.cmd_to_list cmd in

  (*todo: use find_some *)
  let xs' = 
    xs +> Common.map_filter (fun s -> 
      if s=~ date_regexp
      then 
        let (year, month, day) = matched3 s in
        Some (
          Common2.mk_date_dmy 
            (s_to_i day) 
            (s_to_i month)
            (s_to_i year) 
        )

      else 
        if s=~ date_regexp2
        then 
          let (year, month, day) = matched3 s in
          Some (
            Common2.mk_date_dmy 
              (s_to_i day) 
              (s_to_i month)
              (s_to_i year) 
          )
      else None
    ) in
  xs'

let find_date cmd = 
  let xs' = find_all_date cmd in
  match xs' with
  | x::_xs -> Some x
  | [] -> None

let find_date_min cmd = 
  let xs' = find_all_date cmd in
  match xs' with
  | x::xs -> Some (Common2.minimum_dmy (x::xs))
  | [] -> None

let date_file_creation2 ?(basedir="") file = 
  let cmd = 
    spf "%s cvs log -r1.1 '%s' 2>&1" 
      (Lib_vcs.goto_dir basedir) file in
  let cmd2 = 
    spf "%s cvs log -r1.2 '%s' 2>&1" 
      (Lib_vcs.goto_dir basedir) file in
  let cmd_alt = 
    spf "%s cvs log '%s' 2>&1" 
      (Lib_vcs.goto_dir basedir) file in

  match find_date cmd with
  | Some x -> x
  | None -> 
      pr2 ("wierd: cvs cant find date revision 1.1");
      (match find_date cmd2 with
      | Some x -> x 
      | None -> 
          pr2 ("PB: cvs cant find date revision 1.1 or 1.2");
          (match find_date_min cmd_alt with
          | Some x -> x 
          | None -> 
              failwith ("PB: cvs cant find any file date ");
              

          )
      )

let date_file_creation ?basedir a = 
  Common.profile_code "cvs.date_file" (fun() -> date_file_creation2 ?basedir a)

(*****************************************************************************)

