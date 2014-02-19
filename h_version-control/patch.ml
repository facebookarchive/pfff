(* Yoann Padioleau
 * 
 * Copyright (C) 2006 Yoann Padioleau
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * history:
 *  Was first written for coccinelle, for patchparse experiment.
 *
 * 
 * note: diff can have some ambuiguities ? ex:
 * file1:
 * 
 * -- /home/pad/week-end/cocci-c/txt/file1	2006-02-16 11:32:00.000000000 +0100
 * 
 * file2:
 * 
 * -- titi
 * 
 * => Diff: 
 * 
 * diff -u -p -b -B -r /home/pad/week-end/cocci-c/txt/file1 /home/pad/week-end/cocci-c/txt/file2
 * --- /home/pad/week-end/cocci-c/txt/file1	2006-02-16 11:32:38.000000000 +0100
 * +++ /home/pad/week-end/cocci-c/txt/file2	2006-02-16 11:32:07.000000000 +0100
 * @@ -1 +1 @@
 * --- /home/pad/week-end/cocci-c/txt/file1	2006-02-16 11:32:00.000000000 +0100
 * +-- titi
 * \ No newline at end of file
 * 
 * ambiguity ?  no because there is a preceding diff line command
 *
 * 
 * 
 * todo:
 * in fact fragile, cf txt/diff_format.txt,  
 * "^--- " is not enough for a regexp,  there must be just before a diff command
 * todo?: does it handle when a file was just created/deleted ? 
 * --- a/drivers/acorn/char/defkeymap-acorn.c_shipped	Sat Jun 14 12:18:56 2003
 * +++ /dev/null	Wed Dec 31 16:00:00 1969
 * does it need to ? (I think no, after all, if deleted or created, there is no
 * interesting patch info to print)
 * 
 * in main.ml: 
 * 
 * let classic_patch_file = ref ""
 * "-classic_patch_file", Arg.Set_string classic_patch_file, 
 * " the patch file corresponding to the linux version we are analyzing"     ;
 * ] in 
 * 
 * | "test_parse_classic_patch", [] -> 
 * Classic_patch.parse_patch (cat "/tmp/patch1") +> ignore
 * 
 * | "test_filter_driver", [] ->  
 * cat "/home/pad/kernels/patches/patch-2.5.71"
 * +> Classic_patch.filter_driver_sound 
 * +> List.iter pr2
 * 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type patch_raw = string list

(* parsing patch related *)
type patchinfo = (filename, fileinfo) Common.assoc
   and fileinfo = ((int * int) * hunk) list
   (* inv: the regions are sorted, because we process patch from start to 
    * end of file *)
   and hunk = string list (* use parse_hunk() to get patchline *)
  (* with tarzan *)

(* the strings are without the mark *)
type patchline = 
  | Context of string
  | Minus of string
  | Plus of string


let _mark_regexp = "^[-+ ]"
let regexp_mark_and_line = "^\\([-+ ]\\)\\(.*\\)"
let str_regexp_no_mark = "^[-+ ]\\(.*\\)"

type stat = {
  mutable nb_minus: int;
  mutable nb_plus: int
}

let verbose = false

(* generating patch related *)
type edition_cmd = 
  | RemoveLines of int list
  | PreAddAt of int * string list
  | PostAddAt of int * string list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let remove_prefix_mark s = 
  if s =~ str_regexp_no_mark
  then matched1 s
  else begin
    pr2 ("no mark in: " ^ s);
    s
  end

let parse_hunk xs =
  xs +> List.map (fun s ->
    if s=~ regexp_mark_and_line
    then
      let (mark, rest_line) = Common.matched2 s in
      (match mark with
      | " " -> Context rest_line
      | "-" -> Minus rest_line
      | "+" -> Plus rest_line
      | _ -> raise Impossible
      )
    else
      failwith ("wrong format for a hunk: " ^ s)
  )


let rec mark_file_boundary_and_normalize_with_xxx xs =
  match xs with
  | [] -> []
  | x::xs ->
      if x =~ "^diff .*" 
      then
        let has_minus_and_plus =
          match xs with
          | y::z::_xs ->
              y =~ "--- \\(.*\\)" &&
              z =~ "\\+\\+\\+ \\(.*\\)"
          | _ -> false
        in
        if has_minus_and_plus
        then
          (match xs with
          | y::z::xs ->

              (* ugly *)
              let _ = y =~ "--- \\(.*\\)" in
              let s_y = matched1 y in
              let _ = z =~ "\\+\\+\\+ \\(.*\\)" in
              let s_z = matched1 z in
              if s_y = "/dev/null"
              then ("xxx " ^ s_z)::mark_file_boundary_and_normalize_with_xxx xs
              else ("xxx " ^ s_y)::mark_file_boundary_and_normalize_with_xxx xs
          | _ -> raise Impossible
          )
        else begin
          match xs with
          | x::xs when x =~ "Binary files .*" ->
              mark_file_boundary_and_normalize_with_xxx xs
          | _ ->
              if verbose 
              then pr2 ("weird, diff header but no modif: " ^ x);
              mark_file_boundary_and_normalize_with_xxx (xs)
        end
      else
        x::mark_file_boundary_and_normalize_with_xxx xs

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let (parse_patch: (string list) -> patchinfo) = fun lines ->

  (* dont need those lines any more *)

  (* old:
   * let lines = lines +> List.filter (fun s -> 
   * not (s =~ "^\\-\\-\\-" || s =~ "^diff"))    
   *  in
   * but this does not handle well patches that remove or mv files which
   * will contain some +++ /dev/null or ---/dev/null so just normalize it
   *)
  let lines = mark_file_boundary_and_normalize_with_xxx lines in

  (* note: split_list_regexp can generate __noheading__ category 
   * (cf common.ml code) 
   *)
  let double_splitted = 
    lines 
      +> Common2.split_list_regexp "^xxx "  
      +> List.map (fun (s, group) -> 
          (s, Common2.split_list_regexp "^@@" group) 
         )
  in

  double_splitted +>         
     List.map (fun (s, group) -> 
       if s =~ "^xxx [^/]+/\\([^ \t]+\\)[ \t]?"
       then

       (* old:
        s =~ "^\\-\\-\\- [^/]+/\\([^/]+\\)/\\(.*\\)/\\([^ \t]+\\)[ \t]"
         +> (fun b -> if not b then (pr2 s; pr2 (dump group); assert b));
          let (driver_or_sound, subdirs, filename) = matched3 s in
         assert drivers|sound    
          driver_or_sound ^ "/" ^ subdirs ^ "/" ^ filename, 
       *)
        let (file) = matched1 s in
        file, 
        group +>
          List.map (fun (s, group) -> 
            match () with
            | _ when 
                  s =~ "^@@ \\-\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@" ->
                let (start1, plus1, _start2, _plus2) = matched4 s in
                let (start1, plus1) = Common2.pair s_to_i (start1, plus1) in
                ((start1, start1 + plus1), 
                (* just put the +- into the string ? *)
                group
                )
                  
            (* [("@@ -1 +0,0 @@", ["-"])] *)
            | _ when 
                  s =~ "^@@ \\-\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@" ->
                let (start1, _start2, _plus2) = matched3 s in
                let (start1, plus1) = Common2.pair s_to_i (start1, "0") in
                ((start1, start1 + plus1), 
                (* just put the +- into the string ? *)
                group
                )

            (* @@ -0,0 +1 @@ *)
            | _ when 
                  s =~ "^@@ \\-\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\) @@" ->
                let (start1, plus1,  _start2) = matched3 s in
                let (start1, plus1) = Common2.pair s_to_i (start1, plus1) in
                ((start1, start1 + plus1), 
                (* just put the +- into the string ? *)
                group
                )

            (* @@ -1 +1 @@ *)
            | _ when
                  s =~ "^@@ \\-\\([0-9]+\\) \\+\\([0-9]+\\) @@" ->
                let (start1, _start2) = matched2 s in
                let (start1) = s_to_i start1 in
                ((start1, start1), 
                (* just put the +- into the string ? *)
                group
                )
                  
            | _ -> failwith ("pb with hunk: " ^ s)

          )
       else failwith ("pb with line: " ^ s)
     )

(*****************************************************************************)
(* Hunk processing *)
(*****************************************************************************)

let hunk_containing_string s (pinfos: patchinfo) = 
  pinfos +> Common.find_some (fun (_file, fileinfo) -> 
    Common2.optionise (fun () -> 
      fileinfo +> Common.find_some (fun (_limits, hunk) -> 
        let hunk' = hunk +> List.map remove_prefix_mark in
        if List.mem s hunk'
        then Some hunk
        else None
      ))
  )

let hunks_containing_string s (pinfos: patchinfo) = 
  pinfos +> Common.map_filter (fun (_file, fileinfo) -> 
    let res = 
      (fileinfo +> Common.map_filter (fun (_limits, hunk) -> 
        let hunk' = hunk +> List.map remove_prefix_mark in
        if List.mem s hunk'
        then Some hunk
        else None
      ))
    in
    if null res then None else Some res
  ) +> List.flatten


let modified_lines fileinfo = 
  fileinfo +> List.map (fun ((start, _end), hunk) ->
    let hunk = parse_hunk hunk in
    let noplus = hunk +> Common.exclude (function Plus _ -> true | _ -> false)
    in
    Common.index_list noplus +> Common.map_filter (function
    | Minus _, idx -> Some (idx + start)
    | _ -> None
    )
  ) +> List.flatten

(*****************************************************************************)
(* Extra func *)
(*****************************************************************************)


let diffstat_file finfo = 
  let plus = ref 0 in
  let minus = ref 0 in
  
  finfo +> List.iter (fun (_, hunk) ->
    let plines = parse_hunk hunk in
    plines +> List.iter (function
    | Context _ -> ()
    | Minus _ -> incr minus
    | Plus _ -> incr plus
  );
  );
  { nb_minus = !minus;
    nb_plus = !plus;
  }

let diffstat pinfo = 
  pinfo +> List.map (fun (file, fileinfo) ->
    file, diffstat_file fileinfo
  )

let string_of_stat stat = 
  spf "nb plus = %d, nb minus = %d" stat.nb_plus stat.nb_minus

(*****************************************************************************)
(* misc *)
(*****************************************************************************)

 
let (relevant_part: (filename * (int * int)) -> patchinfo -> string) = 
 fun (_filename, (_startl, _endl)) _patchinfo ->
   raise Todo
(*
  try 
    let xs = patchinfo#find filename in
    let is_in i (min, max) = i >= min && i <= max in
    xs +> map_filter (fun ((i,j), s) -> 
      if ((is_in i (startl, endl)) || 
          (is_in j (startl, endl)) || 
          (i < startl && j > endl))
      then Some s 
      else None
      ) 
      +> String.concat "\nOTHER REGION\n" 

  with Not_found -> ("NO MODIF in patch file for:" ^ filename)
*)

let (_filter_driver_sound: string list -> string list) = fun _lines -> 
  raise Todo

(*
  let res = ref [] in

  let state = ref 0 in

  begin
  lines +> List.iter (fun s -> 
    if s =~ "^\\-\\-\\- .*" then state := 0;

    (* GET ALSO Documentation,  include/*(except asm-  ?   futur=fs, net *)
    (* could filter .h too,  newfile *)
    if s =~ "^\\-\\-\\- [^/]+/\\([^/]+\\)/\\(.*\\)/\\([a-zA-Z0-9_\\-]+\\.[a-zA-Z0-9_]+\\)[ \t]"
    then
       let (driver_or_sound, subdirs, filename) = matched3 s in
       if driver_or_sound = "drivers" || driver_or_sound = "sound"
       then begin state := 1; push2 s res end
       else state := 0
    else 
      if s =~ "^\\-\\-\\- .*" then state := 0
      else 
        if !state = 1 then push2 s res
        else ()
            );
   List.rev !res
  end
*)


(*****************************************************************************)
(* Patch generation *)
(*****************************************************************************)

(* 
 * This is sligtly complicated. Modifying files is not that easy. 
 * A far easier path is to use Emacs and do macros. In most cases it 
 * is easier than programming the modif in OCaml. I don't have 
 * enough helper functions for such things. Nevertheless it is not
 * always practical to run emacs macros from a script so this is a crude
 * but possible alternative.
 * 
 * Could use 'ed' as sgimm suggested, or make a small OCaml API around 'ed'.
 *)
let (generate_patch: 
     edition_cmd list -> filename_in_project:string -> Common.filename ->
     string list) = 
 fun edition_cmds ~filename_in_project filename ->

   let indexed_lines = 
     Common.cat filename +> Common.index_list_1 in
   
   let indexed_lines = 
     edition_cmds +> List.fold_left (fun indexed_lines edition_cmd ->
     match edition_cmd with
     | RemoveLines index_lines -> 
         indexed_lines
         +> Common.exclude (fun (_line, idx) -> List.mem idx index_lines)
     | PreAddAt (lineno, lines_to_add) 
     | PostAddAt (lineno, lines_to_add) ->
         let lines_to_add_fake_indexed = 
           lines_to_add +> List.map (fun s -> s, -1) in

         indexed_lines +> Common2.map_flatten (fun (line, idx) ->
           if idx = lineno 
           then 
             (match edition_cmd with
             | PreAddAt _ -> lines_to_add_fake_indexed @ [line, idx]
             | PostAddAt _ -> [line, idx] @ lines_to_add_fake_indexed
             | _ -> raise Impossible
             )
           else [line, idx]
         )
     ) indexed_lines
   in
   let lines' = indexed_lines +> List.map fst in
                                    
   (* generating diff *)
   let tmpfile = Common.new_temp_file "genpatch" ".patch" in
   write_file ~file:tmpfile (Common2.unlines lines');
   (* pr2 filename_in_project; *)
   let (xs, _) = 
     Common2.cmd_to_list_and_status 
       (spf "diff -u -p  \"%s\" \"%s\"" filename tmpfile) 
   in
   
   let xs' = xs +> List.map (fun s ->
     match () with
     | _ when  s =~ "^\\-\\-\\- \\([^ \t]+\\)\\([ \t]?\\)" ->
         let (_, rest) = matched2 s in 
         "--- a/" ^ filename_in_project ^ rest
     | _ when  s =~ "^\\+\\+\\+ \\([^ \t]+\\)\\([ \t]?\\)" ->
         let (_, rest) = matched2 s in 
         "+++ b/" ^ filename_in_project ^ rest
     | _ -> s
   )
   in
   xs'

(*****************************************************************************)
(* Debug *)
(*****************************************************************************)
