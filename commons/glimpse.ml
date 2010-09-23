open Common

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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* history: 
 * was first used for LFS, then a little for cocci, and then for aComment 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type glimpse_search = 
  (* -i insensitive search *)
  | GlimpseCaseInsensitive
  (* -w match on complete words. But not always good idea, for instance 
   * if file contain chazarain_j then dont work with -w 
   *)
  | GlimpseWholeWord

let default_glimpse_search = [GlimpseWholeWord] 

let s_of_glimpse_search = function
  | GlimpseCaseInsensitive -> "-i"
  | GlimpseWholeWord -> "-w" 


type glimpsedir = Common.dirname

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let check_have_glimpse () = 
  let xs = 
    Common.cmd_to_list ("glimpse -V") +> Common.exclude Common.null_string in
  (match xs with
  | ["This is glimpse version 4.18.2, 2006."] -> ()
  | ["This is glimpse version 4.18.5, 2006."] -> ()
  | _ -> failwith "glimpse not found or bad version"
  )

let s_of_glimpse_options xs = 
  xs +> List.map s_of_glimpse_search +> Common.join " "


(*****************************************************************************)
(* Indexing *)
(*****************************************************************************)

(*  
 * note:
 *  - -o or -b for glimpseindex => bigger index, faster search   
 *  - no need to use -b with our way to use glimpse 
 *    cos we use -l so dont need to know what is the place of the word 
 *    in the file
 *  - -f is for incremental indexing. Handle when files are deleted ? 
 *    I think that not that bad cos yes certainly in the index there will
 *    have some  no-more-valid pointers, but as glimpse actually then do
 *    a real search on the file, he will see that dont exist anymore and
 *    so using -f is slower but very very little slower 
 *  - for -z the order is important in .glimpse_filters => put
 *    the case of compressed file first 
 *  - -F  receive  the list of files to index from stdin
 *  - -H target index dir
 *  - -n for indexing numbers as sometimes some glimpse request are looking
 *    for a number
 * 
 * 
 * Note que glimpseindex index pas forcement tous les fichiers texte. 
 * Si le fichier texte est trop petit, contient par exemple un seul mot, 
 * alors il l'indexe pas. Si veut indexer quand meme, il faudrait ajouter 
 * l'option -E
 * 
 * command2 "echo '*_backup'   > glimpse/.glimpse_exclude";
 * command2 "echo '*_backup,v' >> glimpse/.glimpse_exclude";
 * 
 * ex: glimpseindex -o -H . home 
 * 
 *)
let glimpse_cmd s = spf "glimpseindex -o -H %s -n -F" s

let glimpseindex ext dir indexdir = 
  check_have_glimpse ();
  Common.command2(spf "mkdir -p %s" indexdir);
  Common.command2 
    (spf "find %s -name \"*.%s\" | %s"
        dir ext (glimpse_cmd indexdir)
    );
  ()


let glimpseindex_files files indexdir = 
  check_have_glimpse ();
  Common.command2(spf "mkdir -p %s" indexdir);

  let tmpfile = Common.new_temp_file "glimpse" "list" in
    (* "/tmp/pad_glimpseindex_files.list" *)
  
  Common.uncat files tmpfile;
  Common.command2 
    (spf "cat %s | %s" tmpfile (glimpse_cmd indexdir));
  ()


(*****************************************************************************)
(* Searching *)
(*****************************************************************************)


(* note:
 *  - -y dont ask for prompt
 *  - -N allow far faster search as it does not actually search the file 
 *    => when pdf/ps files no filtering done of them => far faster.
 *    the -N fait pas un grep, donc si file deteled ou modified entre temps, 
 *    bah il le voit pas. Ca veut dire aussi que si y'a pas -N, et bien
 *    glimpse fait des grep si le fichier a ete modifié entre temps pour
 *    toujours filer quelque chose de valide (pas de false positive, mais 
 *    y'a quand meme peut etre des miss). Est ce qu'il utilise la date du
 *    fichier pour eviter de faire des grep inutile ?
 *    the -N can actually return wrong result. cos a file may 
 *    contain "peter norvig"
 *    => better to not use -N at first 
 * 
 *  - -N also just show the filename on output
 *  - -l show just the filename too, but the files are still searched so
 *    at least no false positives.
 *  - if use -z for glimpseindex, dont forget the -z too for glimpse 
 *  - -W for boolean and queries to not be done on line level but file level
 * 
 *  query langage: good;bad  for conjunction.   good,bad for disjunction.
 * 
 *  ex: glimpse -y -H . -N -W -w pattern;pattern2
 * 
 *)
let glimpse query ?(options=default_glimpse_search) dir = 
  let str_options = s_of_glimpse_options options in
  let res = 
    Common.cmd_to_list 
      (spf "glimpse -y -H %s -N -W %s '%s' 2>/dev/null" dir str_options query) in
  res

(* grep -i -l -I  *)
let grep query = 
  raise Todo


(*
check_have_position_index

let glimpseindex_position: string -> ... (filename * int) list
let glimpse_position: string -> ... (filename * int) list
*)
