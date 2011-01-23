(*s: deadcode_php.mli *)

type hooks = {
  (* to remove certain false positives *)
  is_probable_dynamic_funcname: string -> bool;

  (* to avoid generating patches for code which does not have a valid 
   * git owner anymore
   *)
  is_valid_author: string -> bool;

  (* to avoid generating patches for certain files, such as code in 
   * third party libraries or auto generated code
   *)
  is_valid_file: Common.filename -> bool;

  false_positive_deadcode_annotations: Annotation_php.annotation list;

  (* config *)
  print_diff: bool;
  with_blame: bool;

 (* deadcode_analysis may generate some .git_annot files in your 
  * source directory to speedup things. *)
  cache_git_blame: bool;
  (* place where we would put the generated patches *)
  patches_path: Common.dirname;
  
 (* patch generation can be long, can focus on certain files first *)
 skip_patch_generation_for_file: Common.filename -> bool;

}
val default_hooks: hooks

type dead_ids_by_file = 
  (Common.filename * (string * Database_php.fullid * Database_php.id) list) list

type deadcode_patch_info = {
  file     : Common.filename; (* relative to the project *)
  reviewer : string option;
  cc       : string option;
  date     : Common.date_dmy;
}


(* main entry point. Will generate data in hooks.patches_path *)
val deadcode_analysis: hooks -> Database_php.database -> unit

(* internal analysis functions *)
val finding_dead_functions:
  hooks -> Database_php.database -> (string * Database_php.id) list
val finding_dead_classes:
  hooks -> Database_php.database -> (string * Database_php.id) list

val deadcode_fixpoint_per_file:
  Database_php.id list (* original set of dead ids *) -> 
  hooks -> Database_php.database ->
  dead_ids_by_file

(* path where resides all deacode patches *)
val deadcode_stat: Common.dirname -> unit
val deadcode_patch_info: Common.filename -> deadcode_patch_info

(* helpers *)
val group_ids_by_file: 
  Database_php.id list -> Database_php.database -> dead_ids_by_file
val ungroup_ids_by_file: 
  dead_ids_by_file -> Database_php.id list

val false_positive: 
  Database_php.id -> hooks -> Database_php.database -> bool


(*x: deadcode_php.mli *)
(*e: deadcode_php.mli *)
