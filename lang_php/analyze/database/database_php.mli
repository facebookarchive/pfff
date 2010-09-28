(*s: database_php.mli *)

(* 
 * The goal of database_php.ml is to give access to information computed
 * by a set of global static or dynamic analysis such as what are the
 * set of callers to a certain function, what is the test coverage of
 * a file.
 * 
 * The main type is 'database' defined below.
 *)

(* The database of code that we will build usually refers to a set of 
 * source files belonging to a common project.
 *)
type project = 
  Project of 
    (* having a base directory allows when dealing with many files to 
     * sometimes display those files relative to the root of the project *)
    Common.dirname * 
    (* name of project *)
    string option 

(* Most PHP entities such as functions, classes, methods are referenced
 * through an id in the database of PHP code defined below.
 *)
type id     = Entity_php.id 

(* The PHP entities can also be unambiguously referred via their 
 * location in the code, e.g. a/b/foo.php:45. The 'fullid' type represents such
 * a location. It's also possible from an id to get its corresponding
 * fullid, see the fullid_of_id assoc in the database below.
 *)
type fullid = Entity_php.fullid 

type id_kind   = Entity_php.id_kind
type id_string = string

(* aliases *)
type id_function = id
type id_class = id
type id_interface = id
type id_method = id
type id_define = id

(* Remaining ids, e.g. serie of toplevel statements. See ast_entity_php.mli  *)
type id_other = id 

(* can be useful in gui to highlight part of text *)
type range_pos = int * int (* charpos x charpos *)

(* ---------------------------------------------------------------------- *)

(* The PHP code database *)
type database = {

  (* used for instance in GUI to display files relative to their 
   * project base, that is for readable paths. *)
  project: project;

  db_support: db_support;

  mutable next_free_id: int;

  (* opti: *)
  fullid_of_id: (id, fullid) Oassoc.oassoc;
  id_of_fullid: (fullid, id) Oassoc.oassoc;

  (* The filename are stored in absolute path format. This is true 
   * for all other references of filenames below except when indicated. 
   * The ids are sorted in order of apperance in the file.
   *)
  file_to_topids: (Common.filename, id list) Oassoc.oassoc;
  file_info: (Common.filename, file_info) Oassoc.oassoc;
  
  (* return Not_found for toplevel ids *)
  enclosing_id: (id, id) Oassoc.oassoc;
  (* method ids are inside other ids (class ids). Same for nested functions *)
  children_ids: (id, id list) Oassoc.oassoc;

  defs: database_defs;
  uses: database_uses;

  (* symbols, to build completion list in gui for instance (faster than defs) *)
  symbols: (string, unit) Oassoc.oassoc; 

  strings: (string, unit) Oassoc.oassoc; 

  (* IO *)
  flush_db: unit -> unit;
  close_hook: unit -> unit;
}
   and database_defs = {
     (* asts *)
     toplevels:   (id, Ast_php.toplevel) Oassoc.oassoc;

     (* NEVER USE THIS FIELD directly. Use the ast_of_id helper function! *)
     asts: (id, Ast_entity_php.id_ast) Oassoc.oassoc;

     (* consider also using toks_of_topid_of_id wrapper func *)
     str_of_topid:    (id, string)         Oassoc.oassoc;
     tokens_of_topid: (id, Parser_php.token list) Oassoc.oassoc;
     range_of_topid: (id, (Parse_info.parse_info * Parse_info.parse_info)) 
       Oassoc.oassoc;

     (* Not all ids have a name; for instance StmtList ASTs 
      * really dont have one (well for now I gave them a __TOPSTMT__ name).
      * So 'id_name' could be a partial assoc. Same for 'id_kind' 'id_phpname'.
      *
      * for 'id_name', see also name_of_id() helper.
      *)
     name_defs: (id_string, id list) Oassoc.oassoc;

     id_kind: (id, id_kind)   Oassoc.oassoc;

     (* computed statically or dynamically *)
     id_type: (id, Type_php.phptype) Oassoc.oassoc;

     (* shortcut, to avoid getting the ast to get the name of the entity *)
     id_name: (id, id_string) Oassoc.oassoc;
     id_phpname: (id, Ast_php.name) Oassoc.oassoc;

     extra: (id, extra_id_info) Oassoc.oassoc;
   }
   and database_uses = {
    callees_of_f: (id, Callgraph_php.callsites_opt list) Oassoc.oassoc;
    callers_of_f: (id, Callgraph_php.callersinfo_opt list) Oassoc.oassoc; 

    (* see also Class_php.static_new_of_ast for the opposite *)
    users_of_class: (id_class, id list) Oassoc.oassoc;
    users_of_define: (id_define, id list) Oassoc.oassoc;

    extenders_of_class:        (id_class,     id_class list) Oassoc.oassoc;
    implementers_of_interface: (id_interface, id_class list) Oassoc.oassoc;

    (* This does mention only the direct includers or includees. 
     * The filenames again are stored in absolute path format.
     *)
    includers_of_file: (Common.filename, Common.filename list) Oassoc.oassoc;
    includees_of_file: (Common.filename, Common.filename list) Oassoc.oassoc;
   }

  and file_info = {
    parsing_status: [`OK | `BAD];
  }
  and extra_id_info = {
    tags: Annotation_php.annotation list;
    partial_callers: bool;
    partial_callees: bool;
    todo: unit;
  }
 and db_support = 
  | Disk of Common.dirname (* the path to the Berkeley DB meta data *)
  | Mem


(* ---------------------------------------------------------------------- *)
type error = 
  | ErrorDb of string
val report_error: error -> string

exception Error of error

(* ---------------------------------------------------------------------- *)
(* note that the open and closing of the database is now in 
 * database_backend.ml to make database_php.ml independent
 * of berkeley DB
 *
 * 
 * note: create_db is in database_php_build.mli 
 *)
exception DatabaseAlreadyLocked

val check_db: database -> unit
val open_db_mem: project -> database
val close_db: database -> unit

val _current_open_db_backend: (Common.dirname -> database) ref
val with_db:  metapath:Common.dirname -> (database -> 'a) -> 'a

(* ---------------------------------------------------------------------- *)
(* xxx_of_id *)
(* ---------------------------------------------------------------------- *)

val name_of_id:  id -> database -> string
(* put also the classname when the id is a method *)
val complete_name_of_id: id -> database -> string

val filename_of_id:          id -> database -> Common.filename
val readable_filename_of_id: id -> database -> Common.filename

val line_of_id: id -> database -> int
val col_of_id: id -> database -> int
(* for debugging *) 
val str_of_id:      id -> database -> string

val kind_of_id: id -> database -> id_kind

val ast_of_id:           id -> database -> Ast_entity_php.id_ast
val toks_of_topid_of_id: id -> database -> Parser_php.token list

val is_top_id: id -> database -> bool
val is_function_id: id -> database -> bool


(* ---------------------------------------------------------------------- *)
(* Entities relationships *)
(* ---------------------------------------------------------------------- *)

(* works for function and methods *)
val callees_of_id: id -> database -> Callgraph_php.callsite list
val callers_of_id: id_function -> database -> Callgraph_php.callerinfo list

val class_users_of_id: id_class -> database -> id list

(* inheritance tree *)
val parent_name_of_id: id_class -> database -> string option

val class_extenders_of_id: id_class -> database -> id_class list
val class_implementers_of_id: id_interface -> database -> id_class list

val classdef_of_nested_id_opt: id -> database -> Ast_php.class_def option

(* !!Use memoization so can be called many times for all the files
 * in a database without being too slow. But that means it does
 * side effects so take care!!
 * 
 * The filenames are in absolute path format.
 *)
val includees_rec_of_file: 
  Common.filename -> database -> Common.filename list
val includers_rec_of_file: 
  Common.filename -> database -> Common.filename list

val includees_graph_of_file:
  ?depth_limit: int option ->
  Common.filename -> database -> Common.filename Graph.graph
val includers_graph_of_file:
  ?depth_limit: int option ->
  Common.filename -> database -> Common.filename Graph.graph


(* ---------------------------------------------------------------------- *)
(* id_of_xxx *)
(* ---------------------------------------------------------------------- *)

(* the ids returned should correspond to the order of appearance in the 
 * file. *)
val ids_in_file: Common.filename -> database -> id list

val ids_with_kind__of_string: string -> database -> (id * id_kind) list 

(* Entity accessor. The function returns a list of id because
 * some entities, e.g. function foo, can be defined multiple
 * times in a project.
 *)
val filter_ids_of_string: string -> id_kind -> database -> id list

(* Shortcuts to filter_ids_of_string. *)
val function_ids__of_string: string -> database -> id_function list
val method_ids_of_string:    string -> database -> id_method list
val class_ids_of_string:     string -> database -> id_class list
val interface_ids_of_string: string -> database -> id_interface list

(* Shortcuts when we know the entity is unambiguous *)
val id_of_function: string -> database -> id_function
val id_of_class:    string -> database -> id_class
val id_of_interface:    string -> database -> id_interface
val id_of_method:   theclass:string -> string -> database -> id_method

val static_function_ids_of_strings: 
  theclass:string -> string -> database -> id list


(* get all entities *)
val filter_ids_in_db: 
  id_kind list -> database -> (string, id list) Common.assoc

val functions_in_db: database -> (string, id_function list) Common.assoc
val classes_in_db: database -> (string, id_class list) Common.assoc
val methods_in_db: database -> (string, id_method list) Common.assoc

val functions_or_static_methods_in_db: 
  database -> (string, id list) Common.assoc


val id_of_phpname:   Ast_php.name -> database -> id

val id_of_kind_call: 
  ?file_disambiguator:Common.filename ->
  Callgraph_php.kind_call -> database -> id

(* ---------------------------------------------------------------------- *)
(* misc *)
(* ---------------------------------------------------------------------- *)

(* This is about recursing on nested entities, like functions inside 
 * functions or methods inside classes. This is not about inheritance
 * but about "enclosedness".
 *)
val recurse_children: (id -> unit) -> database -> id -> unit

(* The returned list starts by the id itself and
 * then its direct children and so on.
 *)
val all_children_ids: id -> database -> id list

(* Opposite of children. The returned list starts by the id itself and
 * then its enclosing and enclosing and so on.
 *)
val enclosing_ids: id -> database -> id list


val all_files:
  database -> Common.filename list

val has_parsing_problem: 
  Common.filename -> database -> bool

(* ---------------------------------------------------------------------- *)
(* file/path *)
(* ---------------------------------------------------------------------- *)
val path_of_project_in_database: database -> Common.dirname

val path_of_project:             project -> Common.dirname
val glimpse_metapath_of_database: database -> Common.dirname
val default_metapath_of_project:  project -> Common.dirname
val normalize_project: project -> project
val database_tag_filename: string
val check_is_database_dir: Common.dirname -> unit

val default_extra_id_info : extra_id_info

val absolute_to_readable_filename: 
  Common.filename -> database -> Common.filename
val readable_to_absolute_filename: 
  Common.filename -> database -> Common.filename

(* ---------------------------------------------------------------------- *)
val actions: unit -> Common.cmdline_actions
(*x: database_php.mli *)
(*e: database_php.mli *)
