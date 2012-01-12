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

(* for fields *)
open Ast_php
open Database_php

module Ast  = Ast_php
module Flag = Flag_analyze_php
module V = Visitor_php
module E   = Database_code
module Db = Database_php

open Database_php_build_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * We build the full database in multiple steps as some
 * operations need the information computed globally by the
 * previous step:
 * 
 * - we first add the top ASTs in the database (index_db1)
 * 
 * - we then add nested ASTs (methods, class vars, nested funcs), and their ids
 * - we add all strings found in the code in a global table
 * - before adding the callgraph, we need to 
 *   be able to decide given a function call what are the possible entities
 *   that define this function (there can be multiple candidates), so 
 *   we add some string->definitions (function, classes, ...) information
 *   (index_db2)
 * 
 * - then we do the callgraph
 * - we add other reversed index like the callgraph, but for other 
 *   entities than functions, e.g. the places where a class in instantiated,
 *   or inherited (index_db3)
 * 
 * - TODO given this callgraph we can now run a type inference analysis, as 
 *   having the callgraph helps doing the analysis in a certain order (from
 *   the bottom of the callgraph to the top, with some fixpoint in the 
 *   middle).
 * 
 * time to build the db on www: 
 *   - 15min in bytecode
 *   - 26min when storing the string and tokens
 *   - xxmin when storing method calls
 * cf also score.org !!!
 * 
 * TODO: can take value of _SERVER in params so can partially evaluate
 * things to statically find the file locations (or hardcode with ~/www ... ) 
 * or use a 'find' ...
 * 
 *    type php_config = ...
 *    type apache_config = ...
 *)

(*****************************************************************************)
(* Build_entity_finder *)
(*****************************************************************************)
(* See entity_php.mli for the rational behind having both a database
 * type and an entity_finder type.
 *)
let (build_entity_finder: database -> Entity_php.entity_finder) = fun db ->
 (fun (id_kind, s) ->
   try (
    match id_kind with
    | E.Class _ ->
        Db.class_ids_of_string s db 
        +> List.map (fun id -> Db.ast_of_id id db)
    | E.Function ->
        Db.function_ids__of_string s db 
        +> List.map (fun id -> Db.ast_of_id id db)
(*
    | Entity_php.StaticMethod ->
        if s =~ "\\(.*\\)::\\(.*\\)"
        then
          let (sclass, smethod) = Common.matched2 s in
          Db.static_function_ids_of_strings ~theclass:sclass smethod db
          +> List.map (fun id -> Db.ast_of_id id db)
        else
          failwith ("wong static method format: " ^ s)
*)
    | _ ->
        raise Todo
    )
    with exn ->
      if !Flag.show_analyze_error
      then 
        pr2 (spf "Entity_finder: pb with '%s', exn = %s" 
              s (Common.exn_to_s exn));
      raise exn
  )

(*****************************************************************************)
(* Build database intermediate steps *)
(*****************************************************************************)

let debug_index_db = ref true

(* ---------------------------------------------------------------------- *)
(* step1:  
 * - store toplevel asts
 * - store file to toplevel ids mapping
 *)
let index_db1_2 db files = 

  let nbfiles = List.length files in

  let pbs = ref [] in
  let parsing_stat_list = ref [] in

  files +> Common.index_list +> List.iter (fun (file, i) -> 
    pr2 (spf "PARSING: %s (%d/%d)" file i nbfiles);

    (* for undoing in case of pbs *)
    let all_ids = ref [] in

    try (Common.timeout_function 20 (fun () ->
        (* parsing, the important call *)
      let (ast2, stat) = Parse_php.parse file in
      let file_info = 
        { parsing_status = if stat.Parse_info.bad = 0 then `OK else `BAD; } in

      db.file_info#add2 (file, file_info);

      Common.push2 stat  parsing_stat_list;

      Common.profile_code "Db.add db1" (fun () ->
        ast2 +> List.iter (fun (topelem, info_item) -> 
          match topelem with
          (* bugfix: the finaldef have the same id as the previous item so
           * do not add it otherwise id will not be a primary key.
           *)
          | Ast.FinalDef _ -> ()
          (* Do we want to add the NotParsedCorrectly in the db ? Yes, it
           * can be useful in the code visualizer to have all
           * the elements in a file, including the one that do not
           * parse. Note that this id does not have a id_kind for now.
           *)
          | _ ->
              let topelem = Unsugar_php.unsugar_self_parent_toplevel topelem in
              let id = db +> add_toplevel2 file (topelem, info_item) in
              Common.push2 id all_ids;
        );
        db +> add_filename_and_topids (file, (List.rev !all_ids));
      );
      db.flush_db();
    ))
    with 
    | Out_of_memory  (*| Stack_overflow*) | Timeout
    | Parse_php.Parse_error _
    -> 
      (* Backtrace.print (); *)
      pr2 ("PB: BIG PBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB: " ^ file);
      pr2 ("Undoing addition");
      db +> add_filename_and_topids(file, []);
      !all_ids +> List.iter (fun id -> 
        try 
          let _ = db.defs.toplevels#assoc id in
          db.defs.toplevels#delkey id +> ignore;
          (* todo? del also the fullid and id info ? *)
        with Not_found -> ()
      );
  );
  !parsing_stat_list, !pbs

let index_db1 a b = 
  Common.profile_code "Db.index_db1" (fun () -> index_db1_2 a b)

(* ---------------------------------------------------------------------- *)
(* step2:  
 *  - add defs (string -> entity mapping)
 *  - add strings
 *  - add nested ASTs and nested ids
 *    (and also update fullid_of_id, extra and children_ids tables)
 *  - add classes, methods, variables
 *  - add extra information (e.g. the 'kind' of an id)
 *)
let index_db2_2 db =
  iter_files_and_topids db "ANALYZING2" (fun id file -> 
    let ast = db.defs.toplevels#assoc id in

    (* Extract all strings in the code. Can be used for instance by the
     * deadcode analyzer to remove some false positives if a function
     * is mentionned in a string.
     *)
    let strings = Lib_parsing_php.get_constant_strings_any (Toplevel ast) in
    strings +> List.iter (fun s ->
      db.strings#add2 (s, ());
    );
    (* add strings in not parsed correctly too *)
    (match ast with
    (* note: dead now that we don't do error_recovery *)
    | NotParsedCorrectly _infos -> 
        let toks = db.defs.tokens_of_topid#assoc id in
        toks +> List.iter (fun tok -> 
          match tok with
          | Parser_php.T_CONSTANT_ENCAPSED_STRING (s, _) ->
              db.strings#add2 (s, ());
          | Parser_php.T_IDENT(s, _) ->
              db.strings#add2 (s, ());
          | _ -> ()
        );
    | ( FinalDef _
      | ClassDef _| FuncDef _ | ConstantDef _
      | StmtList _)
        -> ()
    );
  (* let's add definitions and nested asts and entities *)
  (*
    let add_def_hook (s, kind) = db +> add_def (IdString s, kind, id) in
    let add_type_hook typ = db +> add_type (id, typ) in
    ~add_type:add_type_hook
  *)
    
   (* old: Definitions_php.visit_definitions_of_ast ~add_def:add_def_hook ast
    * we now need to add more information in the database (e.g. enclosing ids),
    * so we have to inline the visit_definitions code here and expand it
    *)
    let enclosing_id = ref id in

    (* Classes can be empty, and so children_ids will not get a chance to 
     * be populated in add_nested_id_and_ast, which can lead to some
     * Not_found when accessing those children in some of our algorithms.
     * It's simpler to have a children_ids that is always valid, and 
     * return [] (not Not_found) for empty classes, hence this code.
     *)
    db.children_ids#add2 (id, []);

    let hooks = { V.default_visitor with
      V.ktop = (fun (k, bigf) x ->
        match x with
        | FuncDef def -> 
            let s = Ast_php.name def.f_name in
            add_def (s, E.Function, id, Some def.f_name) db;
            (* add_type def.Ast_c.f_type; *)
            k x
        | StmtList stmt -> 
            let s = "__TOPSTMT__" in
            add_def (s, E.TopStmts, id, None) db;
            k x
        | ClassDef class_def ->
            let s = Ast_php.name class_def.c_name in
            let kind = Class_php.class_type_of_class class_def in
            add_def (s, E.Class kind, id, Some class_def.c_name) db;
            k x
        | ConstantDef def ->
            raise Todo
        | NotParsedCorrectly _ -> ()
            
        (* right now FinalDef are not in the database, because of possible 
         * fullid ambiguity 
         *)
        | FinalDef _ -> raise Impossible
      );

      (* todo?  could factorize more ... *)
      V.kstmt_and_def = (fun (k, bigf) x ->
        match x with
        | Stmt _ -> ()

        | FuncDefNested def ->
            let newid = add_nested_id_and_ast ~enclosing_id:!enclosing_id
              (Ast_php.FunctionE def) db in
            let s = Ast_php.name def.f_name in
            add_def (s, E.Function, newid, Some def.f_name) db;
            Common.save_excursion enclosing_id newid  (fun () -> k x);
            
        | ClassDefNested def ->
            let newid = add_nested_id_and_ast ~enclosing_id:!enclosing_id
              (Ast_php.ClassE def) db in
            let s = Ast_php.name def.c_name in
            let kind = Class_php.class_type_of_class def in
            add_def (s, E.Class kind, newid, Some def.c_name) db;
            Common.save_excursion enclosing_id newid (fun () -> k x);
      );
      V.kclass_stmt = (fun (k, bigf) x ->
        match x with
        | Method def ->
            let newid = add_nested_id_and_ast  ~enclosing_id:!enclosing_id
              (Ast_php.MethodE def) db in
            let s = Ast_php.name def.m_name in
            let kind =
              if Class_php.is_static_method def
              then E.StaticMethod
              else E.RegularMethod
            in
            (* todo? should we put just the method name, or also add
             * the class name for the StaticMethod case ? 
             *)
            add_def (s, E.Method kind, newid, Some def.m_name) db;
            Common.save_excursion enclosing_id newid (fun () -> k x);

        (* we generate one id per constant. Note that they can not have the 
         * same AST because then we would get some "fullid already in database"
         * error.
         *)
        | ClassConstants (tok1, class_constant_list, tok2) -> 
            class_constant_list +> Ast.uncomma +> List.iter (fun class_cst ->
              let (name, _affect) = class_cst in

              let newid = add_nested_id_and_ast ~enclosing_id:!enclosing_id
                (Ast_php.ClassConstantE(class_cst))
                db
              in
              let s = Ast.name name in
              add_def (s, E.ClassConstant, newid, None) db;
            );
            (* not sure we need to recurse. There can't be more definitions
             * inside class declarations.
             *)
            k x

        | ClassVariables (class_var_modifier, _opt_ty, 
                          class_variable_list, tok) ->
            class_variable_list +> Ast.uncomma +> List.iter (fun class_var ->
              let (dname, _affect) = class_var in

              let modifier = 
                match class_var_modifier with
                | NoModifiers _ -> []
                | VModifiers xs -> 
                    List.map fst xs
              in

              let newid = add_nested_id_and_ast ~enclosing_id:!enclosing_id
                (Ast_php.ClassVariableE(class_var, modifier)) db in

              (* old: was adding $, but not that useful for prolog,
               * and is actually not consistent with how we use
               * field (we do $this->fld, not $this->$fld).
               * Finally xhp attributes don't have a $ so it's again
               * more consistent to not add a $ here.
               *)
              let s = Ast.dname dname in
              add_def (s, E.Field, newid, None) db;
            );
            k x

        | XhpDecl decl ->
            (match decl with
            | XhpAttributesDecl (_, xs, _) ->
               xs +> Ast.uncomma +> List.iter (fun x ->
                 match x with
                 | XhpAttrDecl (_type, (s, tok), _affect_opt, _tokopt) ->
                     let newid = 
                       add_nested_id_and_ast ~enclosing_id:!enclosing_id
                         (Ast_php.XhpAttrE x) db 
                     in
                     add_def (s, E.Field, newid, None) db;
                 (* todo? *)
                 | XhpAttrInherit _ ->
                     ()
               );
            (* todo *)
            | XhpChildrenDecl _ | XhpCategoriesDecl _ -> ()
            )

        (* todo? *)
        | UseTrait _ ->
            ()
      );
    }
    in
    (V.mk_visitor hooks) (Toplevel ast)
  )

let index_db2 a = 
  Common.profile_code "Db.index_db2" (fun () -> index_db2_2 a)

(* ---------------------------------------------------------------------- *)
(* step3: 
 * - caller/callees global analysis, part 1 
 * - class instantiation reverse index (who creates object of class X)
 * - extends and implements reverse index (who extends/implements class X)
 * - TODO globals-used reverse index
 * 
 * Was before in index_db2. Prefer now to do this step in a separate phase
 * than index_db2. Before they were together but I found this coupling
 * not intellectually satisfactory and it turns out later that we needed
 * anyway to do things in sequence. 
 * Indeed we would later need in the caller/callees phase to 
 * have run one time index_db2 on all the files to get access to 
 * all the possible definitions of a function call string.  So now split
 * step in two; first add the type, struct def, macro def in db, 
 * and then do the first caller/callee. 
 * 
 * This sequencing also enable to know if the ident corresponds to a
 * macro or not. This can be useful. This also handles recursive calls
 * in an easy way.
 * This also enable to give a score of confidence to DirectCall
 * as for instance a call on a function f defined in the same file is
 * more probable than call to function with same name f in another file 
 * or directory.
 * 
 * Note that add_callees_of_f can be quite expansive if not optimized, 
 * especially when want to store all call instances and for each instances,
 * even for the directcalls, all the possible targets (because even for
 * directcalls can have ambiguities and multiple candidates).
 * 
 * Note that in PHP the type inference will work
 * better if we have the information on the function at the call site,
 * which mean that by doing the callgraph analysis first, and do 
 * some topological sort, we can more easily do a type inference analysis.
 *)
let index_db3_2 db = 

  (* how assert no modif on those important tables ? *)
  iter_files_and_ids db "ANALYZING3" (fun id file -> 
    let ast = Db.ast_of_id id db in
    (* bugfix: was calling Unsugar_php.unsugar_self_parent_entity ast
     * here but it's too late because an entity can be a nested id
     * which does not have an enclosing class_def to set the classname.
     * So the unsugaring must be done in phase 1.
     *)
    let idcaller = id in

    (* the regular function calls sites
     * todo: if the entity is a class, then right now we will consider
     * any calls inside its method. We used to use Visitor2 which was
     * not visiting the class_statements of a class but now that we
     * removed it, we visit everything. Not sure if it's an issue.
     *)
    let callees = Callgraph_php.callees_of_any (Entity ast) in

    (* TODO: actually when have parent::foo it does not mean it's
     * a static method. It could be a regular inherited public/protected 
     * method. Should use class_php?
     *)
    let static_method_callees = 
      Callgraph_php.static_method_callees_of_any (Entity ast) in

    db +> add_callees_of_id (idcaller,  callees ++ static_method_callees);

    (* the new, X::, extends, etc *)
    let classes_used = users_of_class_in_any (Entity ast) in
    let candidates = 
      classes_used +> List.map Ast.name +> Common.set 
      +> Common.map_flatten (fun s -> class_ids_of_string s db)
    in
    candidates +> List.iter (fun idclass -> 
      db.uses.users_of_class#apply_with_default idclass
        (fun old -> id::old) (fun() -> []) +> ignore
    );

    (* the extends and implements *)
    (match ast with
    | Ast.ClassE def ->
        let idB = id in
        def.c_extends |> Common.do_option (fun (tok, classnameA) ->
         (* we are in a situation like: class B extends A *)

          let s = Ast.name classnameA in
          (* There may be multiple classes defining classnameA and 
           * so multiple ids. 
           * 
           * todo: at some point we would like a better
           * scope/file analysis so that there is no ambiguity.
           *)
          let candidates = class_ids_of_string s db in
          candidates |> List.iter (fun idA -> 
            db.uses.extenders_of_class#apply_with_default idA
              (fun old -> idB::old) (fun() -> []) +> ignore
          );
        );

        def.c_implements |> Common.do_option (fun (tok, interface_list) ->
          interface_list |> Ast.uncomma |> List.iter (fun interfacenameA ->
            (* we are in a situation like: class B implements A *)

            let _s = Ast.name interfacenameA in
            let candidates =  []
              (* TODO? 
              interface_ids_of_string s db 
              *)
            in
            candidates |> List.iter (fun idA -> 
              db.uses.implementers_of_interface#apply_with_default idA
                (fun old -> idB::old) (fun() -> []) +> ignore
            );
          );
        );
        
    | Ast.MiscE _
    | Ast.ClassVariableE _  | Ast.ClassConstantE _
    | Ast.XhpAttrE _
    | Ast.MethodE _
    | Ast.StmtListE _
    | Ast.FunctionE _ 
    | Ast.ConstantE _ 
      ->  ()
    );
    (*
      let global_used = Relation_c.globals_of_ast ~also_def:false ast in
      db +> add_globals_in_toplevel (id, global_used);
      let structs_used = Relation_c.structnames_of_ast ~also_def:false ast in
      db +> add_structs_in_toplevel (id, structs_used);
      let fields_used = Relation_c.fields_used_of_ast ~also_def:false ast in
      db +> add_fields_in_toplevel (id, fields_used);
      let typedefs_used = Relation_c.globals_of_ast ~also_def:false ast in
      db +> add_typedefs_in_toplevel (id, typedefs_used);
    *)
    ()
  )

let index_db3 a = 
  Common.profile_code "Db.index_db3" (fun () -> index_db3_2 a)

(* ---------------------------------------------------------------------- *)
(* step4:
 *  - tags annotations for functions
 *  - local/global annotations
 *)
let index_db4_2 ~annotate_variables_program db = 

  (* todo: those mutual dependency between entity_finder and build_db is ugly *)
  let find_entity = build_entity_finder db in
  let msg = "ANALYZING4" in
  iter_files db (fun ((file, ids), i, total) -> 
   pr2 (spf "%s: %s %d/%d " msg file i total);
    
    let asts = ids +> List.map (fun id -> db.defs.toplevels#assoc id) in

    (*Check_variables_php.check_and_annotate_program  *)
    annotate_variables_program +> Common.do_option 
      (fun annotate_variables_program ->
        annotate_variables_program
          (Some (fun x ->
            (* we just want to annotate here *)
            try 
              find_entity x 
            with Multi_found ->
              raise Not_found
          ))
          asts;
      );
    (* store back the AST *)
    zip ids asts +> List.iter (fun (id, ast) ->
      db.defs.toplevels#add2 (id, ast);
    );

   ids +> List.iter (fun id ->
    let ast = db.defs.toplevels#assoc id in
    
    (* tags, like @phpsh *)
    let toks = db.defs.tokens_of_topid#assoc id in
    let comment_opt = first_comment ast toks in 

    let tags = ref [] in
    comment_opt +> Common.do_option (fun info ->
      let str = info.Parse_info.str in
      let comment_tags = 
        try Annotation_php.extract_annotations str 
        with exn ->
          pr2_err (spf "PB: EXTRACT ANNOTATION: %s on %s" 
                  (Common.exn_to_s exn) file);
          []
      in
      tags := comment_tags;
    );

    let callees = Lib_parsing_php.get_funcalls_any (Toplevel ast) in
    (* facebook specific ... *)
    
    if List.mem "THIS_FUNCTION_EXPIRES_ON" callees
    then Common.push2 Annotation_php.Have_THIS_FUNCTION_EXPIRES_ON tags;

    let extra = db.defs.extra#assoc id in
    let extra' = { extra with
      tags = !tags;
    } in

    db.defs.extra#add2 (id, extra');
   );
  );
  ()

let index_db4 ~annotate_variables_program a = 
  Common.profile_code "Db.index_db4" (fun () -> 
    index_db4_2 ~annotate_variables_program a)

(*****************************************************************************)
(* create_db *)
(*****************************************************************************)
let max_phase = 4

let create_db 
    ?(verbose_stats=true)
    ?(db_support=Db.Mem)
    ?(phase=max_phase) 
    ?(files=None) 
    ~annotate_variables_program
    prj  
 = 
  let prj = normalize_project prj in 

  let db = 
    match db_support with
    | Disk metapath ->
        let metapath = 
          if metapath = ""
          then Database_php.default_metapath_of_project prj
          else metapath
        in
        (*
          if (Sys.file_exists (Filename.concat metapath "notes.txt"))
          then failwith "there is a notes.txt file";
        *)
        if not (Common.command2_y_or_no("rm -rf " ^ metapath))
        then failwith "ok we stop";
        
        Common.command2("mkdir -p " ^ metapath);
        Common.command2(spf "touch %s/%s" metapath 
                           Database_php.database_tag_filename);
        Common.write_value prj (metapath ^ "/prj.raw");

        (* berkeley DB usually *)
        let db = !Database_php._current_open_db_backend metapath in

        let logchan = open_out (metapath ^ "/log.log") in
        Common._chan_pr2 := Some logchan;
        db
    (* note that is is in practice less efficient than Disk :( because
     * probably of the GC that needs to traverse more heap cells
     *)
    | Mem -> 
        open_db_mem prj
  in
  begin
    let files = 
      match files with
      | None ->
          let dir = path_of_project db.project in
          (* note that if dir is a symlink, files would be an empty list,
           * especially as we chop the / in path_of_project, so add an extra "/"
           * 
           * bugfix: some file like scripts/update_database are php files
           * but do not end in .php ... so not enough to just use 
           * 
           *      let ext = ".*\\.\\(php\\|phpt\\)$" in
           *)
          Lib_parsing_php.find_php_files_of_dir_or_files [(dir ^ "/")]
      | Some xs -> xs
    in

    let _nbfiles = List.length files in
    (* does not work well on Centos 5.2 *)
    (* Common.check_stack_nbfiles nbfiles;  *)

    (* ?default_depth_limit_cpp *)
    let parsing_stats, bigpbs = 
      index_db1 db files
    in
    db.flush_db();

    if phase >= 2 then index_db2 db;
    db.flush_db();
    if phase >= 3 then index_db3 db;
    db.flush_db();

    if phase >= 4 then index_db4 ~annotate_variables_program db;
    db.flush_db();

    if verbose_stats && !Flag.show_analyze_error then begin
      (* Parsing_stat.print_stat_numbers (); *)
      Parse_info.print_parsing_stat_list   parsing_stats;
      !_errors +> List.iter pr2;
    end;
    db
  end

(*****************************************************************************)
(* Fast db construction *)
(*****************************************************************************)

(* The goal here is to build a database containing enough context, enough
 * relevant files, to be able to do interesting global analysis on a file
 * passed as a parameter to one of our command line program.
 * 
 * The idea is to build a database with all the files included
 * by the file. In a way it is similar to what gcc does when it calls
 * 'cpp' to get the full information for a file.
 * 
 * todo: see facebook/fb_common/www_db_build.ml for now
 * todo: factorize all the db_of_files_or_dirs out there.
 *)
let fast_create_db_mem_a_la_cpp ?phase files_or_dirs =
  raise Todo

(*****************************************************************************)
(* Create db shortcuts *)
(*****************************************************************************)

(* The default way to analyze a set of PHP files is to first build
 * a database containing information about the code (stored internally
 * using Berkeley DB), e.g. with ./pfff_db ~/www -metapath /tmp/pfff_db,
 * and then run different analysis on this database, e.g. with
 * ./pfff_misc -deadcode_analysis /tmp/pfff_db.
 * In our testing code we want to test some of our analysis without
 * requiring to have a directory with a set of files, or some space on
 * disk to store the database. This small wrapper allows to build
 * a database in memory from a give set of files, usually temporary
 * files built with tmp_php_file_from_string() below.
 *)
let db_of_files_or_dirs ?(annotate_variables_program=None) files_or_dirs =
  let php_files =
    Lib_parsing_php.find_php_files_of_dir_or_files files_or_dirs
    +> List.map Common.relative_to_absolute
  in
  (* prj is normally used in GUI to display files relative to a specific
   * project base. Here we want to analyze a set of adhoc files or multiple
   * dirs so there is no base so we use /
   *)
  Common.save_excursion Flag.verbose_database false (fun () ->
  create_db
    ~db_support:Database_php.Mem
    ~files:(Some php_files)
    ~annotate_variables_program
    ~verbose_stats:false
    (Database_php.Project ("/", None))
  )

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)
let actions () = [
  (* no -create_db as it is offered as the default action in 
   * main_db,ml
   *)
(*
  "-index_db1", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname index_db1);
*)
  "-index_db2", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname index_db2);
  "-index_db3", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname index_db3);
]
