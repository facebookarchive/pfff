open Common

open Ast_php

module Ast = Ast_php
module Db = Database_php
module V = Visitor_php

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* In addition to flags that can be tweaked via -xxx options (cf the
 * full list of options in the "the options" section below), this 
 * program also depends on external files ?
 *)

let metapath = ref "/tmp/pfff_db"

(* for build_db *)
let phase = ref Database_php_build.max_phase

let index_method = ref false

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Some  debugging functions *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main action, build the database *)
(*****************************************************************************)

let main_action xs = 
  match xs with
  | [dir] -> 

      let dir = 
        (try Common.realpath dir
        with exn ->
          pr2 "note: could not find realpath";
          dir
        )
        +> Common.chop_dirsymbol 
      in
      let prj = Database_php.Project (dir, None) in
      let prj = Database_php.normalize_project prj in 

      let db = 
        Database_php_build.create_db
          ~db_support:(Database_php.Disk !metapath)
          ~phase:!phase
          ~annotate_variables_program:
           (Some Check_variables_php.check_and_annotate_program)
          prj 
      in
      if !index_method then Database_php_build2.index_db_method db;

      Database_php.close_db db;
      ()
  | x::y::ys ->
      raise Todo

  | [] -> raise Impossible

(* see also facebook/thrift/main_server.ml *)
let layers = [
  "layer_deadcode.json", (fun dir db layerfile ->
    let hooks = Deadcode_php.default_hooks in
    Layer_deadcode_php.gen_layer ~hooks ~db ~output:layerfile;
  );
  "layer_bugs.json", (fun dir db layerfile ->
    
    (* mostly copy paste of main_scheck_heavy.ml *)
    let files = Lib_parsing_php.find_php_files_of_dir_or_files [dir] in
    let errors = ref [] in
    let find_entity = Some (Database_php_build.build_entity_finder db) in
    let env = Env_php.mk_env dir in

    files +> Common.index_list_and_total +> List.iter (fun (file, i, total) ->
      try 
        pr2 (spf "processing: %s (%d/%d)" file i total);
        Check_all_php.check_file ~find_entity env file;
      with 
      | (Timeout | UnixExit _) as exn -> raise exn
      | exn ->
          Common.push2 (spf "PB with %s, exn = %s" file 
                           (Common.string_of_exn exn)) errors;
    );
    !errors +> List.iter pr2;
    Layer_checker_php.gen_layer ~root:dir ~output:layerfile !Error_php._errors
  );
]

let gen_layers metapath =
  Db.with_db ~metapath (fun db ->
    let dir = Db.path_of_project_in_database db in
    pr2 (spf "generating layers for %s in %s" dir metapath);

    layers +> List.iter (fun (layerfile, f) ->
      f dir db (Filename.concat metapath layerfile);
    );
  )

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

let deadcode_analysis db =

  let hooks = { Deadcode_php.default_hooks with
    Deadcode_php.print_diff = false;
    Deadcode_php.with_blame = false;
  }
  in
  let dead_ids_func = Deadcode_php.finding_dead_functions hooks db in
  let dead_ids_class = Deadcode_php.finding_dead_classes hooks db in
  pr2 (spf "total dead functions = %d, total dead classes = %d"
          (List.length dead_ids_func) (List.length dead_ids_class));
   ()


(*****************************************************************************)
(* Prolog db *)
(*****************************************************************************)

(* todo: move in prolog_db_php.ml in foundation/ ? *)
module EC = Entity_php

(* quite similar to Db.complete_name_of_id *)
let name_id id db =
  try 
    let s = db.Db.defs.Db.id_name#assoc id in
    let id_kind = db.Db.defs.Db.id_kind#assoc id in

    (match id_kind with
    | EC.Method | EC.StaticMethod 
    | EC.ClassConstant | EC.ClassVariable 
    | EC.XhpDecl
      ->
        (match Db.class_or_interface_id_of_nested_id_opt id db with
        | Some id_class -> 
            let sclass = Db.name_of_id id_class db in
            (match id_kind with
            | EC.Method ->       spf "('%s','%s')" sclass s
            | EC.StaticMethod -> spf "('%s','%s')" sclass s
            (* todo? something special ? *)
            | EC.ClassConstant | EC.ClassVariable 
            | EC.XhpDecl 
              -> spf "('%s','%s')" sclass s

            | _ -> raise Impossible
            )
        | None ->
            failwith (spf "could not find enclosing class for %s"
                    (Db.str_of_id id db))
        )

    | EC.StmtList -> spf "'__TOPSTMT__%s'" (EC.str_of_id id)
    | EC.Interface | EC.Class | EC.Function -> spf "'%s'" s
    (* ?? *)
    | EC.IdMisc -> spf "'__IDMISC__%s'" (EC.str_of_id id)
    )
  with Not_found -> 
    failwith (spf "could not find name for id %s" (Db.str_of_id id db))
      
let string_of_id_kind = function
  | EC.Function -> "function"
  (* todo? merge class/interface too? *)
  | EC.Class -> "class"
  | EC.Interface -> "interface"

  (* the static/1 predicate will say if static method (or class var) *)
  | EC.Method | EC.StaticMethod -> "method"

  | EC.ClassConstant -> "constant"
  | EC.ClassVariable -> "field"
  | EC.XhpDecl -> "xhpDecl"

  | EC.StmtList  -> "stmtlist"

  | EC.IdMisc -> "idmisc"

let string_of_modifier = function
  | Public    -> "is_public"  
  | Private   -> "is_private" 
  | Protected -> "is_protected"
  | Static -> "static"  | Abstract -> "abstract" | Final -> "final"

let gen_prolog_db db file =
  Common.with_open_outfile file (fun (pr, _chan) ->
   let pr s = pr (s ^ "\n") in
   pr ("%% -*- prolog -*-");
   pr (spf "%% facts about %s" (Db.path_of_project_in_database db));
   
   pr (":- discontiguous kind/2, at/3.");
   pr (":- discontiguous static/1, abstract/1, final/1.");
   pr (":- discontiguous arity/2.");
   pr (":- discontiguous is_public/1, is_private/1, is_protected/1.");
   pr (":- discontiguous extends/2, implements/2.");
   pr (":- discontiguous docall/3.");

   db.Db.file_info#tolist +> List.iter (fun (file, _parsing_status) ->
     let file = Db.absolute_to_readable_filename file db in
     let parts = Common.split "/" file in
     pr (spf "file('%s', [%s])." file
            (parts +> List.map (fun s -> spf "'%s'" s) +> Common.join ","));
   );

   db.Db.defs.Db.id_kind#tolist
   +> (fun xs -> Common_extra.execute_and_show_progress2 (List.length xs) 
      (fun k -> xs +> List.iter (fun (id, kind) ->
        k();
        pr (spf "kind(%s, %s)." (name_id id db) (string_of_id_kind kind));
        pr (spf "at(%s, '%s', %d)." 
               (name_id id db) 
               (Db.readable_filename_of_id id db)
               (Db.line_of_id id db)
        );
        (* note: variables can also be static but for prolog we are
         * interetested in a coarser grain level.
         * 
         * todo: refs, types for params?
         *)
        let ast = Db.ast_of_id id db in

        let add_callgraph () =
          let h = Hashtbl.create 101 in
  
          let visitor = V.mk_visitor { V.default_visitor with
            V.klvalue = (fun (k,vx) x ->
              match Ast.untype x with
              | FunCallSimple (callname, args) ->
                  let str = Ast_php.name callname in
                  if not (Hashtbl.mem h str)
                  then begin
                    Hashtbl.replace h str true;
                    pr (spf "docall(%s, '%s', 'function')." (name_id id db) str)
                  end;
                  k x

              | StaticMethodCallSimple(_, name, args)
              | MethodCallSimple (_, _, name, args)
              | StaticMethodCallVar (_, _, name, args)
                ->
                  let str = Ast_php.name name in
                  (* use a different namespace than func? *)
                  if not (Hashtbl.mem h str)
                  then begin
                    Hashtbl.replace h str true;
                    pr (spf "docall(%s, '%s', 'method')." (name_id id db) str)
                  end;

                  k x
              | _ -> k x
            );
            V.kexpr = (fun (k, vx) x ->
              match Ast.untype x with
              | New (_, classref, args)
              | AssignNew (_, _, _, _, classref, args) ->
                  (match classref with
                  | ClassNameRefStatic x ->
                      (match x with
                      | ClassName name ->

                          let str = Ast_php.name name in
                          (* use a different namespace than func? *)
                          if not (Hashtbl.mem h str)
                          then begin
                            Hashtbl.replace h str true;
                            pr (spf "docall(%s, '%s', 'class')." 
                                   (name_id id db) str)
                          end;
                          
                      (* todo: do something here *)
                      | Self _
                      | Parent _
                      | LateStatic _ ->
                          ()
                      )
                  | ClassNameRefDynamic _ -> ()
                  );
                  k x
              | _ -> k x
            );
          }
          in
          visitor (Entity ast);
        in


        (match kind, ast with
        | EC.Function, FunctionE def ->
            pr (spf "arity(%s, %d)." (name_id id db)
             (List.length (def.f_params +> Ast.unparen +> Ast.uncomma_dots)));
            add_callgraph();

        | EC.Class, ClassE def ->
            (match def.c_type with
            | ClassAbstract _ -> pr (spf "abstract(%s)." (name_id id db))
            | ClassFinal _ -> pr (spf "final(%s)." (name_id id db))
            | ClassRegular _ -> ()
            );
            def.c_extends +> Common.do_option (fun (tok, x) ->
              pr (spf "extends(%s, '%s')." (name_id id db) (Ast.name x));
            );
            def.c_implements +> Common.do_option (fun (tok, interface_list) ->
              interface_list +> Ast.uncomma |> List.iter (fun x ->
              pr (spf "implements(%s, '%s')." (name_id id db) (Ast.name x));
              )
            );

        | EC.Interface, InterfaceE def ->
            def.i_extends +> Common.do_option (fun (tok, interface_list) ->
              interface_list +> Ast.uncomma |> List.iter (fun x ->
              pr (spf "extends(%s, '%s')." (name_id id db) (Ast.name x));
              )
            )
            
        | (EC.Method | EC.StaticMethod), MethodE def -> 
            pr (spf "arity(%s, %d)." (name_id id db)
             (List.length (def.m_params +> Ast.unparen +> Ast.uncomma_dots)));
            def.m_modifiers +> List.iter (fun (m, _) -> 
              pr (spf "%s(%s)." (string_of_modifier m) (name_id id db));
            );
            add_callgraph();

        | EC.ClassVariable, ClassVariableE (var, ms) ->
            ms +> List.iter (fun (m) -> 
              pr (spf "%s(%s)." (string_of_modifier m) (name_id id db))
            )
        | EC.ClassConstant, _ -> ()
        | EC.XhpDecl, _ -> ()
            
        | (EC.StmtList | EC.IdMisc), _ ->
            add_callgraph();

        | _ -> raise Impossible
        )
      ));
   );
   db.Db.uses.Db.includees_of_file#tolist +> List.iter (fun (file1, xs) ->
     let file1 = Db.absolute_to_readable_filename file1 db in
     xs +> List.iter (fun file2 ->
       let file2 = 
         try Db.absolute_to_readable_filename file2 db 
         with Failure _ ->
           file2
       in
       pr (spf "include('%s', '%s')." file1 file2)
     );
   );
  )
    
let pfff_extra_actions () = [
  "-gen_layers", " <metapath>",
  Common.mk_action_1_arg gen_layers;
  "-deadcode_analysis", " <metapath>",
  Common.mk_action_1_arg (fun metapath ->
    Database_php.with_db ~metapath (fun db -> deadcode_analysis db));
  "-gen_prolog_db", " <metapath> <file>",
  Common.mk_action_2_arg (fun metapath file ->
    Database_php.with_db ~metapath (fun db -> gen_prolog_db db file));
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  pfff_extra_actions() ++
  Database_php_build.actions() ++
  (* Layer_checker is in main_scheck.ml *)
  Layer_deadcode_php.actions () ++
  Layer_coverage.actions () ++
  Layer_xhprof.actions () ++
  Layer_cyclomatic_php.actions () ++
  Layer_vcs.actions () ++
  []

let options () = 
  [
    "-metapath", Arg.Set_string metapath, 
    "<dir> (default=" ^ !metapath ^ ")";
    "-phase", Arg.Set_int phase,
    " <phase number>";
    "-index_method", Arg.Set index_method,
    " ";
  ] ++
  Common.options_of_actions action (all_actions()) ++
  Flag_parsing_php.cmdline_flags_verbose () ++
  Flag_parsing_php.cmdline_flags_debugging () ++
  Common.cmdline_flags_devel () ++
  Common.cmdline_flags_verbose () ++
  Common.cmdline_flags_other () ++
  [
    "-version",   Arg.Unit (fun () -> 
      pr2 (spf "pfff db (console) version: %s" Config.version);
      exit 0;
    ), 
    "  guess what";

    (* this can not be factorized in Common *)
    "-date",   Arg.Unit (fun () -> 
      pr2 "version: $Date: 2008/10/26 00:44:57 $";
      raise (Common.UnixExit 0)
    ), 
    "   guess what";
  ] ++
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 

  Common_extra.set_link();
  (* let argv = Features.Distribution.mpi_adjust_argv Sys.argv in *)
  Database_php_storage.set_link();

  let usage_msg = 
    "Usage: " ^ basename Sys.argv.(0) ^ 
      " [options] <file or dir> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () -> 
    
    (match args with
    
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) -> 
        Common.do_action !action xs (all_actions())

    | _ when not (Common.null_string !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 
        main_action (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> 
        Common.usage usage_msg (options()); 
        failwith "too few arguments"
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
    main ();
  )
