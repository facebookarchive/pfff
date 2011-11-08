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

open Ast_php

module Ast = Ast_php
module EC = Entity_php
module Db = Database_php
module V = Visitor_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

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

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

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
