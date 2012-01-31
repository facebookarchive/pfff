(* Yoann Padioleau
 * 
 * Copyright (C) 2011, 2012 Facebook
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
module E = Database_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * This module makes it possible to ask questions on the structure of
 * a PHP codebase, for instance: "What are all the children of class Foo?".
 * It is inspired by a similar tool for java called JQuery
 * (http://jquery.cs.ubc.ca/).
 * 
 * todo:
 *  - precise callgraph, using julien's pathup/pathdown tools
 *  - types, refs
 *  - ??
 * 
 * For more information look at h_program-lang/database_code.pl
 * and its many predicates.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* quite similar to Database_php.complete_name_of_id *)
let name_id id db =
  try 
    let s = db.Db.defs.Db.id_name#assoc id in
    let id_kind = db.Db.defs.Db.id_kind#assoc id in

    (match id_kind with
    | E.Method _ | E.ClassConstant | E.Field ->
        (match Db.class_or_interface_id_of_nested_id_opt id db with
        | Some id_class -> 
            let sclass = Db.name_of_id id_class db in
            (match id_kind with
            | E.Method _ ->       spf "('%s','%s')" sclass s

            (* todo? xhp decl ? *)
            | E.Field ->
                (* old: remove the $ because in use-mode we don't use the $ 
                 * update: now def don't have a $ too, and can be xhp
                 * attributes too.
                *)
                spf "('%s','%s')" sclass s

            | E.ClassConstant -> spf "('%s','%s')" sclass s
            | _ -> raise Impossible
            )
        | None ->
            failwith (spf "could not find enclosing class for %s"
                    (Db.str_of_id id db))
        )

    | E.TopStmts -> spf "'__TOPSTMT__%s'" (EC.str_of_id id)
    | E.Class _ | E.Function | E.Constant -> spf "'%s'" s
    (* ?? *)
    | E.Other s -> spf "'__IDMISC__%s'" (EC.str_of_id id)

    | (E.MultiDirs|E.Dir|E.File | E.Macro|E.Global | E.Type|E.Module) ->
        (* not in db for now *)
        raise Impossible
    )
  with Not_found -> 
    failwith (spf "could not find name for id %s" (Db.str_of_id id db))
      
(* quite similar to database_code.string_of_id_kind *)
let string_of_id_kind = function
  | E.Function -> "function"
  | E.Constant -> "constant"
  | E.Class x -> 
      (match x with
      | E.RegularClass -> "class"
      | E.Interface -> "interface"
      | E.Trait -> "trait"
      )
  (* the static/1 predicate will say if static method (or class var) *)
  | E.Method _ -> "method"

  (* could also put 'constant' here as the pair of (class,cst) will already
   * differentiate it from regular constants.
   *)
  | E.ClassConstant -> "class_constant"
  | E.Field -> "field"

  | E.TopStmts  -> "stmtlist"
  | E.Other _ -> "idmisc"
  | (E.MultiDirs|E.Dir|E.File|E.Macro|E.Global|E.Type|E.Module) ->
      raise Impossible

let string_of_modifier = function
  | Public    -> "is_public"  
  | Private   -> "is_private" 
  | Protected -> "is_protected"
  | Static -> "static"  | Abstract -> "abstract" | Final -> "final"

let read_write in_lvalue =
  if in_lvalue then "write" else "read"
    
let escape_quote_array_field s =
  Str.global_replace (Str.regexp "[']") "__" s
 

(*****************************************************************************)
(* Defs/uses *)
(*****************************************************************************)

(* todo: yet another use/def, factorize code with defs_uses_php.ml?
 * But for defs we want more than just defs, we also want the arity
 * of parameters for instance. And for uses we also want sometimes to
 * process the arguments for instance with require_module, so it's hard
 * to factorize I think. Copy paste is fine sometimes ...
 *)
let add_uses id ast pr db =
  let h = Hashtbl.create 101 in

  let in_lvalue_pos = ref false in
  
  let visitor = V.mk_visitor { V.default_visitor with

    V.klvalue = (fun (k,vx) x ->
      match x with
      (* todo: need to handle pass by ref too so set in_lvalue_pos
       * for the right parameter. So need an entity_finder?
       *)
      | FunCallSimple (callname, args) ->
          let str = Ast_php.name callname in
          let args = args +> Ast.unparen +> Ast.uncomma in
          (match str, args with
          (* a little bit facebook specific ... *)
          | "require_module", [Arg ((Sc (C (String (str,_)))))] ->
              pr (spf "require_module('%s', '%s')."
                     (Db.readable_filename_of_id id db) str)
          | _ -> ()
          );

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
            (* todo: imprecise, need julien's precise callgraph *)
            pr (spf "docall(%s, '%s', 'method')." (name_id id db) str)
          end;
          
          k x

      | ObjAccessSimple (lval, tok, name) ->
          let str = Ast_php.name name in
          (* use a different namespace than func? *)
          if not (Hashtbl.mem h str)
          then begin
            Hashtbl.replace h str true;
            pr (spf "use(%s, '%s', field, %s)." 
                   (name_id id db) str (read_write !in_lvalue_pos))
          end;
          k x
      | VArrayAccess (lval, (_, Some((Sc(C(String((fld, i_9)))))), _)) ->
          let str = escape_quote_array_field fld in
          (* use a different namespace than func? *)
          if not (Hashtbl.mem h str)
          then begin
            Hashtbl.replace h str true;
            pr (spf "use(%s, '%s', array, %s)." 
                   (name_id id db) str (read_write !in_lvalue_pos))
          end;
          k x
          
          
      | _ -> k x
    );
    V.kexpr = (fun (k, vx) x ->
      match x with
      (* todo: enough? need to handle pass by ref too here *)
      | Assign (lval, _, e)
      | AssignOp(lval, _, e) 
        ->
          Common.save_excursion in_lvalue_pos true (fun () ->
            vx (Lvalue lval)
          );
          vx (Expr e);
          

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
    V.kxhp_html = (fun (k, _) x ->
      match x with
      | Xhp (xhp_tag, _attrs, _tok, _, _) 
      | XhpSingleton (xhp_tag, _attrs, _tok) 
        ->
          let str = Ast_php.name (Ast_php.XhpName xhp_tag) in
          (* use a different namespace than func? *)
          if not (Hashtbl.mem h str)
          then begin
            Hashtbl.replace h str true;
            pr (spf "docall(%s, '%s', 'class')." 
                   (name_id id db) str)
          end;
          k x
    );
  }
  in
  visitor (Entity ast);
  ()


let add_defs_and_uses id kind ast pr db =
  match kind, ast with
  | E.Function, FunctionE def ->
      pr (spf "arity(%s, %d)." (name_id id db)
             (List.length (def.f_params +> Ast.unparen +> Ast.uncomma_dots)));
      add_uses id ast pr db;
  | E.Constant, ConstantE def ->
      add_uses id ast pr db

  | E.Class _, ClassE def ->
      (match def.c_type with
      | ClassAbstract _ -> pr (spf "abstract(%s)." (name_id id db))
      | ClassFinal _ -> pr (spf "final(%s)." (name_id id db))
      | ClassRegular _ -> ()
      | Interface _ 
      | Trait _ -> ()
      );
      def.c_extends +> Common.do_option (fun (tok, x) ->
        pr (spf "extends(%s, '%s')." (name_id id db) (Ast.name x));
      );
      def.c_implements +> Common.do_option (fun (tok, interface_list) ->
        interface_list +> Ast.uncomma |> List.iter (fun x ->
          (* could put implements instead? it's not really the same
           * kind of extends. Or have a extends_interface/2? maybe
           * not worth it, just add kind(X, class) when using children/2
           * if you want to restrict your query.
           *)
          (match def.c_type with
          | Interface _ ->
             pr (spf "extends(%s, '%s')." (name_id id db) (Ast.name x));
          | _ ->
             pr (spf "implements(%s, '%s')." (name_id id db) (Ast.name x));
          )
        ));
      def.c_body +> Ast.unbrace +> List.iter (function
      | UseTrait (_tok, names, rules_or_tok) ->
          names +> Ast.uncomma +> List.iter (fun name ->
            pr (spf "mixins(%s, '%s')." (name_id id db) (Ast.name name))
          )
      | _ -> ()
      );
            
  | E.Method _, MethodE def -> 
      pr (spf "arity(%s, %d)." (name_id id db)
             (List.length (def.m_params +> Ast.unparen +> Ast.uncomma_dots)));
      def.m_modifiers +> List.iter (fun (m, _) -> 
        pr (spf "%s(%s)." (string_of_modifier m) (name_id id db));
      );
      add_uses id ast pr db;

  | E.Field, ClassVariableE (var, ms) ->
      ms +> List.iter (fun (m) -> 
        pr (spf "%s(%s)." (string_of_modifier m) (name_id id db))
      )

  (* todo? *)
  | E.Field, XhpAttrE _ ->
      ()

  | E.ClassConstant, _ -> ()
            
  | (E.TopStmts | E.Other _), _ ->
      add_uses id ast pr db;
      
  | _ -> raise Impossible


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* todo? could avoid going through database_php.ml and parse directly? *)
let gen_prolog_db ?(show_progress=true) db file =
  Common.with_open_outfile file (fun (pr, _chan) ->
   let pr s = pr (s ^ "\n") in
   pr ("%% -*- prolog -*-");
   pr (spf "%% facts about %s" (Db.path_of_project_in_database db));
   
   pr (":- discontiguous kind/2, at/3.");
   pr (":- discontiguous static/1, abstract/1, final/1.");
   pr (":- discontiguous is_public/1, is_private/1, is_protected/1.");
   pr (":- discontiguous extends/2, implements/2, mixins/2.");
   pr (":- discontiguous arity/2.");
   pr (":- discontiguous docall/3, use/4.");
   pr (":- discontiguous include/2, require_module/2.");

   db.Db.file_info#tolist +> List.iter (fun (file, _parsing_status) ->
     let file = Db.absolute_to_readable_filename file db in
     let parts = Common.split "/" file in
     pr (spf "file('%s', [%s])." file
            (parts +> List.map (fun s -> spf "'%s'" s) +> Common.join ","));
   );

   db.Db.defs.Db.id_kind#tolist
   +> Common_extra.with_progress_list_metter ~show_progress (fun k xs ->
      xs +> List.iter (fun (id, kind) ->
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
        add_defs_and_uses id kind ast pr db;

      );
   );
   db.Db.uses.Db.includees_of_file#tolist +> List.iter (fun (file1, xs) ->
     let file1 = Db.absolute_to_readable_filename file1 db in
     xs +> List.iter (fun file2 ->
       let file2 = 
         try Db.absolute_to_readable_filename file2 db 
         with Failure _ -> file2
       in
       pr (spf "include('%s', '%s')." file1 file2)
     );
   );
  )
