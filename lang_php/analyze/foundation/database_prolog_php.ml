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

module Env = Env_interpreter_php
module CG = Callgraph_php2

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This module makes it possible to ask questions on the structure of
 * a PHP codebase, for instance: "What are all the children of class Foo?".
 * It is inspired by a similar tool for java called JQuery
 * (http://jquery.cs.ubc.ca/).
 *
 * history:
 *  - basic defs (kinds, at)
 *  - inheritance tree
 *  - basic callgraph
 *  - basic datagraph
 *  - include/require (and possibly desugared wrappers like require_module())
 *  - precise callgraph, using julien's abstract interpreter (was called
 *    previously pathup/pathdown)
 *
 * todo:
 *  - get rid of berkeley db prerequiste, use ast_php_simple
 *  - precise datagraph
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
    | E.Class _ | E.Function | E.Constant ->
        spf "'%s'" s
    | E.Method _ | E.ClassConstant | E.Field ->
        (match Db.class_or_interface_id_of_nested_id_opt id db with
        | Some id_class ->
            let sclass = Db.name_of_id id_class db in
            (match id_kind with
            (* todo? xhp decl ? *)
            (* old: I used to do something different for Field amd remove
             * the $ because in use-mode but we don't use the $ anymore.
             * now def don't have a $ too, and can be xhp attribute too.
             *)
            | E.Method _ | E.Field | E.ClassConstant ->
                spf "('%s','%s')" sclass s
            | _ -> raise Impossible
            )
        | None ->
            failwith (spf "could not find enclosing class for %s"
                         (Db.str_of_id id db))
        )
    | E.TopStmts -> spf "'__TOPSTMT__%s'" (EC.str_of_id id)
    (* ?? *)
    | E.Other s -> spf "'__IDMISC__%s'" (EC.str_of_id id)

    | _ ->
        (* not in db for now *)
        raise Impossible
    )
  with Not_found ->
    failwith (spf "could not find name for id %s" (Db.str_of_id id db))

let name_of_node = function
  | CG.File s -> spf "'__TOPSTMT__%s'" s
  | CG.Function s -> spf "'%s'" s
  | CG.Method (s1, s2) -> spf "('%s','%s')" s1 s2
  | CG.FakeRoot -> "'__FAKE_ROOT__'"

let string_of_id_kind x =
  Graph_code_prolog.string_of_entity_kind x

let string_of_modifier = function
  | Public    -> "is_public"
  | Private   -> "is_private"
  | Protected -> "is_protected"
  | Static -> "static"  | Abstract -> "abstract" | Final -> "final"

let rec string_of_hint_type h =
  match h with
  | Some x ->
      (match x with
      | Hint c ->
          (match c with
          | ClassName (c, _) -> Ast.name c (* TODO: figure out a reasonable respresentation for type args in prolog *)
          | Self _ -> "self"
          | Parent _ -> "parent"
          | LateStatic _ -> "")
      | HintArray _ -> "array"
      | HintQuestion (_, t) -> "?" ^ (string_of_hint_type (Some t))
      | HintTuple v1 ->
          let elts = List.map
                       (fun x -> string_of_hint_type (Some x))
                       (Ast.uncomma (Ast.unparen v1))
          in
          "(" ^ (String.concat ", " elts) ^ ")"
      | HintCallback _ -> "callback"
      )
  | None -> ""

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

  (* to fatorize code between NewLv and New *)
  let docall_class classref =
    match classref with
    | ClassNameRefStatic x ->
      (match x with
      | ClassName (name, _) -> (* TODO: currently ignoring type args *)

        let str = Ast_php.name name in
        (* use a different namespace than func? *)
        if not (Hashtbl.mem h str)
        then begin
          Hashtbl.replace h str true;
          pr (spf "docall(%s, '%s', class)."
                (name_id id db) str)
        end;
        
      (* todo: do something here *)
      | Self _
      | Parent _
      | LateStatic _ ->
        ()
      )
    | ClassNameRefDynamic _ -> ()
  in

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
          (* Many entities (functions, classes) in PHP are passed as
           * string parameters to some higher-order functions. We normally
           * don't index in the Prolog database the arguments to functions, but
           * certain function calls are really disguised calls
           * to new, hence the special cases below.
           *
           * todo: we should automatically extract the list of all
           * higher-order functions.
           *)
          | ("newv" | "DT"), Arg ((Sc (C (String (str2,_)))))::rest ->
              pr (spf "docall(%s, ('%s','%s'), special)."
                     (name_id id db) str str2)

          (* could be encoded as a docall(...,special) too *)
          | "require_module", [Arg ((Sc (C (String (str,_)))))] ->
              pr (spf "require_module('%s', '%s')."
                     (Db.readable_filename_of_id id db) str)

          | _ -> ()
          );

          if not (Hashtbl.mem h str)
          then begin
            Hashtbl.replace h str true;
            pr (spf "docall(%s, '%s', function)." (name_id id db) str)
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
            pr (spf "docall(%s, '%s', method)." (name_id id db) str)
          end;
          (match x with
          | StaticMethodCallSimple ((qu,_tok), name, args) ->
              (match qu with
              | ClassName (name2, _) ->
                  pr (spf "docall(%s, ('%s','%s'), method)."
                           (name_id id db) (Ast_php.name name2) str)
              (* this should have been desugared while building the
               * code database, except for traits code ...
               *)
              | Self _| Parent _
              (* can't do much ... *)
              | LateStatic _ -> ()
              )
          | MethodCallSimple _
          | StaticMethodCallVar _
              -> ()
          | _ -> raise Impossible
          );

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
      | NewLv (_, (_, classref, args), _) ->
         docall_class classref;
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
            vx (Expr lval)
          );
          vx (Expr e);


      | New (_, classref, args)
      | AssignNew (_, _, _, _, classref, args) ->
          docall_class classref;
          k x
      | Yield _ | YieldBreak _ ->
          pr (spf "yield(%s)." (name_id id db));
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
            pr (spf "docall(%s, '%s', class)."
                   (name_id id db) str)
          end;
          k x
    );
    V.kstmt = (fun (k, _) x ->
      (match x with
      | Throw (_, New (_, ClassNameRefStatic (ClassName (name, _)), _), _) ->
          pr (spf "throw(%s, '%s')."
                 (name_id id db) (Ast.str_of_name name))
      | Try (_, _, c1, cs) ->
          (c1::cs) +> List.iter (fun (_, (_, (classname, dname), _), _) ->
            pr (spf "catch(%s, '%s')."
                   (name_id id db) (Ast.str_of_name classname))
          );
      | _ -> ()
      );
      k x
    );
    V.kobj_dim = (fun (k, _) x ->
      match x with
      (* copy paste of ObjAccessSimple *)
      | OName name ->
          let str = Ast_php.name name in
          if not (Hashtbl.mem h str)
          then begin
            Hashtbl.replace h str true;
            pr (spf "use(%s, '%s', field, %s)."
                   (name_id id db) str (read_write !in_lvalue_pos))
          end;
          k x
      | _ -> k x
    );
    V.kscalar = (fun (k, _) x ->
      match x with
      | ClassConstant ((qu,_tok), cstname) ->
         (match qu with
         | ClassName (classname, _) ->
           pr (spf "use(%s, ('%s','%s'), constant, read)."
                (name_id id db) (Ast_php.str_of_name classname)
                (Ast_php.str_of_name cstname))
         (* this should have been desugared while building the
          * code database, except for traits code ...
          *)
         | Self _| Parent _
         (* can't do much ... *)
         | LateStatic _ -> ()
         );
        k x

      | _ -> k x
    );
  }
  in
  visitor (Entity ast);
  ()

let add_function_params id def pr db =
  def.f_params +> Ast.unparen +> Ast.uncomma_dots +> Common.index_list_0 +>
    List.iter (fun (param, i) ->
      pr (spf "parameter(%s, %d, '$%s', '%s')."
             (name_id id db)
             i
             (Ast.str_of_dname param.p_name)
             (string_of_hint_type param.p_type)
      ))

let add_uses_and_properties id kind ast pr db =
  match kind, ast with
  | E.Function, FunctionE def ->
      pr (spf "arity(%s, %d)." (name_id id db)
             (List.length (def.f_params +> Ast.unparen +> Ast.uncomma_dots)));
      add_function_params id def pr db;
      add_uses id ast pr db;
  | E.Constant, ConstantE def ->
      add_uses id ast pr db

  | E.Class _, ClassE def ->
      (match def.c_type with
      | ClassAbstract _ -> pr (spf "abstract(%s)." (name_id id db))
      | ClassFinal _ -> pr (spf "final(%s)." (name_id id db))
      | ClassRegular _ -> ()
      (* the kind/2 will cover those different cases *)
      | Interface _
      | Trait _ -> ()
      );
      def.c_extends +> Common.do_option (fun (tok, x) ->
        pr (spf "extends(%s, '%s')." (name_id id db) (Ast.name x));
      );
      def.c_implements +> Common.do_option (fun (tok, interface_list) ->
        interface_list +> Ast.uncomma +> List.iter (fun x ->
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
             (List.length (def.f_params +> Ast.unparen +> Ast.uncomma_dots)));
      def.f_modifiers +> List.iter (fun (m, _) ->
        pr (spf "%s(%s)." (string_of_modifier m) (name_id id db));
      );
      add_function_params id def pr db;
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
(* Build db *)
(*****************************************************************************)

(* todo? could avoid going through database_php.ml and parse directly? *)
let gen_prolog_db2 ?(show_progress=true) db file =
  Common.with_open_outfile file (fun (pr, _chan) ->
   let pr s = pr (s ^ "\n") in
   pr ("%% -*- prolog -*-");
   pr (spf "%% facts about %s" (Db.path_of_project_in_database db));

   pr (":- discontiguous kind/2, at/3.");
   pr (":- discontiguous static/1, abstract/1, final/1.");
   pr (":- discontiguous is_public/1, is_private/1, is_protected/1.");
   pr (":- discontiguous extends/2, implements/2, mixins/2.");
   pr (":- discontiguous arity/2.");
   pr (":- discontiguous parameter/4.");
   pr (":- discontiguous docall/3, use/4.");
   pr (":- discontiguous docall2/3.");
   pr (":- discontiguous include/2, require_module/2.");
   pr (":- discontiguous yield/1.");
   pr (":- discontiguous throw/2, catch/2.");
   pr (":- discontiguous problem/2.");


   (* see the comment on newv in add_uses() above *)
   pr (":- discontiguous special/1.");
   pr ("special('newv').");
   pr ("special('DT').");

   db.Db.file_info#tolist +> List.iter (fun (file, file_info) ->
     let file = Db.absolute_to_readable_filename file db in
     let parts = Common.split "/" file in
     pr (spf "file('%s', [%s])." file
            (parts +> List.map (fun s -> spf "'%s'" s) +> Common.join ","));
     (match file_info.Db.parsing_status with
     | `OK -> ()
     | `BAD -> pr2 (spf "problem('%s', parse_error)." file)
     );
   );
   let ids = db.Db.defs.Db.id_kind#tolist in
   ids +> Common_extra.progress ~show:show_progress (fun k ->
    List.iter (fun (id, kind) ->
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
        add_uses_and_properties id kind ast pr db;

   ));
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
let gen_prolog_db ?show_progress a b =
  Common.profile_code "Prolog_php.gen" (fun () ->
    gen_prolog_db2 ?show_progress a b)

(* todo:
 * - could also improve precision of use/4
 * - detect higher order functions so that function calls through
 *   generic higher order functions are present in the callgraph
 *)
let append_callgraph_to_prolog_db2 ?(show_progress=true) g file =

  (* look previous information, to avoid introduce duplication
   *
   * todo: check/compare with the basic callgraph I do in add_uses?
   * it should be a superset.
   *  - should find more functions when can resolve statically dynamic funcall
   *  -
   *)
  let h_oldcallgraph = Hashtbl.create 101 in
  file +> Common.cat +> List.iter (fun s ->
    if s =~ "^docall(.*"
    then Hashtbl.add h_oldcallgraph s true
  );

  Common2.with_open_outfile_append file (fun (pr, _chan) ->
    let pr s = pr (s ^ "\n") in
    pr "";
    g +> Map_poly.iter (fun src xs ->
      xs +> Set_poly.iter (fun target ->
        let kind =
          match target with
          (* can't call a file ... *)
          | CG.File _ -> raise Impossible
          (* can't call a fake root*)
          | CG.FakeRoot -> raise Impossible
          | CG.Function _ -> "function"
          | CG.Method _ -> "method"
        in
        (* do not count those fake edges *)
        (match src, target with
        | CG.FakeRoot, _
        | CG.File _, _ -> ()
        | _, CG.Function s when s =~ "__builtin" -> ()
        | _ ->
            let s1 =(spf "docall(%s, %s, %s)."
                       (name_of_node src) (name_of_node target) kind) in
            let s =(spf "docall2(%s, %s, %s)."
                       (name_of_node src) (name_of_node target) kind) in
            if Hashtbl.mem h_oldcallgraph s1
            then ()
            else pr s
        )
      )
    )
  )
let append_callgraph_to_prolog_db ?show_progress a b =
  Common.profile_code "Prolog_php.callgraph" (fun () ->
    append_callgraph_to_prolog_db2 ?show_progress a b)


(*****************************************************************************)
(* Query helpers *)
(*****************************************************************************)

(* used for testing *)
let prolog_query ?(verbose=false) ~source_file ~query =
  let facts_pl_file = Common.new_temp_file "prolog_php_db" ".pl" in
  let helpers_pl_file =
    Config_pfff.path ^ "/h_program-lang/database_code.pl" in

  let show_progress = false in

  (* make sure it's a valid PHP file *)
  let _ast = Parse_php.parse_program source_file in

  (* todo: at some point avoid using database_php_build and
   * generate the prolog db directly from the sources.
   *)
  let db =
    Database_php_build.db_of_files_or_dirs ~show_progress [source_file] in

  gen_prolog_db ~show_progress db facts_pl_file;

  let jujudb =
    Database_juju_php.juju_db_of_files ~show_progress [source_file] in
  let codedb =
    Database_juju_php.code_database_of_juju_db jujudb in
  let cg =
    Callgraph_php_build.create_graph ~show_progress [source_file] codedb in

  append_callgraph_to_prolog_db
    ~show_progress cg facts_pl_file;
  if verbose then Common.cat facts_pl_file +> List.iter pr2;
  let cmd =
    spf "swipl -s %s -f %s -t halt --quiet -g \"%s ,fail\""
      facts_pl_file helpers_pl_file query
  in
  let xs = Common.cmd_to_list cmd in
  xs
