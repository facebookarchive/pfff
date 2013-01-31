(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

module E = Database_code
module G = Graph_code

module Ast = Ast_php_simple
open Ast_php_simple

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for PHP. See graph_code.ml and main_codegraph.ml
 * for more information. Yet another code database for PHP ...
 * 
 * See also old/graph_php.ml, facebook/flib_dependencies/,
 * facebook/check_module/graph_module.ml
 * 
 * schema:
 *  Root -> Dir -> File (.php) -> Class (interfaces and traits too)
 *                                 -> Method
 *                                 -> Field
 *                                 -> ClassConstant
 *                             -> Function
 *                             -> Constant
 *       -> Dir -> SubDir -> File -> ...
 *       -> Dir -> SubDir -> Module? -> ...
 * 
 * todo: 
 *  - handle Interface and Traits, do not translate them in RegularClass?
 *  - handle static vs non static methods/fields? but at the same time
 *    lots of our code abuse $this-> where they should use self::, so
 *    maybe simpler not make difference between static and non static
 *  - reuse env, most of of build() and put it in graph_code.ml
 *    and just pass the PHP specificities.
 *  - add tests
 * 
 * issues regarding errors in a codebase:
 *  - parse errors, test code
 *    => skip list, file: or dir:
 *  - nested functions, duped functions defined conditionnally
 *    => use the at_toplevel field below
 *  - duped functions 
 *    => skip list (or remove the offending code), file: or dir:
 *  - duped local functions in scripts/
 *    => skip list, skip_errors_dir:
 *  - lookup failure because use different software stack (e.g.
 *    html/intern/wiki/, lib/arcanist/, etc) 
 *    => skip list, dir:
 * 
 * where to display the errors:
 *  - terminal for the really important one
 *  - pfff.log for less important and to have more details
 *  - in the codegraph itself under the PB directory for all the rest
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  g: Graph_code.graph;

  phase: phase;

  readable: Common.filename;

  current: Graph_code.node;
  (* "NOSELF" when outside a class *)
  self: string;
  (* "NOPARENT" when no parent *)
  parent: string;

  at_toplevel: bool;

  (* right now used in extract_uses phase to transform a src like main()
   * into its File, and also to give better error messages.
   * We use the Hashtbl.find_all property of the hashtbl below.
   * The pair of filenames is readable * fullpath
   *)
  dupes: (Graph_code.node, (Common.filename * Common.filename)) Hashtbl.t;

  (* in many files like scripts/ people reuse the same function name. This
   * is annoying for global analysis, so when we detect such files, 
   * because right now they are listed in the skip file as skip_errors_dir:,
   * then we dynamically and locally rename this function.
   *)
  (* TODO: dupe_renaming *)

  (* PHP is case insensitive so certain lookup fails because people
   * used the wrong case. Those errors are less important so
   * we just automatically relookup to the correct entity.
   *)
  case_insensitive: (Graph_code.node, Graph_code.node) Hashtbl.t;

  (* todo: dynamic_fails stats *)

  log: string -> unit;
  pr2_and_log: string -> unit;
  is_skip_error_file: Common.filename (* readable *) -> bool;
}
  (* We need 3 phases, one to get all the definitions, one to
   * get the inheritance information, and one to get all the Uses.
   * The inheritance is a kind of use, but certain uses like using
   * a field or method need the full inheritance tree to already be
   * computed as we may need to lookup entities up in the parents.
   *)
  and phase = Defs | Inheritance | Uses

let look_like_class_re = 
  Str.regexp "^\\([A-Z][A-Za-z_0-9]*\\)\\(::[A-Za-z_0-9]*\\)?$"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let _hmemo = Hashtbl.create 101
let parse2 file = 
  try 
    Common.save_excursion Ast_php_simple_build.store_position true (fun () ->
    let cst = Parse_php.parse_program file in
    let ast = Ast_php_simple_build.program cst in
    ast
    )
  with
  | Timeout -> raise Timeout
  | exn ->
    pr2_once (spf "PARSE ERROR with %s, exn = %s" file (Common.exn_to_s exn));
    []
let parse a = 
  (* on huge codebase naive memoization stresses too much the GC.
   * We marshall a la juju so the heap graph is smaller at least.
   *)
  Marshal.from_string
    (Common.memoized _hmemo a (fun () -> 
      Marshal.to_string (parse2 a) []
     )) 0


let add_node_and_edge_if_defs_mode env name_node =
  let (name, kind) = name_node in
  let str =
    match kind with
    | E.ClassConstant | E.Field | E.Method _ -> 
      env.self ^ "." ^ Ast.str_of_name name
    | _ -> 
      Ast.str_of_name name
  in
  let node = (str, kind) in

  if env.phase = Defs then begin
    if G.has_node node env.g 
    then 
      let file = Parse_info.file_of_info (Ast.tok_of_name name) in
      (* todo: look if is_skip_error_file in which case populate
       * a env.dupe_renaming
       *)
      (match kind with
      (* less: log at least? *)
      | E.Class _ | E.Function | E.Constant when not env.at_toplevel -> 
        ()

      (* If the class was dupe, of course all its members are also duped.
       * But actually we also need to add it to env.dupes, so below,
       * we dont set env.current to this node, otherwise we may
       * pollute the callees of the original node.
       *)
      | E.ClassConstant | E.Field | E.Method _ 
        when Hashtbl.mem env.dupes (env.self, E.Class E.RegularClass) ->
        Hashtbl.add env.dupes node (env.readable, file)
      | E.ClassConstant | E.Field | E.Method _ ->
        env.pr2_and_log (spf "DUPE METHOD: %s" (G.string_of_node node));
      | _ -> 
        Hashtbl.add env.dupes node (env.readable, file)
      )
    else begin
      env.g +> G.add_node node;
      (* if we later find a duplicate for node, we will
       * redirect this edge in build() to G.dupe (unless
       * the duplicate are all in a skip_errors dir).
       *)
      env.g +> G.add_edge (env.current, node) G.Has;

      let nodeinfo = { Graph_code.
        pos = Parse_info.parse_info_of_info (Ast.tok_of_name name);
        props = [];
      } in
      env.g +> G.add_nodeinfo node nodeinfo;
    end
  end;
  (* for dupes like main(), but also dupe classes, or methods of dupe 
   * classe, it's better to keep 'current' as the current File so
   * at least we will avoid to add in the wrong node some relations
   * that should not exist.
   *)
  if Hashtbl.mem env.dupes node
  then env
  else 
    { env with current = node }



let rec add_use_edge env (((str, tok) as name, kind)) =
  let src = env.current in
  let dst = (str, kind) in
  (match () with
  (* maybe nested function, in which case we dont have the def *)
  | _ when not (G.has_node src env.g) ->
      env.pr2_and_log (spf "LOOKUP SRC FAIL %s --> %s, src doesn't exist (nested func?)"
              (G.string_of_node src) (G.string_of_node dst));

  | _ when G.has_node dst env.g -> 
      G.add_edge (src, dst) G.Use env.g

  | _ when Hashtbl.mem env.case_insensitive (String.lowercase str, kind) ->
      let (final_str, _) = 
        Hashtbl.find env.case_insensitive (String.lowercase str, kind) in
      add_use_edge env ((final_str, tok), kind)

  | _ -> 
      (match kind with
      (* if dst is a Class, then try Interface *)
      (*
      | E.Class E.RegularClass -> 
          add_use_edge env (name, E.Class E.Interface)
      *)
      | _ ->
          let kind_original = kind in
          let dst = (str, kind_original) in

          G.add_node dst env.g;
          let parent_target = G.not_found in
          (match kind with
          (* TODO, fix those *)
          | E.Method _ | E.Field | E.ClassConstant -> 
            ()
          | _ ->
            let file = name +> Ast.tok_of_name +> Parse_info.string_of_info in
            let f = 
              if env.phase = Inheritance
              then env.pr2_and_log 
              else 
                if file =~ ".*third-party" || file =~ ".*third_party"
                then (fun _s -> ())
                else env.log
            in
            f (spf "PB: lookup fail on %s (at %s)"(G.string_of_node dst) file);
            env.g +> G.add_edge (parent_target, dst) G.Has;
            env.g +> G.add_edge (src, dst) G.Use;
          );
      )
  )

(*****************************************************************************)
(* Lookup *)
(*****************************************************************************)

let lookup2 g (aclass, amethod_or_field_or_constant) tok =
  let rec depth current =
    if not (G.has_node current g)
    then None
    else 
      let children = G.children current g in
      let full_name = (fst current ^ "." ^ amethod_or_field_or_constant) in
      let res = 
        children +> Common.find_some_opt (fun (s2, kind) ->
          if full_name =$= s2
          then Some ((s2, tok), kind)
          else None
        )
      in
      match res with
      | Some x -> Some x
      | None ->
        let parents_inheritance = G.succ current G.Use g in
        breath parents_inheritance
  and breath xs = xs +> Common.find_some_opt depth
  in
  depth (aclass, E.Class E.RegularClass)

let lookup g a = 
  Common.profile_code "Graph_php.lookup" (fun () -> lookup2 g a)

(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)
let rec extract_defs_uses env ast readable =
  let env = { env with current = (readable, E.File); readable } in
  if env.phase = Defs then begin
    let dir = Common.dirname env.readable in
    G.create_intermediate_directories_if_not_present env.g dir;
    env.g +> G.add_node (env.readable, E.File);
    env.g +> G.add_edge ((dir, E.Dir), (env.readable, E.File))  G.Has;
  end;
  List.iter (stmt_toplevel env) ast;

(* ---------------------------------------------------------------------- *)
(* Stmt/toplevel *)
(* ---------------------------------------------------------------------- *)
and stmt env x = 
  stmt_bis { env with at_toplevel = false } x
and stmt_toplevel env x =
  stmt_bis env x
and stmt_bis env x =
  match x with
  (* boilerplate *)
  | FuncDef def -> func_def env def
  | ClassDef def -> class_def env def
  | ConstantDef def -> constant_def env def

  (* old style constant definition, before PHP 5.4 *)
  | Expr(Call(Id("define", _), [String((name)); v])) ->
     let node = (name, E.Constant) in
     let env = add_node_and_edge_if_defs_mode env node in
     expr env v

  | Expr e -> expr env e
  | Block xs -> stmtl env xs
  | If (e, st1, st2) ->
      expr env e;
      stmtl env [st1;st2]
  | Switch (e, xs) ->
      expr env e;
      casel env xs
  | While (e, xs) | Do (xs, e) ->
      expr env e;
      stmtl env xs
  | For (es1, es2, es3, xs) ->
      exprl env (es1 ++ es2 ++ es3);
      stmtl env xs
  | Foreach (e1, e2, e3opt, xs) ->
      exprl env [e1;e2];
      Common.opt (expr env) e3opt;
      stmtl env xs;
  | Return eopt  | Break eopt | Continue eopt ->
      Common.opt (expr env) eopt
  | Throw e -> expr env e
  | Try (xs, c1, cs) ->
      stmtl env xs;
      catches env (c1::cs)

  | StaticVars xs ->
      xs +> List.iter (fun (name, eopt) -> Common.opt (expr env) eopt;)
  (* could add entity for that? *)
  | Global xs -> exprl env xs

(* less: add deps to type hint? *)
and catch env (_hint_type, _name, xs) =
  stmtl env xs

and case env = function
  | Case (e, xs) ->
      expr env e;
      stmtl env xs
  | Default xs ->
      stmtl env xs

and stmtl env xs = List.iter (stmt env) xs
and casel env xs = List.iter (case env) xs
and catches env xs = List.iter (catch env) xs

(* ---------------------------------------------------------------------- *)
(* Defs *)
(* ---------------------------------------------------------------------- *)
and func_def env def =
  let node = (def.f_name, E.Function) in
  let env = 
    match def.f_kind with
    | AnonLambda -> env
    | Function -> add_node_and_edge_if_defs_mode env node 
    | Method -> raise Impossible
  in
  def.f_params +> List.iter (fun p ->
    (* less: add deps to type hint? *)
    Common.opt (expr env) p.p_default;
  );
  stmtl env def.f_body

and class_def env def =
  let kind = E.RegularClass in
  (*
    let _kind = 
    match def.c_kind with
    | ClassRegular | ClassFinal | ClassAbstract -> E.RegularClass
    | Interface -> E.Interface
    | Trait -> E.Trait
    in
  *)
  let node = (def.c_name, E.Class kind) in
  let env = add_node_and_edge_if_defs_mode env node in

  (* opti: could also just push those edges in a _todo ref during Defs *)
  if env.phase = Inheritance then begin
    def.c_extends +> Common.do_option (fun c2 ->
      add_use_edge env (c2, E.Class E.RegularClass);
    );
    (* todo: use Interface and Traits at some point *)
    def.c_implements +> List.iter (fun c2 ->
      add_use_edge env (c2, E.Class E.RegularClass);
    );
    def.c_uses +> List.iter (fun c2 ->
      add_use_edge env (c2, E.Class E.RegularClass);
    );
  end;
  let self = Ast.str_of_name def.c_name in
  let parent = 
    match def.c_extends with 
    | None -> "NOPARENT" 
    | Some c2 -> Ast.str_of_name c2
  in
  let env = { env with self; parent } in
  
  def.c_constants +> List.iter (fun def ->
    let node = (def.cst_name, E.ClassConstant) in
    let env = add_node_and_edge_if_defs_mode env node in
    expr env def.cst_body;
  );
  def.c_variables +> List.iter (fun def ->
    let node = (def.cv_name, E.Field) in
    let env = add_node_and_edge_if_defs_mode env node in
    Common.opt (expr env) def.cv_value
  );
  def.c_methods +> List.iter (fun def ->
    (* less: be more precise at some point *)
    let kind = E.RegularMethod in
    let node = (def.f_name, E.Method kind) in
    let env = add_node_and_edge_if_defs_mode env node in
    stmtl env def.f_body
  )

and constant_def env def =
  let node = (def.cst_name, E.Constant) in
  let env = add_node_and_edge_if_defs_mode env node in
  expr env def.cst_body

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env x = 
  if env.phase = Uses then
  (match x with
  | Int _ | Double _  -> ()

  (* a String in PHP can actually hide a class (or a function) *)
  | String (s, tok) when s ==~ look_like_class_re ->
    let entity = Common.matched1 s in
    (* less: do case insensitive? handle conflicts? *)
    if G.has_node (entity, E.Class E.RegularClass) env.g 
    then begin
      (match env.readable with
      (* less: phabricator specific *)
      | s when s =~ ".*__phutil_library_map__.php" -> ()
      | _ -> 
        env.log (spf "DYNCALL_STR:%s (at %s)" s env.readable);
        add_use_edge env ((entity, tok), E.Class E.RegularClass)
      );
    end
  (* todo? also look for functions? but has more FPs with regular
   * fields. Need to avoid this FP either by not calling
   * the String visitor when inside an array where all fields are
   * short or some field do not correspond to an existing function
   *)
  | String _ -> ()

  (* Note that you should go here only when it's a constant. You should
   * catch the use of Id in other contexts before. For instance you
   * should match on Id in Call, Class_get, etc so that this code
   * is executed really as a last resort, which usually means when
   * there is the use of a constant.
   *)
  | Id name -> 
      (* a parameter or local variable *)
      if Ast.is_variable name 
      then ()
      else add_use_edge env (name, E.Constant)

  (* -------------------------------------------------- *)
  | Call (e, es) ->
    (match e with
    (* simple function call *)
    | Id name when not (Ast.is_variable name) ->
        add_use_edge env (name, E.Function);
        exprl env es

    (* static method call *)
    | Class_get (Id ("__special__self", tok), e2) ->
        expr env (Call (Class_get (Id (env.self, tok), e2), es))
    | Class_get (Id ("__special__parent", tok), e2) ->
        expr env (Call (Class_get (Id (env.parent, tok), e2), es))
    (* TODO: incorrect actually ... but good enough for now for codegraph *)
    | Class_get (Id ("__special__static", tok), e2) ->
        expr env (Call (Class_get (Id (env.self, tok), e2), es))

    | Class_get (Id name1, Id name2) 
        when not (Ast.is_variable name1) && not (Ast.is_variable name2) ->
         let aclass = Ast.str_of_name name1 in
         let amethod = Ast.str_of_name name2 in
         let tok = snd name2 in
         let node = ((aclass ^ "." ^ amethod, tok), E.Method E.RegularMethod)in
         (* some classes may appear as dead because a 'new X()' is
          * transformed into a 'Call (... "__construct")' and such a method
          * may not exist, or may have been "lookup"ed in the parent.
          * So for "__construct" we also create an edge to the class
          * directly
          *)
         (match amethod with
         | "__construct" -> add_use_edge env (name1, E.Class E.RegularClass)
         | _ -> ()
         );
         (match lookup env.g (aclass, amethod) tok with
         | None -> 
           (match amethod with
           | "__construct" -> ()
           | _ -> add_use_edge env node
           )
         | Some n -> add_use_edge env n
         );
         exprl env es

    (* object call *)
    | Obj_get (e1, Id name2)  when not (Ast.is_variable name2) ->
        (match e1 with
        (* handle easy case *)
        | This (_,tok) ->
          expr env (Call (Class_get (Id (env.self, tok), Id name2), es))
        (* need class analysis ... *)
        | _ -> 
          (* todo: increment dynamic_fails stats *)
          expr env e1;
          exprl env es
        )
    (* todo: increment dynamic_fails stats *)
    | _ -> 
      expr env e; 
      exprl env es
    (* todo: increment dynamic_fails stats also when use func_call_args() *)
    )

  (* -------------------------------------------------- *)
  (* This should be executed only for access to class constants or static
   * class variable; calls should have been catched in the Call pattern above.
   *)
  | Class_get (e1, e2) ->
      (match e1, e2 with
      | Id ("__special__self", tok), _ ->
        expr env (Class_get (Id (env.self, tok), e2))
      | Id ("__special__parent", tok), _ ->
        expr env (Class_get (Id (env.parent, tok), e2))
      (* TODO: incorrect actually ... but good enough for now for codegraph *)
      | Id ("__special__static", tok), _ ->
        expr env (Class_get (Id (env.self, tok), e2))

      | Id name1, Id name2
        when not (Ast.is_variable name1) && not (Ast.is_variable name2) ->
          let aclass = Ast.str_of_name name1 in
          let aconstant = Ast.str_of_name name2 in
          let tok = snd name2 in
          let node = ((aclass ^ "." ^ aconstant, tok), E.ClassConstant) in
          (match lookup env.g (aclass, aconstant) tok with
          | None -> add_use_edge env node
          (* less: assert kind = ClassConstant? *)
          | Some n -> add_use_edge env n
          )

      | Id name1, Id name2
        when not (Ast.is_variable name1) && (Ast.is_variable name2) ->
          let aclass = Ast.str_of_name name1 in
          let astatic_var = Ast.str_of_name name2 in
          let tok = snd name2 in
          let node = ((aclass ^ "." ^ astatic_var, tok), E.Field) in
          (match lookup env.g (aclass, astatic_var) tok with
          | None -> add_use_edge env node
          (* less: assert kind = Static variable *)
          | Some n -> add_use_edge env n
          )
 
     (* todo: update dynamic stats *)
     | Id name1, e2 when not (Ast.is_variable name1) ->
          add_use_edge env (name1, E.Class E.RegularClass);
          expr env e2;
     | e1, Id name2 when not (Ast.is_variable name2) ->
       expr env e1;
     | _ -> 
       exprl env [e1; e2]
      )

  (* same, should be executed only for field access *)
  | Obj_get (e1, e2) ->
      (match e1, e2 with
      (* handle easy case *)
      | This (_, tok), Id name2 when not (Ast.is_variable name2) ->
          let (s2, tok2) = name2 in
          expr env (Class_get (Id (env.self, tok), Id ("$" ^ s2, tok2)))
      | _, Id name2 when not (Ast.is_variable name2) ->
          expr env e1;
      | _ ->
          exprl env [e1; e2]
      )

  | New (e, es) ->
      expr env (Call (Class_get(e, Id ("__construct", None)), es))

  (* -------------------------------------------------- *)
  (* boilerplate *)
  | List xs -> exprl env xs
  | Assign (_, e1, e2) -> exprl env [e1;e2]
  | InstanceOf (e1, e2) -> 
      expr env e1;
      (match e2 with
      (* less: add deps? *)
      | Id name when not (Ast.is_variable name) -> 
          ()
      | _ -> 
          (* todo: update dynamic *)
          expr env e2
      )
          
  | This _ -> ()
  | Array_get (e, eopt) ->
      expr env e;
      Common.opt (expr env) eopt
  | Infix (_, e) | Postfix (_, e) | Unop (_, e) -> expr env e
  | Binop (_, e1, e2) -> exprl env [e1; e2]
  | Guil xs -> exprl env xs
  | Ref e -> expr env e
  | ConsArray (_, xs) -> array_valuel env xs
  | Xhp x -> xml env x
  | CondExpr (e1, e2, e3) -> exprl env [e1; e2; e3]
  (* less: again, add deps for type? *)
  | Cast (_, e) -> expr env e
  | Lambda def -> func_def env def
  )

and array_value env = function
  | Aval e -> expr env e
  | Akval (e1, e2) -> exprl env [e1; e2]  

(* todo: dependency to :x:y class? *)
and xml env x =
(* todo: dependency on field? *)
  x.xml_attrs +> List.iter (fun (name, xhp_attr) -> expr env xhp_attr);
  x.xml_body +> List.iter (xhp env)

and xhp env = function
  | XhpText s -> ()
  | XhpExpr e -> expr env e
  | XhpXml x -> xml env x

and exprl env xs = List.iter (expr env) xs
and array_valuel env xs = List.iter (array_value env) xs

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) ?(only_defs=false) dir skip_list =
  let root = Common.realpath dir in
  let all_files = Lib_parsing_php.find_php_files_of_dir_or_files [root] in
  
  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files skip_list root all_files in
  (* step0: reorder files *)
  let files = Skip_code.reorder_files_skip_errors_last skip_list root files in
  
  let g = G.create () in
  G.create_initial_hierarchy g;

  let chan = open_out (Filename.concat (Sys.getcwd()) "pfff.log") in

  let env = {
    g; 
    phase = Defs;
    current = ("filled_later", E.File);
    readable = "filled_later";
    self = "NOSELF"; parent = "NOPARENT";
    dupes = Hashtbl.create 101;
    (* set after the defs phase *)
    case_insensitive = Hashtbl.create 101;
    log = (fun s ->
        output_string chan (s ^ "\n"); 
        flush chan; 
    );
    pr2_and_log = (fun s ->
      if verbose then pr2 s;
      output_string chan (s ^ "\n"); 
      flush chan; 
    );
    is_skip_error_file = Skip_code.build_filter_errors_file skip_list;
    at_toplevel = true;
  } 
  in

  (* step1: creating the nodes and 'Has' edges, the defs *)
  env.pr2_and_log "\nstep1: extract defs";
  files +> Common_extra.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let readable = Common.filename_without_leading_path root file in
      let ast = parse file in
      (* will modify env.dupes instead of raise Graph_code.NodeAlreadyPresent *)
      extract_defs_uses { env with phase = Defs} ast readable;
   ));
  Common.hkeys env.dupes 
  +> List.filter (fun (_, kind) ->
    match kind with
    | E.ClassConstant | E.Field | E.Method _ -> false
    | _ -> true
  )
  +> List.iter (fun node ->
    let nodeinfo = G.nodeinfo node g in
    let orig_file = nodeinfo.G.pos.Parse_info.file in
    let orig_readable = Common.filename_without_leading_path root orig_file in

    let files = Hashtbl.find_all env.dupes node in
    let (ex_readable, ex_file) = List.hd files in

    let dupes = orig_readable::List.map fst files in
    let cnt = List.length dupes in

    let (in_skip_errors, other) = 
      List.partition env.is_skip_error_file dupes in

    (match List.length in_skip_errors, List.length other with
    (* dupe in regular codebase, bad *)
    | _, n when n >= 2 ->
      env.pr2_and_log  (spf "DUPE: %s (%d)" (G.string_of_node node) cnt);
      g +> G.remove_edge (G.parent node g, node) G.Has;
      g +> G.add_edge (G.dupe, node) G.Has;
      env.log (spf " orig = %s" orig_file) ;
      env.log (spf " dupe = %s" ex_file);
    (* duplicating a regular function, bad, but ok, should have renamed it in
     * our analysis, see env.dupe_renaming
     *)
    | n, 1 when n > 0 -> 
      env.log (spf "DUPE BAD STYLE: %s (%d)" (G.string_of_node node) cnt);
      env.log (spf " orig = %s" orig_file) ;
      env.log (spf " dupe = %s" ex_file);
    (* probably local functions to a script duplicated in independent files,
     * most should have also been renamed, see env.dupe_renaming *)
    | n, 0 -> ()
    | _ -> raise Impossible
    )
  );
  if not only_defs then begin
    g +> G.iter_nodes (fun (str, kind) ->
      Hashtbl.replace env.case_insensitive 
        (String.lowercase str, kind) (str, kind)
    );

    (* step2: creating the 'Use' edges for inheritance *)
    env.pr2_and_log "\nstep2: extract inheritance";
    files +> Common_extra.progress ~show:verbose (fun k -> 
      List.iter (fun file ->
        k();
        let readable = Common.filename_without_leading_path root file in
        let ast = parse file in
        extract_defs_uses { env with phase = Inheritance} ast readable
      ));
    
    (* step3: creating the 'Use' edges, the uses *)
    env.pr2_and_log "\nstep3: extract uses";
    files +> Common_extra.progress ~show:verbose (fun k -> 
      List.iter (fun file ->
        k();
        let readable = Common.filename_without_leading_path root file in
        let ast = parse file in
        extract_defs_uses {env with phase = Uses} ast readable
   ));
  end;
  
  g
