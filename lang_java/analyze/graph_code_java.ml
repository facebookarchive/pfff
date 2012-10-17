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

open Ast_java
module Ast = Ast_java

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for Java. See graph_code.ml and main_codegraph.ml
 * for more information.
 * 
 * choices:
 *  - package-based or dir-based schema? Seems simpler to use packages.
 *  - merge overloaded methods? yes, alternative is to mangle the
 *    name of the method with the type (a la C++ linker)
 * 
 * schema:
 *   Package -> SubPackage -> File -> Class (TODO | Interface )
 *                                    -> Method
 *                                    -> Field
 *                                    -> Constant (static final)
 *                                    -> Constant (enum, inlined in parent)
 *                            File -> Class -> SubClass -> ...
 *                                          -> EnumSubClass (nothing)
 *   (when have no package)
 *   Dir -> Subdir -> File 
 * 
 *   PB -> Not_Found -> Package2 -> SubPackage2 -> ...
 * 
 * todo: 
 *  - handle generics
 *  - adjust graph to remove intermediate singleton? com.xxx?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = {
  g: Graph_code.graph;

  phase: phase;
  current: Graph_code.node;
  current_qualifier: Ast_java.qualified_ident;

  (* import x.y.* => [["x";"y"]; ...] *)
  imported_namespace: (string list) list;
  (* import x.y.z => [("z", (false, ["x";"y";"z"])); ...] *)
  imported_qualified: (string * (bool * Ast_java.qualified_ident)) list;

  (* This field is to avoid looking up parameters or locals in the graph.
   * We could also store them in the code graph so that the lookup
   * would work, but really fine-grained intra-method dependencies 
   * are not that useful.
   *)
  params_locals: string list;
  (* todo, to avoid looking up typenames *)
  type_params_local: string list;

  (* less: skip_edges *)
}

  (* We need 3 phases, one to get all the definitions, one to
   * get the inheritance information, and one to get all the Uses.
   * The inheritance is a kind of use, but certain uses like using
   * a field needs the full inheritance tree to already be computed
   * as we may need to lookup entities up in the parents.
   *)
  and phase = Defs | Inheritance | Uses

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let parse ~show_parse_error file =
  try 
    Parse_java.parse_program file
  with 
  | Timeout -> raise Timeout
  | exn ->
      if show_parse_error
      then pr2_once (spf "PARSE ERROR with %s, exn = %s" file 
                        (Common.exn_to_s exn));
      { package = None; imports = []; decls = [] }


let str_of_qualified_ident xs =
  xs +> List.map Ast.unwrap +> Common.join "."

let str_of_name xs = 
  xs +> List.map (fun (_tyarg_todo, ident) -> Ast.unwrap ident) +> 
    Common.join "."

(* TODO *)
let long_ident_of_name xs = List.map snd xs
(* TODO *)
let long_ident_of_class_type xs = List.map fst xs



let looks_like_class_name s =
  s =~ "[A-Z]"
let looks_like_enum_constant s =
  s =~ "^[A-Z_0-9]+$"

let rec classname_and_info_of_typ t =
  match t with
  | TBasic x -> x
  | TArray t -> classname_and_info_of_typ t
  | TClass xs ->
      let x = Common.list_last xs in
      let (ident, _args) = x in
      ident
let classname_and_charpos_of_typ t = 
  let (s, info) = classname_and_info_of_typ t in
  s, Ast.pos_of_info info

(* quite similar to create_intermediate_directories_if_not_present *)
let create_intermediate_packages_if_not_present g root xs =
  let dirs = Common.inits xs +> List.map str_of_qualified_ident in
  let dirs = 
    match dirs with
    | ""::xs -> xs
    | _ -> raise Impossible
  in

  let rec aux current xs =
    match xs with
    | [] -> ()
    | x::xs ->
        let entity = x, E.Package in
        if G.has_node entity g
        then aux entity xs
        else begin
          g +> G.add_node entity;
          g +> G.add_edge (current, entity) G.Has;
          aux entity xs
        end
  in
  aux root dirs

let rec add_use_edge env (name, kind) =
  let src = env.current in
  let dst = (name, kind) in
  (match () with
  | _ when not (G.has_node src env.g) ->
      pr2 (spf "LOOKUP SRC FAIL %s --> %s, src does not exist???"
              (G.string_of_node src) (G.string_of_node dst));

  | _ when G.has_node dst env.g -> 
      G.add_edge (src, dst) G.Use env.g

  | _ -> 
      (match kind with
      | _ ->
          let kind_original = kind in
          let dst = (name, kind_original) in
          let parent_target = G.not_found in
          (match kind_original with 
          | E.Package ->
              let fake_package = 
                (Common.split "\\." name) +> List.map (fun s -> s^"2") in
              let dst = (Common.join "." fake_package, kind_original) in
              if not (G.has_node dst env.g)
              then begin 
                create_intermediate_packages_if_not_present 
                  env.g parent_target 
                  (fake_package +> List.map (fun s -> s,()));
                pr2 (spf "PB: lookup fail on %s (in %s)" 
                        (G.string_of_node dst) (G.string_of_node src));
              end;
              env.g +> G.add_edge (src, dst) G.Use;
              ()
          | _ ->
              pr2 (spf "PB: lookup fail on %s (in %s)" 
                      (G.string_of_node dst) (G.string_of_node src));
              G.add_node dst env.g;
              env.g +> G.add_edge (parent_target, dst) G.Has;
              env.g +> G.add_edge (src, dst) G.Use;
          )
      )
  )

(*****************************************************************************)
(* Class/Package Lookup *)
(*****************************************************************************)

let (lookup_fully_qualified2: 
  Graph_code.graph -> string list -> Graph_code.node option) = 
 fun g xs ->
  let rec aux current xs =
    match xs with
    | [] -> Some current
    | x::xs ->
        let children = G.children current g in
        (* because have File intermediate (noisy) nodes *)
        let children = children +> List.map (fun child ->
          match child with
          | (_, E.File) -> G.children child g
          (* we prefer Package to Dir when we lookup, we don't want
           * The "multiple entities" warning when have both
           * a "net" package and "net" directory.
           *)
          | (_, E.Dir) -> []
          | _ -> [child]
        ) +> List.flatten
        in
        (* sanity check, quite expansive according to -profile *)
        Common.group_assoc_bykey_eff children +> List.iter (fun (k, xs) ->
          if List.length xs > 1 
             (* issue warnings lazily, only when the ambiguity concerns
              * something we are actually looking for 
              *) 
             && k =$= x
          then begin
            (* todo: this will be a problem when go from class-level
             * to method/field level dependencies
             *)
            pr2 "WARNING: multiple entities with same name";
            pr2_gen (k, xs);
          end
        );
        
        let str =
          match current with
          | ".", E.Dir -> x
          | s, _ -> s ^ "." ^ x
        in
        let new_current = 
          children +> Common.find_some_opt (fun (s2, kind) ->
            if str =$= s2
            then Some (s2, kind)
            else None
          ) in
        (match new_current with
        (* less: could return at least what we were able to resolve *)
        | None -> None
        | Some current -> aux current xs
        )
  in
  aux G.root xs

let _hmemo = Hashtbl.create 101 

let lookup_fully_qualified_memoized env x = 
  Common.profile_code "Graph_java.lookup_qualified" (fun () ->
    if env.phase = Uses || env.phase = Inheritance
    then 
      Common.memoized _hmemo x (fun () ->
        lookup_fully_qualified2 env.g x
      )
    else lookup_fully_qualified2 env.g x
  )

(* Java allows to open namespaces by for instance importing packages
 * in which case we unsugar by preprending the package name.
 * Note that extending a class also imports its namespace (and
 * of all its parents too), hence import_of_inherited_classes below.
 *)
let with_full_qualifier env xs =
  env.imported_namespace +> List.map (fun (qualified_ident) ->
    let rev = List.rev qualified_ident in
    let prefix = 
      (* todo: simplify now that have imported_qualified? *)
      match rev with
      | ("*")::rest ->
          List.rev rest
      (* todo opti: if head match the head of xs, then can accelerate things? *)
      | _ -> List.rev (List.tl rev)
    in
    prefix ++ (xs +> List.map Ast.unwrap)
  )

(* Look for entity (package/class/method/field) in list of imported
 * packages or in global scope. Return fully qualified entity.
 * 
 * Note that the code graph store nodes in fully qualified form.
 *)
let (lookup2: env -> Ast.qualified_ident -> Graph_code.node option) = 
 fun env xs ->
  let candidates = with_full_qualifier env xs in
  (* pr2_gen candidates; *)
  candidates +> Common.find_some_opt (fun full_qualifier ->
    lookup_fully_qualified_memoized env full_qualifier
  )

let lookup a b = 
  Common.profile_code "Graph_java.lookup" (fun () -> lookup2 a b)

(* pre: the Inheritance phase must have been done already
 * otherwise parents_inheritance can be empty or incomplete.
 *)
let rec import_of_inherited_classes env n =
  (* A class should Use only entities its extends or implements.
   * less: could filter out interface but needs them to store
   * then as E.Class E.Interface
   *)
  let parents_inheritance = G.succ n G.Use env.g in
  parents_inheritance +> Common.map_filter (fun (str, kind) ->
    match kind with
    | E.Class _ ->
        let xs = (Common.split "\\." str) ++ ["*"] in
        let res = import_of_inherited_classes env (str, kind) in
        Some (xs::res)
    | _ -> None
  ) +> List.flatten

(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)
(* Note that there is no ~dupe argument. Java code uses packages and 
 * fully qualified entities so there should be no name conflicts.
 *)
let rec extract_defs_uses ~phase ~g ~ast ~readable ~lookup_fails ~skip_edges =

  let env = {
    g; phase;

    (* old: (str_of_qualified_ident long_ident, E.Package).
     * We want a File node because it allows to easily find to which
     * file an entity corresponds too and open this file in emacs/codemap
     * when one click somewhere in codegraph.
     *)
    current = readable, E.File;
    current_qualifier =
      (match ast.package with
      | None -> []
      | Some long_ident -> long_ident
      );
    params_locals = [];
    type_params_local = [];
    imported_namespace = 
      (match ast.package with
      (* we automatically import the current.package.* *)
      | Some long_ident -> [List.map Ast.unwrap long_ident ++ ["*"]]
      | None -> []
      ) ++ 
     (ast.imports +> List.map (fun (is_static, qualified_ident) ->
       List.map Ast.unwrap qualified_ident
     ) ++ [
       (* we automatically import java.lang.* *)
       ["java";"lang";"*"];
       (* we automatically import top packages *)
       ["*"]
     ]
     );
    imported_qualified = ast.imports +> Common.map_filter (fun (is_static, xs)->
      match List.rev xs with
      | [] -> raise Impossible
      | ["*", _] -> None
      | (s, _)::rest -> Some (s, (is_static, xs))
    );
  }
  in

  if phase = Defs then begin
    match ast.package with
    (* have None usually for scripts, tests, or entry points *)
    | None ->
        let dir = Common.dirname readable in
        G.create_intermediate_directories_if_not_present g dir;
        g +> G.add_node (readable, E.File);
        g +> G.add_edge ((dir, E.Dir), (readable, E.File))  G.Has;

    | Some long_ident ->
        create_intermediate_packages_if_not_present g G.root long_ident;
        let str = str_of_qualified_ident long_ident in
        (* we keep a File node because it allows from an entity
         * to know where it's defined by going up the chain of parents
         * until a File is found
         *)
        g +> G.add_node (readable, E.File);
        g +> G.add_edge ((str, E.Package), (readable, E.File)) G.Has;
  end;
  (* TODO: if phase is Uses then can double check if can find
   * some of the imports? especially useful when have a better
   * java_stdlib/ to report third-party not yet handled packages.
   *)

  (* imports is not the only way to use external packages, one can
   * also just qualify the classname or static method so we need
   * to visit the AST and lookup classnames (possibly using information
   * from the import to know where to look for first).
   *)
  decls env ast.decls

(* ---------------------------------------------------------------------- *)
(* Declarations (classes, fields, etc) *)
(* ---------------------------------------------------------------------- *)
and decl env = function
  | Class def, _ -> class_decl env def
  | Method def, _ -> method_decl env def
  | Field def, _ -> field_decl env def
  | Enum def, _ -> enum_decl env def
  | Init (_is_static, st), n ->
      let name = spf "__init__%d" n in
      let full_ident = env.current_qualifier ++ [name, fakeInfo name] in
      let full_str = str_of_qualified_ident full_ident in
      if env.phase = Defs then begin
        env.g +> G.add_node (full_str, E.TopStmts);
        env.g +> G.add_edge (env.current, (full_str, E.TopStmts)) G.Has;
      end;
      let env = { env with
        current = (full_str, E.TopStmts);
        current_qualifier = full_ident;
      } 
      in
      stmt env st

and decls env xs = List.iter (decl env) (Common.index_list_1 xs)

and class_decl env def =
  let full_ident = env.current_qualifier ++ [def.cl_name] in
  let full_str = str_of_qualified_ident full_ident in
  if env.phase = Defs then begin
    (* less: def.c_type? *)
    env.g +> G.add_node (full_str, E.Class E.RegularClass);
    env.g +> G.add_edge (env.current, (full_str, E.Class E.RegularClass)) G.Has;
  end;
  let env = { env with
    current = (full_str, E.Class E.RegularClass);
    current_qualifier = full_ident;
    params_locals = [];
    (* TODO *)
    type_params_local = [];
  }
  in
  let parents = 
    Common.option_to_list def.cl_extends ++
    (def.cl_impls)
  in
  List.iter (typ env) parents;

  let imports = 
    if env.phase = Defs then []
    else 
    (* Java allows programmer to use fields without qualifying them
     * (without a class.xxx, or this.xxx) so we need to unsugar this
     * by prepending the full current classname. We can just
     * generate a fake import package.classname.*. This will also
     * allow nested classes to access siblings.
     *)
     (List.map Ast.unwrap full_ident ++ ["*"]) ::
    import_of_inherited_classes env (full_str, E.Class E.RegularClass)
  in
  decls {env with imported_namespace = imports ++ env.imported_namespace } 
    def.cl_body

(* Java allow some forms of overloading, so the same method name can be
 * used multiple times.
 *)
and method_decl env def =

  let full_ident = env.current_qualifier ++ [def.m_var.v_name] in
  let full_str = str_of_qualified_ident full_ident in
  if env.phase = Defs then begin
    (* less: static? *)
    (* less: for now we just collapse all methods with same name together *)
    if G.has_node (full_str, E.Method E.RegularMethod) env.g
    then ()
    else begin
      env.g +> G.add_node (full_str, E.Method E.RegularMethod);
      env.g +> G.add_edge (env.current, (full_str, E.Method E.RegularMethod)) G.Has;
    end
  end;
  let env = { env with
    current = (full_str, E.Method E.RegularMethod);
    (* No change to the qualifier? methods are not a namespace?
     * Hmm but can have nested classes inside a methods that
     * share the same name so yes need full_ident as a qualifier.
    *)
    current_qualifier = full_ident;
    params_locals = def.m_formals +> List.map (fun v -> Ast.unwrap v.v_name);
    (* TODO *)
    type_params_local = [];
  } 
  in
  var env def.m_var;
  List.iter (var env) def.m_formals;
  (* todo: m_throws *)
  stmt env def.m_body


and field_decl env def =
  let full_ident = env.current_qualifier ++ [def.f_var.v_name] in
  let full_str = str_of_qualified_ident full_ident in
  let kind = 
    if Ast.is_final_static def.f_var.v_mods
    then E.Constant
    else E.Field
  in
  if env.phase = Defs then begin
    (* less: static? *)
    env.g +> G.add_node (full_str, kind);
    env.g +> G.add_edge (env.current, (full_str, kind)) G.Has;
  end;
  let env = { env with
    current = (full_str, kind);
    current_qualifier = env.current_qualifier
  } 
  in
  field env def

and enum_decl env def =
  let full_ident = env.current_qualifier ++ [def.en_name] in
  let full_str = str_of_qualified_ident full_ident in
  if env.phase = Defs then begin
    (* less: make it a class? or a Type? *)
    env.g +> G.add_node (full_str, E.Class E.RegularClass);
    env.g +> G.add_edge (env.current, (full_str, E.Class E.RegularClass)) G.Has;
  end;
  let env = { env with
    current = (full_str, E.Class E.RegularClass);
    current_qualifier = full_ident;
    params_locals = [];
    (* TODO *)
    type_params_local = [];
  } 
  in
  let parents = (def.en_impls) in
  List.iter (typ env) parents;
  let (csts, xs) = def.en_body in
  decls env xs;

  csts +> List.iter (fun enum_constant ->

    let ident = 
      match enum_constant with
      | EnumSimple id | EnumConstructor (id, _) | EnumWithMethods (id, _) -> id
    in
    let full_ident = env.current_qualifier ++ [ident] in
    let full_str = str_of_qualified_ident full_ident in
    if env.phase = Defs then begin
      env.g +> G.add_node (full_str, E.Constant);
      env.g +> G.add_edge (env.current, (full_str, E.Constant)) G.Has;
    end;
    let env = { env with
      current = (full_str, E.Constant);
      current_qualifier = full_ident;
    }
    in
    (match enum_constant with
    | EnumSimple ident -> ()
    | EnumConstructor (ident, args) -> 
        exprs env args
    | EnumWithMethods (ident, xs) -> 
        decls env (xs +> List.map (fun x -> Method x))
    )
  )



(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)
(* mostly boilerplate, control constructs don't introduce entities *)
and stmt env = function
  | Empty -> ()
  | Block xs -> stmts env xs
  | Expr e -> expr env e
  | If (e, st1, st2) ->
      expr env e;
      stmt env st1;
      stmt env st2;
  | Switch (e, xs) ->
      expr env e;
      xs +> List.iter (fun (cs, sts) -> 
        cases env cs;
        stmts env sts
      )
  | While (e, st) ->
      expr env e;
      stmt env st;
  | Do (st, e) ->
      expr env e;
      stmt env st;
  | For (x, st) ->
      let env = 
        match x with
        | Foreach (v, e) -> 
            var env v;
            expr env e;
            { env with
              params_locals = (Ast.unwrap v.v_name):: env.params_locals;
            } 
            
        | ForClassic (init, es1, es2) ->
            (match init with
            | ForInitExprs es0 ->
                exprs env (es0 ++ es1 ++ es2);
                env
            | ForInitVars xs ->
                List.iter (field env) xs;
                let env = { env with
                  params_locals = xs +> List.map (fun fld ->
                    Ast.unwrap fld.f_var.v_name
                  ) ++ env.params_locals;
                } 
                in
                exprs env (es1 ++ es2);
                env
            )
      in
      stmt env st;

  (* could have an entity and dependency ... but it's intra procedural
   * so not that useful
   *)
  | Label (_id, st) -> stmt env st
  | Break _idopt | Continue _idopt -> ()
  | Return eopt -> exprs env (Common.option_to_list eopt)
  | Sync (e, st) ->
      expr env e;
      stmt env st;
  | Try (st, xs, stopt) ->
      stmt env st;
      catches env xs;
      stmts env (Common.option_to_list stopt);
  | Throw e -> expr env e
  | Assert (e, eopt) ->
      exprs env (e::Common.option_to_list eopt)
  (* The modification of env.params_locals is done in decls() *)
  | LocalVar f -> field env f
  | LocalClass def -> class_decl env def

and stmts env xs = 
  let rec aux env = function
    | [] -> ()
    | x::xs -> 
        stmt env x;
        let env = 
          match x with
          | LocalVar fld -> 
              let str = Ast.unwrap fld.f_var.v_name in
              { env with params_locals = str::env.params_locals }
          (* also add LocalClass case? no, 'lookup env ...' handles that *)
          | _ -> env
        in
        aux env xs
  in 
  aux env xs

and cases env xs = List.iter (case env) xs
and case env = function
  | Case e -> expr env e
  | Default -> ()

and catches env xs = List.iter (catch env) xs
and catch env (v, st) =
  var env v;
  let str = Ast.unwrap v.v_name in
  let env = { env with params_locals = str::env.params_locals } in
  stmt env st

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env = function
  (* main dependency source! *)
  | Name n ->
      if env.phase = Uses then begin
        let str = str_of_name n in
        (match str, n with
        (* TODO: look at the type and continue lookup *)
        | _, (_,(s,_))::rest when List.mem s env.params_locals -> ()
        (* TODO *)
        | "super", _ | "this", _ -> 
            ()
        | _ -> 
            (match lookup env (long_ident_of_name n) with
            | Some n2 -> 
                add_use_edge env n2
            | None ->
                (match n with
                | [] -> 
                    pr2 "Name is empty??";
                    pr2_gen (env.current, n);
                    raise Impossible
                | (_, (s,_))::_ when List.mem_assoc s env.imported_qualified ->
                    let (is_static, full_ident) = 
                      List.assoc s env.imported_qualified in
                    let str = str_of_qualified_ident full_ident in
                    add_use_edge env (str, E.Package)

                | [x] when looks_like_enum_constant str -> 
                    pr2 ("PB: " ^ Common.dump n);
                | [x] when looks_like_class_name str ->
                    add_use_edge env (str, E.Package)
                | [x] -> 
                    pr2 ("PB: " ^ Common.dump n);
                    (* env.imported_namespace +> List.iter pr2_gen; *)
                | x::y::xs ->
                    (* unknown package probably *)
                    add_use_edge env (str, E.Package)
                )
            )
        )
      end

  | Literal _ -> ()

  | ClassLiteral t -> typ env t
  | NewClass (t, args, decls_opt) ->
      typ env t;
      exprs env args;
      (match decls_opt with
      | None -> ()
      | Some xs ->
          let classname, charpos = classname_and_charpos_of_typ t in
          let anon_class = spf "__anon__%s__%d" classname charpos in
          let full_ident = env.current_qualifier ++ [anon_class, fakeInfo ""] in
          let full_str = str_of_qualified_ident full_ident in
          if env.phase = Defs then begin
            env.g +> G.add_node (full_str, E.Class E.RegularClass);
            env.g +> G.add_edge (env.current,(full_str,E.Class E.RegularClass))
              G.Has;
          end;
          let env = { env with
            current = (full_str, E.Class E.RegularClass);
            current_qualifier = full_ident;
          }
          in
          decls env xs
      )
  | NewQualifiedClass (e, id, args, decls_opt) ->
      (*
      pr2 "NewQualifiedClass";
      pr2_gen (NewQualifiedClass (e, id, args, decls_opt));
      *)
      (* todo: need to resolve the type of 'e' *)
      expr env (NewClass (TClass ([id, []]), args, decls_opt))

  | NewArray (t, args, i, ini_opt) ->
      typ env t;
      exprs env args;
      init_opt env ini_opt

  | Call (e, es) ->
      expr env e;
      exprs env es
  | Dot (e, idTODO) ->
      (* todo: match e, and try lookup method/field
       * if e is a Name, lookup it, and if a class then
       * lookup children. If local ... then need get its type
       * lookup its node, and then lookup children.
       *)
      expr env e;

  | ArrayAccess (e1, e2) -> exprs env [e1;e2]
  | Postfix (e, op) | Prefix (op, e) -> expr env e
  | Infix (e1, op, e2) -> exprs env [e1;e2]
  | Conditional (e1, e2, e3) -> exprs env [e1;e2;e3]
  | Assignment (e1, op, e2) -> exprs env [e1;e2]

  | Cast (t, e) -> 
      typ env t;
      expr env e
  | InstanceOf (e, tref) ->
      expr env e;
      typ env (tref);
      
      
      


and exprs env xs = List.iter (expr env) xs
and init env = function
  | ExprInit e -> expr env e
  | ArrayInit xs -> List.iter (init env) xs
and init_opt env opt =
  match opt with
  | None -> ()
  | Some ini -> init env ini

(* ---------------------------------------------------------------------- *)
(* Types *)
(* ---------------------------------------------------------------------- *)
and typ env = function
  | TBasic _ -> ()
  | TArray t -> typ env t
  (* other big dependency source! *)
  | TClass reft ->
      (* todo: let's forget generic arguments for now *)
      let xs = long_ident_of_class_type reft in
      let str = str_of_qualified_ident xs in
      if env.phase = Uses || env.phase = Inheritance then begin
        (match lookup env xs with
        (* TODO: look in type_params_local ! *)
        | Some n2 -> 
            (* pr2 ("FOUND: " ^ Common.dump n); *)
            add_use_edge env n2
        | None ->
            (match xs with
            | [] -> raise Impossible
            | ((s,_))::_ when List.mem_assoc s env.imported_qualified ->
                let (is_static, full_ident) = 
                  List.assoc s env.imported_qualified in
                let str = str_of_qualified_ident full_ident in
                add_use_edge env (str, E.Package)

            | [x] -> 
                if looks_like_class_name str
                then add_use_edge env (str, E.Package)
                else 
                  pr2 ("PB: " ^ Common.dump reft);
            | x::y::xs ->
                (* unknown package probably *)
                add_use_edge env (str, E.Package)
            )
        )
      end
      

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)
and var env v =
  typ env v.v_type;
  ()

and field env f =
  var env f.f_var;
  init_opt env f.f_init;
  ()

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) dir skip_list =
  let root = Common.realpath dir in
  let all_files = Lib_parsing_java.find_source_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files ~verbose skip_list root all_files in

  let g = G.create () in
  G.create_initial_hierarchy g;

  let lookup_fails = Common.hash_with_default (fun () -> 0) in
  let skip_edges = skip_list +> Common.map_filter (function
    | Skip_code.Edge (s1, s2) -> Some (s1, s2)
    | _ -> None
  ) +> Common.hash_of_list 
  in

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
  files +> Common_extra.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let readable = Common.filename_without_leading_path root file in
      let ast = parse ~show_parse_error:true file in
     extract_defs_uses ~phase:Defs ~g ~ast ~readable 
       ~lookup_fails ~skip_edges;
    ));

  (* step2: creating the 'Use' edges just for inheritance *)
  if verbose then pr2 "\nstep2: extract inheritance information";
  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let readable = Common.filename_without_leading_path root file in
     let ast = parse ~show_parse_error:false file in
     extract_defs_uses ~phase:Inheritance ~g ~ast ~readable
       ~lookup_fails ~skip_edges;
   ));

  (* step3: creating the 'Use' edges that can rely on recursive inheritance *)
  if verbose then pr2 "\nstep3: extract uses";
  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let readable = Common.filename_without_leading_path root file in
     let ast = parse ~show_parse_error:false file in
     extract_defs_uses ~phase:Uses ~g ~ast ~readable
       ~lookup_fails ~skip_edges;
   ));
  g
