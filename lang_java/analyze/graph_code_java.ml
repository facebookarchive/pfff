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

module E = Entity_code
module G = Graph_code

open Ast_java
module Ast = Ast_java
module PI = Parse_info
module V = Visitor_java

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
 *   Package -> SubPackage -> Class (TODO | Interface )
 *                                    -> Method
 *                                    -> Field
 *                                    -> Constant (static final)
 *                                    -> Constant (enum, inlined in parent)
 *                            Class -> SubClass -> ...
 *                                          -> EnumSubClass (nothing)
 *   (when have no package)
 *   Dir -> Subdir -> File -> Class
 *
 *   PB -> Not_Found -> Package2 -> SubPackage2 -> ...
 *
 * note: adjust graph to remove intermediate singleton? com.xxx? Hmm better
 * to do that lazily in codegraph itself.
 *
 * note: doing codegraph for Java helps evaluate the number of lookup failures
 * in projects, and which code one needs to include to fully analyze the code.
 * If I go in the abstract interpreter path that julien took where he analyzed
 * code but had so many Not_found, Todo, exn, then I'll have no confidence
 * at all. So:
 *
 * - DONE lookup package correctly
 * - SEMI lookup classes correctly
 * - lookup field/methods correctly
 *
 * It also helps to find bug in the parser and better understand
 * Java and the AST :) e.g. Name -> Dot ambiguities.
 * It also helps to see which code is needed to fully analyze our code.
 *
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = {
  g: Graph_code.graph;

  phase: phase;
  current: Graph_code.node;
  current_qualifier: Ast_java.qualified_ident;
  (*Track class classifiers, for mapping nodes *)
  top_level_qualifer: Ast_java.qualified_ident;
  (* import x.y.* => [["x";"y"]; ...] *)
  imported_namespace: (string list) list;
  (* import x.y.z => [("z", (false, ["x";"y";"z"])); ...] *)
  imported_qualified: (string * (bool * Ast_java.qualified_ident)) list;

  (* This field is to avoid looking up parameters or locals in the graph.
   * We could also store them in the code graph so that the lookup
   * would work, but really fine-grained intra-method dependencies
   * are not that useful.
   *
   * The boolean final is because such locals/parameters should be
   * passed to anonymouse classes.
   *)
  params_or_locals: (string * bool (* is_final *)) list;
  (* To avoid looking up type parameters in the graph. *)
  type_parameters: string list;
}

  (* We need 3 phases, one to get all the definitions, one to
   * get the inheritance information, and one to get all the Uses.
   * The inheritance is a kind of use, but certain uses like using
   * a field needs the full inheritance tree to already be computed
   * as we may need to lookup entities up in the parents.
   *)
  and phase = Defs | Inheritance | Uses | MethodToMethod

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let rec iter_until_true ~f list_ =
  match list_ with
  | [] -> None
  | hd::tl -> 
      match f hd with 
      | false -> iter_until_true ~f tl
      | true -> Some hd

let rec fold_inner ~acc ~f ~x =
  (match x with
  | [] -> acc
  | hd::tl -> fold_inner ~acc:(f acc hd) ~f:f ~x:tl
  )

let fold x_list ~init ~f =
  fold_inner ~acc:init ~x:x_list ~f:f

let last = function
  | hd::tl -> List.fold_left (fun _ y -> y) hd tl
  | [] -> failwith "no element"

 let join_list ~sep a=
  let a =
     (match a with
     | "this"::tl -> tl
     | _ -> a)
   in
   let aux = fold ~init:"" ~f:(fun a b -> (Common.join sep [a;b])) in
    (match a with
    | [] -> ""
    | [a] -> a 
    | hd::tl -> hd ^ (aux tl)
    )

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

(*let str_of_class_type xs =*)
  (*xs +> List.map (fun (ident, _tyarg_todo) -> Ast.unwrap ident) +>*)
    (*Common.join "."*)

(* Same as str_of_qualified_ident except neglects super and this     *)
(*
 *let str_of_name_this_super xs=
 *        (match xs with
 *        | (_,hd)::tl ->  (match Ast.unwrap hd with
 *                     | "super"
 *                     | "this" -> str_of_name tl
 *                     | _ -> str_of_name xs
 *                     )
 *        | [] -> "")
 *
 *)

(* helper to build entries in env.params_or_locals *)
let p_or_l v =
  Ast.unwrap v.v_name, Ast.is_final v.v_mods

(* TODO *)
let long_ident_of_name xs = List.map snd xs
(* TODO *)
let long_ident_of_class_type xs = List.map fst xs

let nodeinfo ident =
  { G.pos = Parse_info.token_location_of_info (Ast.info_of_ident ident);
    props = [];
    typ = None;
  }


let looks_like_class_name s =
  s =~ "[A-Z]"
let looks_like_enum_constant s =
  s =~ "^[A-Z_0-9]+$"

let rec classname_and_info_of_typ t =
  match t with
  | TBasic x -> x
  | TArray t -> classname_and_info_of_typ t
  | TClass xs ->
      let x = Common2.list_last xs in
      let (ident, _args) = x in
      ident

(* quite similar to create_intermediate_directories_if_not_present *)
let create_intermediate_packages_if_not_present g root xs =
  let dirs = Common2.inits xs +> List.map str_of_qualified_ident in
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

let add_use_edge env (name, kind) =
  let src = env.current in
  let dst = (name, kind) in
  (match () with
  | _ when not (G.has_node src env.g) ->
                  if env.phase = MethodToMethod then 
                    pr2 (spf "MTM: (add_use_edge) lookup fail on %s" name)  
                  else
                    pr2 (spf "LOOKUP SRC FAIL %s --> %s, src does not exist???" (G.string_of_node src) (G.string_of_node dst));
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
              match env.phase with
              | MethodToMethod -> 
                  pr2 (spf "MTM: (add_use_edge) lookup fail on %s (in %s)" 
                  (G.string_of_node dst) (G.string_of_node src))
              | _ ->
                  pr2 (spf "PB: lookup fail on %s (in %s)" (G.string_of_node dst) (G.string_of_node src));
                  G.add_node dst env.g; 
                  env.g +> G.add_edge (parent_target, dst) G.Has;
                  env.g +> G.add_edge (src, dst) G.Use;
          )
      )
  )


(*
 *Depth first search, checks which class path has the method called in the current
 *node's successors
 *)
let dfs ?(verbose=false) ~env  ~node ~node_str ~get_edges ~f =
  let printer = ref [""] in
  let full_str = (str_of_qualified_ident env.top_level_qualifer) ^"." ^
        node_str in
  if verbose = true then
    begin
    pr (spf "dfs on str: %s, %s" full_str node_str);
    pr (spf "Current method/class/fied: %s" (str_of_qualified_ident env.current_qualifier ^"."^node_str));
    end; 
    let f_imported_namespace_check =
    iter_until_true ~f:(fun a-> f ((join_list ~sep:"." a) ^ "." ^ node_str)) 
    env.imported_namespace
    in
    let f_imported_qualifier_check =
      iter_until_true ~f:(fun (a,(_,b))-> f (a ^ "."^(str_of_qualified_ident b)
      ^"." ^ node_str)) 
      env.imported_qualified 
    in
(*
    pr "Imported qualified";
    pr (match f_imported_qualifier_check with
    | Some (str,_) -> str
    | None -> "None"
    );
*)
    (*
     *let _str = 
     *  match env.imported_qualified with
     *  | [] -> ""
     *  | (hd,(_,b))::_ -> hd ^"."^ (str_of_qualified_ident b)^"."^node_str
     *in
     *pr _str;
     *)
let op =
  (match (f full_str, f node_str, f_imported_namespace_check , f_imported_qualifier_check) with
  | (false,false,Some str, _) -> 
      if verbose = true then
        printer.contents <- printer.contents @ ["Imported namespace edge drawn"];
      Some (join_list ~sep:"." str ^"."^node_str)
  | (false, false, _ , Some (str,_)) ->
      if verbose = true then
        printer.contents <- printer.contents @[ "Imported qualifier edge drawn"];
      Some ( str ^"." ^ node_str) 
  | (true,_,_,_) -> 
      if verbose = true then  
        printer.contents <- printer.contents @ ["Fully qualified "]; Some full_str
  | (_,true,_,_) -> 
      if verbose = true then
        printer.contents <- printer.contents @ ["Method name as is"]; Some node_str
  | (false, false,None, None) ->
          let rec aux  ~verbose ~node_str ~d ~list_ ~get_edges ~f =
(*             Maximum depth that the funtion searched uptil *)
            if (d < 10) then 
              begin
                (match list_ with
                | [] -> None
                | hd::tl -> 
                    let node_str_hd = (fst hd) ^ "." ^ node_str in
                    if verbose = true then
                      printer.contents <- printer.contents @ [node_str_hd];
              (match f node_str_hd with 
              | true -> 
                  Some node_str_hd 
              | false ->       
                  let node_list = get_edges ~n:hd in
                  (match aux ~verbose ~node_str:node_str_hd ~d:(d+1) ~list_:node_list ~get_edges:get_edges ~f:f with
                  | None -> aux ~verbose ~d:d ~node_str:node_str ~list_:tl ~get_edges:get_edges ~f:f 
                  | Some x -> Some x
                  )
              )
              )
                              end
        else
                None
                  in
                  (match aux ~verbose ~d:0 ~node_str:node_str ~list_:(get_edges
                  ~n:node) ~get_edges:get_edges ~f with
                  | Some x -> printer.contents <- printer.contents @ ["dfs, node existing"]; Some x
                  | None -> None
                  )
  )
in
 (op, printer)

(*****************************************************************************)
(* Class/Package Lookup *)
(*****************************************************************************)

let _hmemo = Hashtbl.create 101

let lookup_fully_qualified_memoized env x =
  Common.profile_code "Graph_java.lookup_qualified" (fun () ->
    if env.phase = Uses || env.phase = Inheritance
    then
      Common.memoized _hmemo x (fun () ->
        Package_java.lookup_fully_qualified2 env.g x
      )
    else Package_java.lookup_fully_qualified2 env.g x
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
    prefix @ (xs +> List.map Ast.unwrap)
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
    | E.Class ->
        let xs = (Common.split "\\." str) @ ["*"] in
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
let rec extract_defs_uses ~phase ~g ~ast ~readable ~lookup_fails =
  ignore(lookup_fails);

  let env = {
    g; phase; 

    current =
     (match ast.package with
     | Some long_ident -> (str_of_qualified_ident long_ident, E.Package)
     | None ->            (readable, E.File)
     );
    current_qualifier =
      (match ast.package with
      | None -> []
      | Some long_ident -> long_ident
      );
      top_level_qualifer =
      (match ast.package with
      | None -> []
      | Some long_ident -> long_ident
      );
    params_or_locals = [];
    type_parameters = [];
    imported_namespace =
      (match ast.package with
      (* we automatically import the current.package.* *)
      | Some long_ident -> [List.map Ast.unwrap long_ident @ ["*"]]
      | None -> []
      ) @
     (ast.imports +> List.map (fun (_is_static, qualified_ident) ->
       List.map Ast.unwrap qualified_ident
     ) @ [
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
      | (s, _)::_rest -> Some (s, (is_static, xs))
    );
  }
  in

  if phase = Defs then begin
    match ast.package with
    (* have None usually for scripts, tests, or entry points *)
    | None ->
(*                     pr "None --\n"; *)
        let dir = Common2.dirname readable in
        G.create_intermediate_directories_if_not_present g dir;
        pr "Create defs";
        pr readable;
        g +> G.add_node (readable, E.File);
        g +> G.add_edge ((dir, E.Dir), (readable, E.File))  G.Has;
    | Some long_ident ->
(*                     pr "Some long_indent\n"; *)
                    create_intermediate_packages_if_not_present g G.root long_ident;
(*                     pr "End Some long indent\n"; *)
  end;
  (* double check if we can find some of the imports
   * (especially useful when have a better java_stdlib/ to report
   * third-party packages not-yet handled).
   *)
  if phase = Inheritance then begin
(*           pr "Phase inheritance \n"; *)
    ast.imports +> List.iter (fun (_is_static, qualified_ident) (* Replaced
    is_static with _is_static *)->
      let qualified_ident_bis =
        match List.rev qualified_ident with
        | ("*",_)::rest -> List.rev rest
        (* less: just lookup the class for now *)
        | _x::xs when true-> List.rev xs (* Replaced is_static with true *)
        | _ -> qualified_ident
      in
      let entity = List.map Ast.unwrap qualified_ident_bis in
      (match lookup_fully_qualified_memoized env entity with
      | Some _ ->
        (* no need add_use_edge here, it will be done later when
         * one use the entity
         * less: could be used to detect useless import
         *)
         ()
      | None ->
        pr2_once (spf "PB: wrong import: %s"
                    (str_of_qualified_ident qualified_ident_bis))
      )
    );
  end;

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
  | Class def, _ ->
(*                   pr "Class def \n";  *)
                  class_decl env def
  | Method def, _ ->
(*                   pr "method def\n";  *)
                  method_decl env def
  | Field def, _ -> 
(*                   pr "Def\n";  *)
                  field_decl env def
  | Enum def, _ -> 
(*                   pr "Enum\n"; *)
                  enum_decl env def
  | Init (_is_static, st), n ->
(*                   pr "Init\n"; *)
      let name = spf "__init__%d" n in
      let full_ident = env.current_qualifier @ [name, fakeInfo name] in
      let full_str = str_of_qualified_ident full_ident in
      let node = (full_str, E.TopStmts) in
      if env.phase = Defs then begin
        env.g +> G.add_node node;
        env.g +> G.add_edge (env.current, node) G.Has;
      end;
      let env = { env with
        current = node;
        current_qualifier = full_ident;
      }
      in
      stmt env st

and decls env xs = 
(*         pr "Decls env xs \n"; *)
        List.iter (decl env) (Common.index_list_1 xs);
(*         pr "Done with decls env xs" *)

and class_decl env def =
(*         pr "Class decl env def\n"; *)
  let full_ident = env.current_qualifier @ [def.cl_name] in
  let full_str = str_of_qualified_ident full_ident in
  let node = (full_str, E.Class) in
(*   pr "env.phase = Defs\n "; *)
  if env.phase = Defs then begin
    (* less: def.c_type? *)
       begin  
(*                pr "Inside begin block\n"; *)
               if not (G.has_node node env.g) then
                       begin
(*                   pr "Not existing"; *)
(*                   pr "This is not in the if block"; *)
          env.g +> G.add_node node;
          env.g +> G.add_nodeinfo node (nodeinfo def.cl_name);
          env.g +> G.add_edge (env.current, node) G.Has;
(*           pr "This is not the error" *)
                       end;
       end;
  end;
(*   pr "End adding nodes\n"; *)
  let env = { env with
    current = node;
    current_qualifier = full_ident;
    top_level_qualifer = full_ident;
    (* with anon classes we need to lookup enclosing final parameters/locals *)
    params_or_locals = env.params_or_locals +> List.filter (fun (_x,b) -> b);
    type_parameters = def.cl_tparams +> List.map (function
    | TParam ((str,_tok), _constraints) -> str
    );
  }
  in
(*   pr "parents operation \n"; *)
  let parents =
    Common2.option_to_list def.cl_extends @
    (def.cl_impls)
  in
(*   pr "List iter \n"; *)
  List.iter (typ env) parents;
(*   pr "imports \n"; *)
  let imports =
    if env.phase = Defs then []
    else
    (* Java allows programmer to use fields without qualifying them
     * (without a class.xxx, or this.xxx) so we need to unsugar this
     * by prepending the full current classname. We can just
     * generate a fake import package.classname.*. This will also
     * allow nested classes to access siblings.
     *)
     (List.map Ast.unwrap full_ident @ ["*"]) ::
    import_of_inherited_classes env (full_str, E.Class)
  in
  decls {env with imported_namespace = imports @ env.imported_namespace }
    def.cl_body

(* Java allow some forms of overloading, so the same method name can be
 * used multiple times.
 *)
and method_decl env def =

  let full_ident = env.current_qualifier @ [def.m_var.v_name] in
  let full_str = str_of_qualified_ident full_ident in
  let node = (full_str, E.Method) in
  if env.phase = Defs then begin
    (* less: static? *)
    (* less: for now we just collapse all methods with same name together *)
    if G.has_node (full_str, E.Method) env.g
    then ()
    else begin
            (*pr "Print Method";*)
            (*pr full_str;*)
            (*pr "----";*)
      env.g +> G.add_node node;
      env.g +> G.add_nodeinfo node (nodeinfo def.m_var.v_name);
      env.g +> G.add_edge (env.current, node) G.Has;
    end
  end;
  let env = { env with
    current = node;
    (* No change to the qualifier? methods are not a namespace?
     * Hmm but can have nested classes inside a methods that
     * share the same name so yes need full_ident as a qualifier.
    *)
    current_qualifier = full_ident;
    params_or_locals = (def.m_formals +> List.map p_or_l)
      @
     (* with methods of anon classes we need to lookup enclosing
      * final parameters/locals
      *)
     (env.params_or_locals +> List.filter (fun (_x,b) -> b));

    (* TODO use m_tparams *)
    type_parameters = [];
  }
  in
  var env def.m_var;
  List.iter (var env) def.m_formals;
  (* todo: m_throws *)
  stmt env def.m_body

and field_decl env def =
  let full_ident = env.current_qualifier @ [def.f_var.v_name] in
  let full_str = str_of_qualified_ident full_ident in
  let kind =
    if Ast.is_final_static def.f_var.v_mods
    then E.Constant
    else E.Field
  in
  let node = (full_str, kind) in
  if env.phase = Defs then begin
    (* less: static? *)
          if not (G.has_node node env.g)
          then
                  begin
    env.g +> G.add_node node;
    env.g +> G.add_nodeinfo node (nodeinfo def.f_var.v_name);
    env.g +> G.add_edge (env.current, node) G.Has;
                  end
            else pr2 (spf "Package: %s already existing" (G.string_of_node node)) 
  end;
  let env = { env with
    current = node;
    current_qualifier = env.current_qualifier
  }
  in
  field env def

and enum_decl env def =
  let full_ident = env.current_qualifier @ [def.en_name] in
  let full_str = str_of_qualified_ident full_ident in
    (* less: make it a class? or a Type? *)
  let node = (full_str, E.Class) in
  if env.phase = Defs then 
          begin
(*           pr "Enum decl env.phases = Def"; *)
          if not (G.has_node node env.g) 
          then
                  begin
    env.g +> G.add_node node;
    env.g +> G.add_nodeinfo node (nodeinfo def.en_name);
    env.g +> G.add_edge (env.current, node) G.Has;
                  end
          else begin pr2 (spf "Package: %s already existing" (G.string_of_node
          node)) end
                  end;
  let env = { env with
    current = node;
    current_qualifier = full_ident;
    params_or_locals = [];
    (* TODO *)
    type_parameters = [];
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
    let full_ident = env.current_qualifier @ [ident] in
    let full_str = str_of_qualified_ident full_ident in
    let node = (full_str, E.Constant) in
    if env.phase = Defs then begin
            if not (G.has_node node env.g) then
                    begin
      env.g +> G.add_node node;
      env.g +> G.add_nodeinfo node (nodeinfo ident);
      env.g +> G.add_edge (env.current, node) G.Has;
                    end
            else  begin pr2 (spf ": %s already existing"
            (G.string_of_node node)) end
    end;
    let env = { env with
      current = node;
      current_qualifier = full_ident;
    }
    in
    (match enum_constant with
    | EnumSimple _ident -> ()
    | EnumConstructor (_ident, args) ->
        exprs env args
    | EnumWithMethods (_ident, xs) ->
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
              params_or_locals = p_or_l v :: env.params_or_locals;
            }

        | ForClassic (init, es1, es2) ->
            (match init with
            | ForInitExprs es0 ->
                exprs env (es0 @ es1 @ es2);
                env
            | ForInitVars xs ->
                List.iter (field env) xs;
                let env = { env with
                  params_or_locals =
                    (xs +> List.map (fun fld -> p_or_l fld.f_var)
                    ) @ env.params_or_locals;
                }
                in
                exprs env (es1 @ es2);
                env
            )
      in
      stmt env st;

  (* could have an entity and dependency ... but it's intra procedural
   * so not that useful
   *)
  | Label (_id, st) -> stmt env st
  | Break _idopt | Continue _idopt -> ()
  | Return eopt -> exprs env (Common2.option_to_list eopt)
  | Sync (e, st) ->
      expr env e;
      stmt env st;
  | Try (st, xs, stopt) ->
      stmt env st;
      catches env xs;
      stmts env (Common2.option_to_list stopt);
  | Throw e -> expr env e
  | Assert (e, eopt) ->
      exprs env (e::Common2.option_to_list eopt)
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
              { env with
                params_or_locals = p_or_l fld.f_var :: env.params_or_locals }
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
  let env = { env with params_or_locals = p_or_l v :: env.params_or_locals } in
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
        | _, (_,(s,_))::_rest when List.mem_assoc s env.params_or_locals -> ()
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
                    let (_is_static, full_ident) =
                      List.assoc s env.imported_qualified in
                    let str = str_of_qualified_ident full_ident in
                    add_use_edge env (str, E.Package)

                | [_x] when looks_like_enum_constant str ->
                    pr2 ("PB: " ^ Common.dump n);
                | [_x] when looks_like_class_name str ->
                    add_use_edge env (str, E.Package)
                | [_x] ->
                    pr2 ("PB: " ^ Common.dump n);
                    (* env.imported_namespace +> List.iter pr2_gen; *)
                | _x::_y::_xs ->
                    (* unknown package probably *)
                    add_use_edge env (str, E.Package)
                )
            )
        )
      end
  | NameOrClassType _ -> ()
  | Literal _ -> ()

  | ClassLiteral t -> typ env t
  | NewClass (t, args, decls_opt) ->
      typ env t;
      exprs env args;
      (match decls_opt with
      | None -> ()
      | Some xs ->
        (* less: quite similar to class_decl, factorize code? *)
          let classname, info  = classname_and_info_of_typ t in
          let charpos = PI.pos_of_info info in
          let anon_class = spf "__anon__%s__%d" classname charpos in
          let cdecl = {
            cl_name = (anon_class, info);
            cl_extends = Some t;
            cl_impls = [];
            cl_kind = ClassRegular;
            cl_body = xs;
            (* ?? *)
            cl_tparams = [];
            cl_mods = [];
          }
          in
          class_decl env cdecl
      )
  | NewQualifiedClass (_e, id, args, decls_opt) ->
      (*
      pr2 "NewQualifiedClass";
      pr2_gen (NewQualifiedClass (e, id, args, decls_opt));
      *)
      (* todo: need to resolve the type of 'e' *)
      expr env (NewClass (TClass ([id, []]), args, decls_opt))

  | NewArray (t, args, _i, ini_opt) ->
      typ env t;
      exprs env args;
      init_opt env ini_opt

  | Call (e, es) ->
                  (if env.phase = MethodToMethod then
                    resolve_call env (e,es));
      expr env e;
      exprs env es;
(* TODO: resolve call    *)
  | Dot (e, _idTODO) ->
      (* todo: match e, and try lookup method/field
       * if e is a Name, lookup it, and if a class then
       * lookup children. If local ... then need get its type
       * lookup its node, and then lookup children.
       *)
      expr env e;

  | ArrayAccess (e1, e2) -> exprs env [e1;e2]
  | Postfix (e, _op) | Prefix (_op, e) -> expr env e
  | Infix (e1, _op, e2) -> exprs env [e1;e2]
  | Conditional (e1, e2, e3) -> exprs env [e1;e2;e3]
  | Assignment (e1, _op, e2) -> exprs env [e1;e2]

  | Cast (t, e) ->
      typ env t;
      expr env e
  | InstanceOf (e, tref) ->
      expr env e;
      typ env (tref);

(* TODO: Remove ast, if it is not used *)
and resolve_call env (expr_,_) =
       let rec aux expr_ =
        (match expr_ with 
        | Name name_ -> resolve_name name_
        | NameOrClassType _ 
        | Literal _ 
        | ClassLiteral _  
        | NewClass _ 
        | NewArray _ 
        | NewQualifiedClass _ -> ["Not supported"]
        | Call(expr_,_) ->     aux expr_
        | Dot (expr_, ident) -> 
            let qualifier = (aux expr_) in
            (match qualifier with 
            | [] -> [Ast.unwrap ident]
            | q -> q @ [Ast.unwrap ident]
            )
        | ArrayAccess _
        | Postfix _
        | Prefix _
        | Infix _
        | Cast _
        | InstanceOf _
        | Conditional _
        | Assignment _ -> ["Not supported"]
        )
        in
        let node_str_array =  aux expr_ in
        let node_str =  (last node_str_array) in
        let f a = join_list ~sep:"." a
               in
        let dfs_f = ref (dfs ~verbose:true ~env  ~node:env.current  ~get_edges:(fun
                ~n -> G.succ n G.Use env.g) ~f:(fun node -> G.has_node
                (node,E.Method) env.g) )
        in
        (*
         *if node_str = "addAccessibilityInteractionConnection" then
         *  dfs_f.contents <- (dfs ~verbose:true ~env  ~node:env.current  ~get_edges:(fun
         *        ~n -> G.succ n G.Use env.g) ~f:(fun node -> G.has_node
         *        (node,E.Method) env.g) );
         *)
        let node_str_o =
          (match node_str_array with
          | [] -> None
          | _::node_str_array_tl -> 
              (match dfs_f.contents ~node_str:(f node_str_array_tl) with
              | (None, printer) -> 
                  printer.contents <- printer.contents @ 
                  ["MTM: lookup without first qualifier failed"];
                  (match dfs_f.contents ~node_str:(f node_str_array) with
                  | (None, printer2) -> 
                      let string_ = join_list ~sep:"\n"  (printer.contents @
                      printer2.contents) in
                      pr "Print dfs";
                      pr string_;
                      None
                  | (Some a,_) -> Some a
                  )
              | (Some a, _) -> Some a 
            )
          )
        (*
         *let node_str_o =
         *  (match dfs_f.contents ~node_str:(node_str) with
         *  | None -> 
         *       (match node_str_array with
         *       | [] -> None
         *       | _::node_str_array_tl -> 
         *           pr "MTM: lookup with first qualifier failed";
         *           pr (f node_str_array_tl);
         *           pr (f node_str_array);
         *           dfs_f.contents ~node_str:(f node_str_array_tl)
         *       )
         *  | str_o -> str_o
         *  )
         *)
        in      
        (match node_str_o with
        | None -> 
            pr (spf "MTM: (resolve_call) lookup fail on %s" node_str)
        | Some str -> 
            add_use_edge env (str, E.Method);
            pr (spf "MTM: (resolve_call) Edge drawn to method %s, %s" str
           node_str)
        );
        ()

(* Removes the object's name in method calls, NOTE: this is a heuristic which
 * relies on the fact that most method calls are of the form A.x(), where A is
 * an object. Also, it assumes that in type name, first element of the list can
 * be removed*)
and resolve_name name_ =
  List.map (fun (_tyarg_todo, ident) -> Ast.unwrap ident) name_
  
        (*
         * and resolve_namr _ast name_ =
         *match name_ with
         *| hd::[] -> str_of_name [hd] 
         *| _::tl -> str_of_name tl
         *| [] -> "" 
         *)

(* Finds the type of object, and prepends it to qualifying path
and resolve_name ast name_ =
        let flag = ref true in
        let field_name_ = match name_ with
        | hd::_ -> Ast.unwrap (snd hd)
        | [] -> flag.contents <- false;
                pr " field_name_ []";
                        "None"
        in 
        let str = ref (str_of_name_this_super name_) in
        let traverse_ast = V.mk_visitor { V.default_visitor with 
        (* Only handling fields, as visitor functions are written only for it*)
        V.kfield= ( fun(k,_) d->
                (match d with
                | field  -> 
                                let field_name = fst (field.f_var.v_name) in
                                if field_name = field_name_ then
                                        begin
                                        pr "field_name equal \n";
                                        pr "field_name_ ";
                                        pr field_name_;
(*                                         flag.contents <- true; *)
                                        let object_name = resolve_typ field.f_var.v_type in
                                        let str_name_td = match name_ with
                                        | [] -> flag.contents <- false;
 (*                                                 pr "str_name_td []";  *)
                                                        "None"
                                        | _::name_td -> str_of_name_this_super name_td
                                        in
                                        (*
                                         *pr "str_name_td";
                                         *pr str_name_td;
                                         *pr "--------";
                                         *)
                                        str.contents <- 
                                        (match str_name_td with
                                        | "" -> object_name 
                                        | q -> object_name ^ "."^q
                                        )
                                        end;

        );
        k d
        )
        
        }
        in traverse_ast(Ast.AProgram ast);
        if flag.contents = true then
        str.contents
        else
                str_of_name_this_super name_

and resolve_typ = function 
        | TBasic ident_ -> Ast.unwrap ident_
        | TClass class_type_ -> str_of_class_type class_type_
        | TArray typ_ -> resolve_typ typ_ 

*)
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
        (match str, reft with
        (* TODO: look at the type and continue lookup *)
        | _, (((s,_),_))::_rest when List.mem s env.type_parameters -> ()
        | _ ->
            (match lookup env xs with
            (* TODO: look in type_params_local ! *)
            | Some n2 ->
                (* pr2 ("FOUND: " ^ Common.dump n); *)
                add_use_edge env n2
            | None ->
                (match xs with
                | [] -> raise Impossible
                | ((s,_))::_ when List.mem_assoc s env.imported_qualified ->
                    let (_is_static, full_ident) =
                      List.assoc s env.imported_qualified in
                    let str = str_of_qualified_ident full_ident in
                    add_use_edge env (str, E.Package)

                | [_x] ->
                    if looks_like_class_name str
                    then add_use_edge env (str, E.Package)
                    else
                      pr2 ("PB: " ^ Common.dump reft);
                | _x::_y::_xs ->
                    (* unknown package probably *)
                    add_use_edge env (str, E.Package)
                )
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

let build ?(verbose=true) ?(only_defs=false) ?(method_to_method=false) root files =
  let g = G.create () in
  G.create_initial_hierarchy g;

  let lookup_fails = Common2.hash_with_default (fun () -> 0) in

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
(*    pr "Step 1\n";  *)
  files +> Console.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let readable = Common.readable ~root file in
      let ast = parse ~show_parse_error:true file in
     extract_defs_uses ~phase:Defs ~g ~ast ~readable ~lookup_fails;
    ));
  if not only_defs then begin

  (* step2: creating the 'Use' edges just for inheritance *)
  if verbose then pr2 "\nstep2: extract inheritance information";
(*   pr "Step 2 \n"; *)
  files +> Console.progress ~show:verbose (fun k ->
   List.iter (fun file ->
     k();
     let readable = Common.readable ~root file in
     let ast = parse ~show_parse_error:false file in
     extract_defs_uses ~phase:Inheritance ~g ~ast ~readable ~lookup_fails;
   ));

  (* step3: creating the 'Use' edges that can rely on recursive inheritance *)
  if verbose then pr2 "\nstep3: extract uses";
  files +> Console.progress ~show:verbose (fun k ->
   List.iter (fun file ->
     k();
     let readable = Common.readable ~root file in
     let ast = parse ~show_parse_error:false file in
     extract_defs_uses ~phase:Uses ~g ~ast ~readable ~lookup_fails;
   ));
  end;
  if method_to_method then 
    begin
  if verbose then pr2 "\nstep4: methodtomethod";
(*    pr "Step 4\n";  *)
  files +> Console.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let readable = Common.readable ~root file in
      let ast = parse ~show_parse_error:true file in
     extract_defs_uses ~phase:MethodToMethod ~g ~ast ~readable ~lookup_fails;
  ));
    end;
  g
