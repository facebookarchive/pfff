open Eliom_pervasives

open Common

module E = Database_code
module G = Graph_code
module J = Json_type

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Prototype blueprint backend.
 * 
 * todo:
 *  - store range of methods in graph_code
 *  - the comment associated with it?
 *    (hmmm the graph will be significantly bigger on disk then no?)
 *  - inline mode where inline parent methods?
 *  - less: improve layer, detect constructor calling initialisation function?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type layer =
  | Init
  | Interface
  | Implementation
  | Accessor
  | Property

type kind =
 | Attribute (* <=> property layer *)
 | Setter | Getter (* only for Accessor layer *)

 | Async
 | Abstract

 | CallSuper
 | Delegate
 | ReturnConstant

 | Nothing

type db = {
  g: Graph_code.graph;
  pred: Graph_code.node -> Graph_code.node list;
  hrank: (Graph_code.node, int) Hashtbl.t;
  (* from children to parents *)
  parents: Graph_code_class_analysis.class_hierarchy;
  (* from parent to children *)
  children: Graph_code_class_analysis.class_hierarchy;
}

(*****************************************************************************)
(* Boilerplate *)
(*****************************************************************************)

let int_of_layer = function
  | Init -> 0
  | Interface -> 1
  | Implementation -> 2
  | Accessor -> 3
  | Property -> 4

let string_of_kind = function
 | Attribute -> "Attribute"
 | Setter  -> "Setter "
 | Getter -> "Getter"
 | Async -> "Async"
 | Abstract -> "Abstract"
 | CallSuper -> "CallSuper"
 | Delegate -> "Delegate"
 | ReturnConstant -> "ReturnConstant"
 | Nothing -> ""

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let _hmemo = Hashtbl.create 101
let db () =
  Common.memoized _hmemo "www" (fun () ->
    let graph_file = "/home/pad/www/graph_code.marshall" in
    let g = G.load graph_file in
    let pred = G.mk_eff_use_pred g in
    let hrank =
      Common.cache_computation graph_file ".rank.marshall" (fun () ->
        G.bottom_up_numbering g
      )
    in
    let hierarchy = Graph_code_class_analysis.class_hierarchy g in
    let children = hierarchy in
    let parents = Graph.mirror children in
    {g; pred; hrank; children; parents }
 )

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let look_like_accessor node g =
  let uses = G.succ node G.Use g in
  uses +> List.for_all (fun (_str, kind) ->
    match kind with
    | E.ClassConstant | E.Field -> true
    (* todo: or just call functions like invariant_violation *)
    | _ -> false
  )

let layer_of_node shortname node g =
  match shortname, snd node with
  | "__construct", _   -> Init
  | s, E.Method when (s =~ "set.*" || s =~ "get.*") && 
                       look_like_accessor node g -> Accessor
  | _, E.Field         -> Property
  | _, E.ClassConstant -> Property (* todo: skip them *)
  | _, E.Method      ->
      let privacy = G.privacy_of_node node g in
      (match privacy with
      | E.Public                -> Interface
      | E.Private | E.Protected -> Implementation
      )
  | _ -> failwith (spf "unexpected member: %s" 
                     (G.string_of_node node))

let kind_of_node layer shorname node g =
  match () with
  | _ when layer =*= Property -> Attribute
  | _ when layer =*= Accessor ->
      if shorname =~ "set.*"
      then Setter
      else Getter
  (* todo: should have such a property in the nodeinfo *)
  | _ when shorname =~ "gen.*" -> Async
  | _ ->
    let info = G.nodeinfo node g in
    let props = info.G.props in
    let is_async = ref false in
    let is_abstract = ref false in
    props +> List.iter (function
    | E.Async -> is_async := true
    | E.Abstract -> is_abstract := true
    | _ -> ()
    );
    if !is_async then Async
    else 
      if !is_abstract then Abstract
      else Nothing

(* very crude way to count the LOC, but should be good enough *)
let mk_loc_members members g =
  let positions = members +> List.map (fun node ->
    let info = G.nodeinfo node g in
    let line = info.G.pos.Parse_info.line in
    line, node
  )
  in
  let sorted = Common.sort_by_key_lowfirst positions in
  
  let hloc = Hashtbl.create 101 in
  let rec aux = function
    | [] -> ()
    (* TODO: need get size of file, or store range in graph_code *)
    | [(line, node)] -> Hashtbl.add hloc node 5 (* hmmm *)
    | (line, node)::(line2, node2)::xs ->
      if (line > line2)
      then failwith (spf "line not sorted, %s:%d and %d"
                       (G.string_of_node node) line
                       line2);
      Hashtbl.add hloc node (line2 - line);
      aux ((line2, node2)::xs)
  in
  aux sorted;
  hloc

let all_parents_of_class ?(depth_limit=2) node dag =
  let inheritance = ref [] in
  let rec aux depth node =
    (* todo: could go deep enough to reach relevant method, that
     * is when class have methods that calls you
     *)
    if depth = depth_limit
    then []
    else begin
      (* can have multiple parents with interfaces and traits *)
      let parents = Graph.succ node dag in
      parents +> List.iter (fun parent ->
        Common.push (parent, node) inheritance;
      );
      parents @ (parents +> List.map (aux (depth + 1)) +> List.flatten)
    end
  in
  let res = aux 0 node in
  node::res, !inheritance

(* could factorize with all_parents_of_class but inheritance is tricky *)
let all_children_of_class ?(depth_limit=2) node dag =
  let inheritance = ref [] in
  let rec aux depth node =
    if depth = depth_limit
    then []
    else begin
      let children = Graph.succ node dag in
      children +> List.iter (fun child ->
        Common.push (node, child) inheritance;
      );
      children @ (children +> List.map (aux (depth + 1)) +> List.flatten)
    end
  in
  let res = aux 0 node in
  node::res, !inheritance



(*****************************************************************************)
(* main entry point *)
(*****************************************************************************)

let main_service = Eliom_registration.String.register_service 
  ~path:["blueprint"]
  ~get_params:Eliom_parameter.
  (string "class" ** string "hierarchy" ** int "depth_limit")
  (fun (classname, (hierarchy_direction, depth_limit)) () ->
    let depth_limit =
      if depth_limit = 0 then 2 else depth_limit
    in

    let {g; pred; hrank; children; parents } = db () in

    let node = classname, E.Class in
    if not (g +> G.has_node node)
    then failwith (spf "classname %s not found in code database" classname);

    
    let classes, inheritance_edges = 
      match hierarchy_direction with
      | "" -> [node], []
      | "up" -> 
        let dag = parents in
        all_parents_of_class ~depth_limit node dag
      | "down" ->
        let dag = children in
        all_children_of_class ~depth_limit node dag
      | _ -> failwith "wrong hierarchy param: either up, down, or nothing"
    in

    let class_and_members = classes +> List.map (fun node -> 
      let members = g +> G.children node in
      let hloc = mk_loc_members members g in
      node, members, hloc )
    in
    let all_members = 
      class_and_members +> List.map (fun (a,b,c) -> b) +> List.flatten in
    let hmembers = Common.hashset_of_list all_members in
    
    let json = 
      J.Object [
        "classes", J.Array (
          class_and_members +> List.map (fun (node, members, hloc) ->
            J.Object [
              "name", J.String (fst node);
              "nodes", J.Array (
                members +> List.map (fun node ->
                  let name = fst node in
                  let shortname = G.shortname_of_node node in
                  let layer = layer_of_node shortname node g in
                  (* #callers *)
                  let nb_callers = List.length (pred node) in
                  (* #loc *)
                  let loc = Hashtbl.find hloc node in
                  let code_graph_rank = Hashtbl.find hrank node in
                  let kind = kind_of_node layer shortname node g in
            
                  J.Object [
                    "name", J.String name;
                    "nb_callers", J.Int nb_callers;
                    "loc", J.Int loc;
                    "layer", J.Int (int_of_layer layer);
                    "tooltip", J.String "TODO";
                    "rank", J.Int code_graph_rank;
                    "kind", J.String (string_of_kind kind);
                  ];
                ))
            ]
          ));
        "edges", J.Array (
          all_members +> List.map (fun node ->
            (g +> G.succ node G.Use)
              +> List.filter (fun node -> Hashtbl.mem hmembers node)
              +> List.map (fun node2 ->
                let src = fst node in
                let dst = fst node2 in
                J.Array [ J.String src; J.String dst]
              )
          ) +> List.flatten
        );
        "hierarchy", J.Array (
          inheritance_edges +> List.map (fun (parent, child) ->
            let src = fst parent in
            let dst = fst child in
            J.Array [ J.String src; J.String dst]
          ));
      ]
    in
    let str = Json_out.string_of_json json in
    Lwt.return (str, "json")
  )
