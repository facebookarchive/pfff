open Eliom_pervasives

open Common

module E = Database_code
module G = Graph_code
module J = Json_type

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type layer =
  | Init
  | Interface
  | Implementation
  | Accessor
  | Property

let int_of_layer = function
  | Init -> 0
  | Interface -> 1
  | Implementation -> 2
  | Accessor -> 3
  | Property -> 4

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let _hmemo = Hashtbl.create 101
let g_and_pred () =
  Common.memoized _hmemo "www" (fun () ->
    let g = G.load "/home/pad/www/graph_code.marshall" in
    let pred = G.mk_eff_use_pred g in
    g, pred
 )

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let layer_of_node shortname node g =
  match shortname, snd node with
  | "__construct", _ -> Init
  | s, E.Method _ when s =~ "set.*" || s =~ "get.*" -> Accessor
  | _, E.Field -> Property
  | _, E.ClassConstant -> Property (* todo: skip them *)
  | _, E.Method _ ->
    let privacy = G.privacy_of_node node g in
    (match privacy with
    | E.Public -> Interface
    | E.Private | E.Protected -> Implementation
    )
  | _ -> failwith (spf "unexpected member: %s" 
                     (G.string_of_node node))

(* very crude way to count the LOC, but should be good enough *)
let mk_loc_members classnode members g =
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
      if (line >= line2)
      then failwith (spf "line not sorted, %s:%d and %d"
                       (G.string_of_node node) line
                       line2);
      Hashtbl.add hloc node (line2 - line);
      aux ((line2, node2)::xs)
  in
  aux sorted;
  hloc

(*****************************************************************************)
(* main entry point *)
(*****************************************************************************)

let main_service = Eliom_registration.String.register_service 
  ~path:["blueprint"]
  ~get_params:(Eliom_parameter.string "class")
  (fun (classname) () ->

    let g, pred = g_and_pred () in
    
    let node = classname, E.Class E.RegularClass in
    if not (g +> G.has_node node)
    then failwith (spf "classname %s not found in code database" classname);

    let members = g +> G.children node in
    let hmembers = Common.hashset_of_list members in
    let hloc = mk_loc_members node members g in
    
    let json = 
      J.Object [
        "nodes", J.Array (
          members +> List.map (fun node ->
            let shortname = G.shortname_of_node node in
            let layer = layer_of_node shortname node g in
            (* #callers *)
            let nb_callers = List.length (pred node) in
            (* #loc *)
            let loc = Hashtbl.find hloc node in
            let code_graph_rank = 42 in
            
            J.Object [
              "name", J.String shortname;
              "nb_callers", J.Int nb_callers;
              "loc", J.Int loc;
              "layer", J.Int (int_of_layer layer);
              "tooltip", J.String "TODO";
              "rank", J.Int code_graph_rank;
          ];
          ));
        "edges", J.Array (
          members +> List.map (fun node ->
            (g +> G.succ node G.Use)
              +> List.filter (fun node -> Hashtbl.mem hmembers node)
              +> List.map (fun node2 ->
                let src = G.shortname_of_node node in
                let dst = G.shortname_of_node node2 in
                J.Array [ J.String src; J.String dst]
              )
        ) +> List.flatten
        )
      ]
    in
    let str = Json_out.string_of_json json in
    Lwt.return (str, "json")
  )
