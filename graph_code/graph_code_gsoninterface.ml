open Parse_info
open Common
open Graph_code

module GS = Graph_to_graphson
module G = Graph_code
module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * An interface from Graph_code, which can be accessed to crete graphson files.
 * This cannot be added to graph_code.ml because of circular dependency between
 * Graph_code and Graph_to_graphson.
 *
 * Below graph construction functions can be used instead of from Graph_code.
 * These automatically create a GraphSON file. Nodes or edges in GraphSON can 
 * be added by either add_nodeinfo/add_edgeinfo or add_node/add_edge depending
 * on flag set (see Global flags).
 *
 * NOTE: Please remember to call function merge_graphson once all graph
 * operations are completed. 
 *)

(*****************************************************************************)
(* Global flags *)
(*****************************************************************************)
let add_nodeinfo_flag = ref true
let add_node_flag = ref false
let add_edgeinfo_flag = ref false
let add_edge_flag = ref true 

(*****************************************************************************)
(* Graph construction*)
(*****************************************************************************)
let empty_edgeinfo =
  { G.write = true; G.read = false }

let empty_ident =
  let a =
  {
    str = "Empty";
    charpos = 0;
    line = 0;
    column = 0;
    file = "Empty";
  }
  in
  {
    token = OriginTok a;
    transfo = NoTransfo;
  }
let empty_nodeinfo = { G.pos = Parse_info.token_location_of_info empty_ident;
    G.props = [];
    G.typ = None;
  }
let add_node n g =
  if !add_node_flag then
    GS.add_graphson_vertex n empty_nodeinfo;
  G.add_node n g

let add_nodeinfo n ninfo g =
  if !add_nodeinfo_flag then
    GS.add_graphson_vertex n ninfo;
  G.add_nodeinfo n ninfo g

let add_edge (n1, n2) e g =
  if !add_edge_flag then
    GS.add_graphson_edge (n1, n2, e) empty_edgeinfo;
  G.add_edge (n1, n2) e g

let add_edgeinfo (n1, n2) e einfo g =
  if !add_edgeinfo_flag then
  GS.add_graphson_edge (n1, n2, e) einfo;
  G.add_edgeinfo (n1, n2) e einfo g

let create_initial_hierarchy g =
  g +> add_node root;
  g +> add_node pb;
  g +> add_node not_found;
  g +> add_node dupe;

  g +> add_nodeinfo root empty_nodeinfo;
  g +> add_nodeinfo pb empty_nodeinfo;
  g +> add_nodeinfo not_found empty_nodeinfo;
  g +> add_nodeinfo dupe empty_nodeinfo;

(*  g +> add_node stdlib;*)
  g +> add_edge (root, pb) Has;
  g +> add_edge (pb, dupe) Has;
  g +> add_edge (pb, not_found) Has;
(*  g +> add_edge (root, stdlib) Has;*)
  ()


let graph_of_dotfile dotfile =
  let xs = Common.cat dotfile in
  let deps =
    xs +> Common.map_filter (fun s ->
      if s =~ "^\"\\(.*\\)\" -> \"\\(.*\\)\"$"
      then
        let (src, dst) = Common.matched2 s in
        Some (src, dst)
      else begin
        pr2 (spf "ignoring line: %s" s);
        None
      end
    )
  in
  let g = G.create () in
  create_initial_hierarchy g;
  (* step1: defs *)
  deps +> List.iter (fun (src, dst) ->
    let srcdir = Filename.dirname src in
    let dstdir = Filename.dirname dst in
    try 
      create_intermediate_directories_if_not_present g srcdir;
      create_intermediate_directories_if_not_present g dstdir;
      if not (has_node (src, E.File) g) then begin
        g +> add_node (src, E.File);
        g +> add_edge ((srcdir, E.Dir), (src, E.File)) Has;
      end;
      if not (has_node (dst, E.File) g) then begin
        g +> add_node (dst, E.File);
        g +> add_edge ((dstdir, E.Dir), (dst, E.File)) Has;
      end;

    with Assert_failure _ ->
      pr2_gen (src, dst);
  );
  (* step2: use *)
  deps +> List.iter (fun (src, dst) ->
    let src_node = (src, E.File) in
    let dst_node = (dst, E.File) in
    
    g +> add_edge (src_node, dst_node) Use;
  );
  g

let create_intermediate_directories_if_not_present g dir =
  let dirs = Common2.inits_of_relative_dir dir in

  let rec aux current xs =
    match xs with
    | [] -> ()
    | x::xs ->
        let entity = x, E.Dir in
        if has_node entity g
        then aux entity xs
        else begin
          g +> add_node entity;
          g +> add_edge (current, entity) Has;
          aux entity xs
        end
  in
  aux root dirs

(* NOTE: Please remember to call this function once all graph operations are
 * done *)
let merge_graphson () =
  Graph_print_options.merge_graphson ()
