(* Graph viewer
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

module IntSet =
  Set.Make (struct type t = int let compare (x : int) y = compare x y end)

module IntMap =
  Map.Make (struct type t = int let compare (x : int) y = compare x y end)

type id = int

module IdMap = IntMap

let last_id = ref (-1)

let fresh_id () = incr last_id; !last_id

type 'a sequence =
  { mutable count : int;
    mutable seq : 'a IntMap.t;
    id : (id, 'a) Hashtbl.t }

let make_sequence () =
  { count = 0;
    seq = IntMap.empty;
    id = Hashtbl.create 17 }

let sequence_add s id v =
  if not (Hashtbl.mem s.id id) then begin
    let n = s.count in
    s.count <- n + 1;
    s.seq <- IntMap.add n v s.seq;
    Hashtbl.add s.id id v
  end

module StringMap = Map.Make (String)

type node =
  { name : string;
    id : id;
    mutable node_attr : string StringMap.t }

type edge =
  { head : node;
    tail : node;
    edge_id : id;
    mutable edge_attr : string StringMap.t }

type def_attr =
  { mutable g_attr : string StringMap.t;
    mutable n_attr : string StringMap.t;
    mutable e_attr : string StringMap.t }

type graph =
  { graph_id : id;
    graph_name : string option;
    mutable graph_attr : string StringMap.t;
    subgraphs : graph sequence;
    nodes : node sequence;
    edges : edge sequence;
    parents : (id, graph) Hashtbl.t }

type info =
  { kind : [`Graph | `Digraph];
    strict : bool }

type st =
  { st_info : info;
    st_graphs : (string, graph) Hashtbl.t;
    st_nodes : (string, node) Hashtbl.t;
    st_edges : (string * string * string, edge) Hashtbl.t }

let make_def_attr () =
  { g_attr = StringMap.empty;
    n_attr = StringMap.empty;
    e_attr = StringMap.empty }

let clone_def_attr a =
  { g_attr = a.g_attr; n_attr = a.n_attr; e_attr = a.e_attr }

let rec all_parents s g =
  if IntMap.mem g.graph_id s then s else
  Hashtbl.fold (fun _ g s -> all_parents s g)
    g.parents (IntMap.add g.graph_id g s)

let insert_graph parent g =
  if not (IntMap.mem g.graph_id (all_parents IntMap.empty parent)) then begin
    Hashtbl.add g.parents parent.graph_id parent;
    sequence_add parent.subgraphs g.graph_id g
  end

let make_graph parent name def_attrs =
  let g =
    { graph_id = fresh_id ();
      graph_name = name;
      graph_attr = def_attrs.g_attr;
      subgraphs = make_sequence ();
      nodes = make_sequence ();
      edges = make_sequence ();
      parents = Hashtbl.create 17 }
  in
  begin match parent with
    Some parent -> insert_graph parent g
  | None        -> ()
  end;
  g

let insert_node g n =
  let p = all_parents IntMap.empty g in
  IntMap.iter (fun _ g -> sequence_add g.nodes n.id n) p

let make_node g name def_attrs =
  let node =
    { name = name;
      id = fresh_id ();
      node_attr = def_attrs.n_attr }
  in
  insert_node g node;
  node

let insert_edge g e =
  let p = all_parents IntMap.empty g in
  IntMap.iter (fun _ g -> sequence_add g.edges e.edge_id e) p

let make_edge g n1 n2 attrs =
  let edge =
    { tail = n1; head = n2;
      edge_id = fresh_id ();
      edge_attr = attrs }
  in
  insert_edge g edge;
  edge

(****)

let find_graph st parent name def_attrs =
  match name with
    Some nm when Hashtbl.mem st.st_graphs nm ->
      let g = Hashtbl.find st.st_graphs nm in
      begin match parent with
        Some parent -> insert_graph parent g
      | None        -> ()
      end;
      g
  | _ ->
      let g = make_graph parent name def_attrs in
      begin match name with
        Some nm -> Hashtbl.add st.st_graphs nm g
      | None    -> ()
      end;
      g

let find_node st g name def_attrs =
  try
    let n = Hashtbl.find st.st_nodes name in
    insert_node g n;
    n
  with Not_found ->
    let n = make_node g name def_attrs in
    Hashtbl.add st.st_nodes name n;
    n

let lookup_edge st n1 n2 key =
  try
    Hashtbl.find st.st_edges (n1.name, n2.name, key)
  with Not_found when st.st_info.kind = `Graph ->
    Hashtbl.find st.st_edges (n2.name, n1.name, key)

let find_edge st g n1 n2 key attrs =
  let key = if st.st_info.strict then Some "" else key in
  try
    let key =
      match key with
        Some k -> k
      | None   -> raise Not_found
    in
    let e = lookup_edge st n1 n2 key in
    insert_edge g e;
    e
  with Not_found ->
    let e = make_edge g n1 n2 attrs in
    begin match key with
      Some key -> Hashtbl.add st.st_edges (n1.name, n2.name, key) e
    | None     -> ()
    end;
    e

(****)

let add_attributes def l =
  List.fold_left (fun s (nm, v) -> StringMap.add nm v s) def l

let get_edges x =
  match x with
    `Node (n, p) ->
      (IntMap.add 0 n IntMap.empty, p)
  | `Graph gr ->
      (gr.nodes.seq, None)


let opt_add nm v m =
  match v with
    Some v -> StringMap.add nm v m
  | None   -> m

let add_edge st g n1 p1 n2 p2 key attrs =
  let attrs = opt_add "tailport" p1 (opt_add "headport" p2 attrs) in
  ignore (find_edge st g n1 n2 key attrs)

let rec add_edges st g x r key attrs =
  match r with
    [] ->
      ()
  | y :: r ->
      let (s1, p1) = get_edges x in
      let (s2, p2) = get_edges y in
      IntMap.iter
        (fun _ n1 ->
           IntMap.iter
             (fun _ n2 -> add_edge st g n1 p1 n2 p2 key attrs) s2)
        s1;
      add_edges st g y r key attrs

let rec compound_to_graph st g def_attr (c, attr) =
  let c =
    List.map
      (fun s ->
         match s with
         `Node node ->
           `Node (find_node st g node.Dot_file.name def_attr,
                  node.Dot_file.port)
       | `Graph gr ->
           `Graph (graph_def_to_graph st (Some g) def_attr gr))
    c
  in
  match c with
    [] ->
      assert false
  | [`Node (n, _)] ->
      n.node_attr <- add_attributes n.node_attr attr
  | [`Graph _] ->
      ()
  | x :: r ->
      let attrs = add_attributes def_attr.e_attr attr in
      let key =
        try Some (StringMap.find "key" attrs) with Not_found -> None
      in
      add_edges st g x r key attrs

and body_to_graph st g def_attr body =
  List.iter
    (fun stmt ->
       match stmt with
         `Compound c ->
           compound_to_graph st g def_attr c
       | `Attributes (typ, l) ->
           match typ with
             `Graph -> def_attr.g_attr <-
                         add_attributes def_attr.g_attr l
           | `Node  -> def_attr.n_attr <-
                         add_attributes def_attr.n_attr l
           | `Edge  -> def_attr.e_attr <-
                         add_attributes def_attr.e_attr l)
    body

and graph_def_to_graph st g def_attr gr =
  let g = find_graph st g gr.Dot_file.graph_name def_attr in
  let def_attr = clone_def_attr def_attr in
  body_to_graph st g def_attr gr.Dot_file.body;
  g.graph_attr <- def_attr.g_attr;
  g

let of_file_spec f =
  let st =
    { st_info = { kind = f.Dot_file.kind; strict = f.Dot_file.strict };
      st_graphs = Hashtbl.create 101;
      st_nodes = Hashtbl.create 101;
      st_edges = Hashtbl.create 101 }
  in
  (st.st_info, graph_def_to_graph st None (make_def_attr ()) f.Dot_file.graph)

let of_channel c =
  Dot_lexer.reset ();
  let g = Dot_parser.graph Dot_lexer.token (Lexing.from_channel c) in
  of_file_spec g
