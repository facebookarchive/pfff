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

module StringMap : Map.S with type key = string
module IntMap : Map.S with type key = int

type id

type 'a sequence =
  { mutable count : int;
    mutable seq : 'a IntMap.t;
    id : (id, 'a) Hashtbl.t }

type node =
  { name : string;
    id : id;
    mutable node_attr : string StringMap.t }

type edge =
  { head : node;
    tail : node;
    edge_id : id;
    mutable edge_attr : string StringMap.t }

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

val of_file_spec : Dot_file.t -> info * graph

val of_channel : in_channel -> info * graph
