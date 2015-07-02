open Parse_info
open Graph_code
open Graph_code_piqi
open Common

module E = Entity_code
module GC = Graph_code
module GP = Graph_code_piqi_ext
module G = Graph

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parses graph_code/graph and writes to file in xml/json/pb/piqi format. Cons
 * is, stack overflows for large graphs. Pls use graph_to_graphson for larger
 * graphs, and when time is a constraint. Writes only to json, but can be
 * changed to accommodate xml/pb/piqi also.
 *)

(* ---------------------------------------------------------------------- *)
(* Entity_code *)
(* ---------------------------------------------------------------------- *)
let entity_kind_piqi = function 
  | E.Package -> `Package
  (* when we use the database for completion purpose, then files/dirs
   * are also useful "entities" to get completion for.
   *)
  | E.Dir -> `Dir 

  | E.Module -> `Module 
  | E.File -> `File 

  | E.Function-> `Function
  | E.Class -> `Class
  | E.Type -> `Type
  | E.Constant -> `Constant 
  | E.Global -> `Global
  | E.Macro -> `Macro
  | E.Exception -> `Exception
  | E.TopStmts -> `TopStmts

  (* nested entities *)
  | E.Field -> `Field
  | E.Method -> `Method
  | E.ClassConstant -> `ClassConstant
  | E.Constructor -> `Constructor 

  (* forward decl *)
  | E.Prototype -> `Prototype 
  | E.GlobalExtern -> `GlobalExtern

  (* people often spread the same component in multiple dirs with the same
   * name (hmm could be merged now with Package)
   *)
  | E.MultiDirs -> `MultiDirs

  | E.Other x -> `Other x

let privacy_piqi = function
  | E.Public -> `Public 
  | E.Protected -> `Protected 
  | E.Private-> `Private

  and class_kind_piqi = function
  | E.Struct -> `Struct 
  | E.Class_ -> `Class
  | E.Interface -> `Interface 
  | E.Trait -> `Trait 
  | E.Enum -> `Enum

let property_piqi = function 
   | E.ContainDynamicCall -> `ContainDynamicCall
   | E.ContainReflectionCall -> `ContainReflectionCall

    (* the argument position taken by ref; 0-index based *)
   | E.TakeArgNByRef x -> `TakeArgNByRef x

   | E.UseGlobal x -> `UseGlobal x
   | E.ContainDeadStatements-> `ContainDeadStatements

   | E.DeadCode -> `DeadCode 
   | E.CodeCoverage x ->`CodeCoverage x

   (* for class *)
   | E.ClassKind class_kind_ -> `ClassKind (class_kind_piqi class_kind_)

   | E.Privacy privacy_ -> `Privacy (privacy_piqi privacy_)
   | E.Abstract -> `Abstract
   | E.Final -> `Final
   | E.Static -> `Static

   (* used for the xhp @required fields for now *)
   | E.Required -> `Required
   | E.Async -> `Async

let property_list_piqi x =
  List.map property_piqi x
(* ---------------------------------------------------------------------- *)
(* Parse_info *)
(* ---------------------------------------------------------------------- *)

let token_location_piqi x = 
  { 
    Token_location.str = x.str;
    Token_location.charpos = x.charpos;
    Token_location.line = x.line;
    Token_location.column = x.column;
    Token_location.file = x.file
  }

(* ---------------------------------------------------------------------- *)
(* Graph_code *)
(* ---------------------------------------------------------------------- *)

let edge_string = function
  | GC.Has -> "Has"
  | GC.Use -> "Use"

let edge_piqi = function
  | GC.Has -> `Has 
  | GC.Use -> `Use

let node_piqi (str, entity_kind_) =
  {
    Node.name = str;
    Node.entity_kind = entity_kind_piqi entity_kind_;
  }

let nodeinfo_piqi x = 
  {
    Nodeinfo.pos = token_location_piqi x.pos;
    Nodeinfo.props = property_list_piqi x.props;
    Nodeinfo.typ = x.typ;
  }

let edgeinfo_piqi x =
  {
    Edgeinfo.write = x.write;
    Edgeinfo.read = x.read;
  }
let nodeinfo_hash_piqi x = Hashtbl.fold ( fun key value accum -> accum @
    [
      {
        Nodeinfo_hash_element.key = node_piqi key;
        Nodeinfo_hash_element.value = nodeinfo_piqi value;
      }
    ]
  )
  x []

let edgeinfo_hash_helper_piqi (tuple_1,tuple_2,tuple_3) =
  {
    Edgeinfo_hash_helper.tuple_1 = node_piqi tuple_1;
    Edgeinfo_hash_helper.tuple_2 = node_piqi tuple_2;
    Edgeinfo_hash_helper.tuple_3 = edge_piqi tuple_3;
  }
  
let edgeinfo_hash_piqi x =
  Hashtbl.fold ( fun key value accum -> 
    accum @ 
    [
      {
        Edgeinfo_hash_element.key = edgeinfo_hash_helper_piqi key;
        Edgeinfo_hash_element.value = edgeinfo_piqi value;
      }
    ]
  )
  x []

let key_of_vertex_hash_piqi x =
  Hashtbl.fold ( fun _key value accum -> 
    accum @
    [
      {
        Key_of_vertex_hash_element.key = Hashtbl.hash value;
        Key_of_vertex_hash_element.value = node_piqi value;
      }
    ]
  )
  x []

let vertex_of_key_hash_piqi x =
  Hashtbl.fold ( fun key _value accum ->
    accum @
    [
      {
        Vertex_of_key_hash_element.key = node_piqi key;
        Vertex_of_key_hash_element.value = Hashtbl.hash key;
      }
    ]
  )
  x []

let commons_graph_piqi x =
  {
    Commons_graph.og = 1;
    Commons_graph.key_of_vertex = key_of_vertex_hash_piqi (G.graph_key_of_vertex
        x);
    Commons_graph.vertex_of_key = vertex_of_key_hash_piqi (G.graph_vertex_of_key
        x);
    Commons_graph.cnt = !(G.graph_cnt x);
  }

let graph_code_graph_piqi x = 
  {
    Graph_code_graph.has = commons_graph_piqi (GC.graph_code_graph_has x);
    Graph_code_graph.use = commons_graph_piqi (GC.graph_code_graph_use x);
    Graph_code_graph.nodeinfo_hash_instance = nodeinfo_hash_piqi
        (GC.graph_code_graph_nodeinfo x);
    Graph_code_graph.edgeinfo_hash_instance = edgeinfo_hash_piqi
    (GC.graph_code_graph_edgeinfo x);
  }

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let graph_print g output_format_string =
  let g = graph_code_graph_piqi g in
  let g2 = Graph_print_options.graph_options g output_format_string in
  (*Graph_code_piqi_ext.print_graph_code_graph g*)
  write_file ("graph_code."^output_format_string) g2

