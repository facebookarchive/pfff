open Graph_graphson_piqi
open Graph_code
open Parse_info
open Common

module E = Entity_code
module GC = Graph_code
module G = Graph

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Lots of copy paste from graph_to_piqi.ml, can not reference them here. Have 
 * two piqi types, one for graph_to_piqi and the other for converting to 
 * GraphSON. Module conflicts between them, if graph_to_piqi.ml is referenced.
 *
 * When graph_code/graph is fully loaded in memory, and operations are done on
 * it stack overflows for large graphs. To counter this, edges and vertices are
 * wrote to file as they are loaded to graph_code/graph. This is faster also.
 *
 * If for some reason, output should be written to pb/xml/piqi pls modify
 * piqi_ddl_files/graph_print_options.ml
 *)

(*****************************************************************************)
(* Global variables *)
(*****************************************************************************)

(* Flag for first vertex and edge parsed *)
let graphson_vertex = ref false
let graphson_edge = ref false

(* To keep track of vertex and edge parsed by add_graphson_edge and
 * add_graphson_vertex, to disallow duplicate entries
 *)
let edge_id = Hashtbl.create 101
let vertex_id = Hashtbl.create 101

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
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

let entity_kind_string = function 
  | E.Package -> "Package"
  (* when we use the database for completion purpose, then files/dirs
   * are also useful "entities" to get completion for.
   *)
  | E.Dir -> "Dir"

  | E.Module -> "Module"
  | E.File -> "File"

  | E.Function-> "Function"
  | E.Class -> "Class"
  | E.Type -> "Type"
  | E.Constant -> "Constant"
  | E.Global -> "Global"
  | E.Macro -> "Macro"
  | E.Exception -> "Exception"
  | E.TopStmts -> "TopStmts"
  | E.Field -> "Field"
  | E.Method -> "Method"
  | E.ClassConstant -> "ClassConstant"
  | E.Constructor -> "Constructor"
  | E.Prototype -> "Prototype"
  | E.GlobalExtern -> "GlobalExtern"
  | E.MultiDirs -> "MultiDirs"
  | E.Other _ -> "Other"


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
    Token_location.file = x.file;
  }

(* ---------------------------------------------------------------------- *)
(* Graph_code *)
(* ---------------------------------------------------------------------- *)

let edge_string = function
  | GC.Has -> "Has"
  | GC.Use -> "Use"

let edgeinfo_piqi x =
  {
    Edgeinfo.write = x.write;
    Edgeinfo.read = x.read;
  }

(* Creates unique int value, by hashing name and entity_kind of node *)
let node_hash (name, entity_kind) =
  Hashtbl.hash ( name ^ (entity_kind_string entity_kind))

let nodeinfo_hash_element_piqi key value =
  let (name,entity_kind_) = key in
  {
    Vertex.name = name;
    Vertex._id = node_hash key;
    Vertex._type = "vertex";
    Vertex.entity_kind = entity_kind_piqi entity_kind_;
    Vertex.props = property_list_piqi value.props;
    Vertex.typ = value.typ;
    Vertex.pos = token_location_piqi value.pos;
  }
  

let nodeinfo_vertex_piqi x =
  Hashtbl.fold ( fun key value accum ->
    accum @
    [
      nodeinfo_hash_element_piqi key value
    ]
 )
  x []

(* For every edge, creates a unique int vale, by hashing name and entity_kind of
 * the two nodes
 *)
let edge_hash (u,v,e) =
  let (u_name,u_entity_kind) = u in
  let (v_name,v_entity_kind) = v in
  let u_entity_kind_string = entity_kind_string u_entity_kind in
  let v_entity_kind_string = entity_kind_string v_entity_kind in
  let edge_string_ = edge_string e in
  Hashtbl.hash (u_name ^ u_entity_kind_string ^ v_name ^ v_entity_kind_string ^ edge_string_)

let edgeinfo_hash_element_piqi key value =
  let (u,v,edge) = key in
  let (u_name,u_entity_kind) = u in
  let u_entity_kind_string = entity_kind_string u_entity_kind in
  let (v_name,v_entity_kind) = v in
  let v_entity_kind_string = entity_kind_string v_entity_kind in
  if (Hashtbl.hash (u_name ^ u_entity_kind_string) = 819615078 || Hashtbl.hash
      (v_name ^ v_entity_kind_string) = 819615078) then 
    begin
      pr "Non existing vertex";
      pr u_name;
      pr (entity_kind_string u_entity_kind);
      pr v_name;
      pr (entity_kind_string v_entity_kind);
    end;
    {
      Edge_graphson._id = edge_hash key; 
      Edge_graphson._type = "edge";
      Edge_graphson._inV = node_hash v; 
      Edge_graphson._outV = node_hash u; 
      Edge_graphson._label = edge_string edge;
      Edge_graphson.edgeinfo = edgeinfo_piqi value;
    }
    
let edgeinfo_edge_piqi x =
  Hashtbl.fold ( fun key value accum ->
    accum @
    [
      edgeinfo_hash_element_piqi key value
    ]
  )
  x []

let is_valid_edge (n1, n2, _) =
  if ((Hashtbl.mem vertex_id (node_hash n1)) && (Hashtbl.mem vertex_id
      (node_hash n2)) = true) then
    true
  else
    false

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let graph_graphson_piqi g =
  {
    Graph_graphson.mode = "NORMAL";
    Graph_graphson.vertices = nodeinfo_vertex_piqi (graph_code_graph_nodeinfo g);
    Graph_graphson.edges = edgeinfo_edge_piqi (graph_code_graph_edgeinfo g);
  }

let graphson_print g =
  let gs = graph_graphson_piqi g in
  let gs_json = Graph_print_options.graphson_options gs "pb" in
  write_file ("graphson.json") gs_json

let add_graphson_vertex n info =
  (* Checks if vertex with same hash has already been parsed *)
  if not (Hashtbl.mem vertex_id (node_hash n)) then 
     begin
      let vertex = nodeinfo_hash_element_piqi n info in
      if !graphson_vertex = true then 
        Graph_print_options.vertex_graphson vertex
      else
        begin
          Graph_print_options.create_initial_vertex vertex;
          graphson_vertex := true;
        end;
        Hashtbl.add vertex_id (node_hash n) true;
    end
  else
    ()

let add_graphson_edge e_key info =
  (* Checks if edge with same hash id hash already been parsed, and if node
   * defined in edgeinfo has been defined before edge is writte.
   *)
  if not (Hashtbl.mem edge_id (edge_hash e_key)) && (is_valid_edge e_key) then
       begin
        let edge = edgeinfo_hash_element_piqi e_key info in
        if !graphson_edge = true then 
          Graph_print_options.edge_graphson edge
        else
          begin
            Graph_print_options.create_initial_edge edge;
            graphson_edge := true;
          end;
          Hashtbl.add edge_id (edge_hash e_key) true;
      end
  else
    ()

