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
(* main entry point *)
(*****************************************************************************)

let _hmemo = Hashtbl.create 101
let g () =
  Common.memoized _hmemo "www" (fun () ->
    G.load "/home/pad/www/graph_code.marshall"
  )

let main_service = Eliom_registration.String.register_service 
  ~path:["blueprint"]
  ~get_params:(Eliom_parameter.string "class")
  (fun (classname) () ->

    let g = g () in
    
    let node = classname, E.Class E.RegularClass in
    if not (g +> G.has_node node)
    then failwith (spf "classname %s not found in code database" classname);

    let members = g +> G.children node in
    let hmembers = Common.hashset_of_list members in
    
    let json = 
      J.Object [
        "nodes", J.Array (
          members +> List.map (fun node ->
            let shortname = G.shortname_of_node node in
            let layer =
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
            in
            
            J.Object [
              "name", J.String shortname;
              "width", J.Int 2;
              "height", J.Int 3;
              "layer", J.Int (int_of_layer layer);
              "tooltip", J.String "TODO";
          ];
          ));
        "links", J.Array (
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
