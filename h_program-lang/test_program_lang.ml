open Common

module Db = Database_code
module G = Graph_code
module E = Database_code

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_load_light_db file = 
  let _db = Db.load_database file in
  ()

let test_big_grep file =
  let db = Db.load_database file in
  let entities = 
    Db.files_and_dirs_and_sorted_entities_for_completion 
      ~threshold_too_many_entities:300000
      db in
  let idx = Big_grep.build_index entities in
  let query = "old_le" in
  let top_n = 10 in

  let xs = Big_grep.top_n_search ~top_n ~query idx in
  xs +> List.iter (fun e ->
    let json = Db.json_of_entity e in
    let s = Json_io.string_of_json json in
    pr2 s
  );

  (* naive search *)
  let xs = Big_grep.naive_top_n_search ~top_n ~query entities in
  xs +> List.iter (fun e ->
    let json = Db.json_of_entity e in
    let s = Json_io.string_of_json json in
    pr2 s
  );
  ()

let test_layer file =
  let layer = Layer_code.load_layer file in
  let json = Layer_code.json_of_layer layer in
  let s = Json_out.string_of_json json in
  pr2 s

let layer_stat file =
  let layer = Layer_code.load_layer file in
  let stats = Layer_code.stat_of_layer layer in
  stats +> List.iter (fun (k, v) ->
    pr (spf " %s = %d" k v)
  )

let test_refactoring file =
  let xs = Refactoring_code.load file in
  xs +> List.iter pr2_gen;
  ()

(* ---------------------------------------------------------------------- *)
(* Code graph *)
(* ---------------------------------------------------------------------- *)
let test_graph_code () =
  let g = G.create () in
  g +> G.add_node G.root;

  g +> G.add_node ("a", E.Dir);
  g +> G.add_node ("c", E.Dir);
  g +> G.add_node ("a/b", E.Dir);
  g +> G.add_node ("a/b/foo.php", E.File);
  g +> G.add_node ("c/bar.php", E.File);

  g +> G.add_edge (("a", E.Dir), ("a/b", E.Dir)) G.Has;
  g +> G.add_edge (("a/b", E.Dir), ("a/b/foo.php", E.File)) G.Has;
  g +> G.add_edge (("c", E.Dir), ("c/bar.php", E.File)) G.Has;

  g +> G.add_edge (("a/b/foo.php", E.File), ("c/bar.php", E.File)) G.Use;
  G.display_with_gv g;
  ()

let test_dsm file =
  let g = Graph_code.load file in
  let config = Dependencies_matrix_code.basic_config g in
  let gopti = Graph_code_opti.convert g in
  let dm,_ = Dependencies_matrix_code.build config None gopti in
  Dependencies_matrix_code.display dm;
  ()

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-test_load_db",  " <file>",
  Common.mk_action_1_arg test_load_light_db;
  "-test_big_grep", " <file>",
  Common.mk_action_1_arg test_big_grep;
  "-test_layer", " <file>",
  Common.mk_action_1_arg test_layer;
  "-test_refactoring", " <file>",
  Common.mk_action_1_arg test_refactoring;
  "-test_graph_code", " <>",
  Common.mk_action_0_arg test_graph_code;
  "-test_dsm", " <file>",
  Common.mk_action_1_arg test_dsm;
]
