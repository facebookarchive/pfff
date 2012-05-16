open Common

let test_graph_code_ml dir =
  let verbose = true in
  let _g = Graph_code_ml.build ~verbose dir in
  (* pr2_gen g *)
  ()

let actions () = [
  "-test_graph_code_ml", " <dir>",
  Common.mk_action_1_arg test_graph_code_ml;
]
