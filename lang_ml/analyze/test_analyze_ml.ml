open Common

let test_graph_code_ml dir =
  let verbose = false in
  let g = Graph_code_ml.build ~verbose dir in
  Graph_code.save g (Filename.concat dir "dependencies.marshall")


let actions () = [
  "-test_graph_code_ml", " <dir>",
  Common.mk_action_1_arg test_graph_code_ml;
]
