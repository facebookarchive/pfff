open Common

let verbose = ref true

(* type repo = Pfff | ... ? *)

let graph_file = "/home/pad/pfff/graph_code.marshall"

let g = begin
  pr2 (spf "loading %s" graph_file);
  Graph_code.load graph_file;
end
  
let gopti = begin
  pr2 (spf "loading optimized cached graph");
  Common.cache_computation ~verbose:!verbose graph_file ".opti"
    (fun () -> Graph_code_opti.convert g)
end

let _ = begin
  pr2 "READY";
end
