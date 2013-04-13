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

let paths = ["/home/pad/pfff"]
let filter = Treemap_pl.ocaml_filter_file
(*
let paths = ["/home/pad/www/flib"]
let filter = Treemap_pl.php_filter_file
*)

let treemap = Treemap_pl.code_treemap ~filter_file:filter paths
let algo = Treemap.Ordered Treemap.PivotByMiddle
let rects = Treemap.render_treemap_algo 
    ~algo ~big_borders:false
    treemap 

let _ = begin
  pr2 "READY";
end
