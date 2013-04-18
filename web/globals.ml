open Common

let verbose = ref true

(*****************************************************************************)
(* Projects *)
(*****************************************************************************)

(* type repo = Pfff | ... ? *)
let info_projects = [
  "pfff", ("/home/pad/pfff", Treemap_pl.ocaml_filter_file);

  "www", ("/home/pad/www", Treemap_pl.php_filter_file);
  "flib", ("/home/pad/www/flib", Treemap_pl.php_filter_file);
]

(*****************************************************************************)
(* Codegraph *)
(*****************************************************************************)

(* todo: memoize *)
let gopti_of_project prj =
  let (root, _) = List.assoc prj info_projects in
  let graph_file = Filename.concat root "graph_code.marshall" in
  Common.cache_computation ~verbose:!verbose graph_file ".opti"
    (fun () -> 
      let g = Graph_code.load graph_file in
      Graph_code_opti.convert g
    )

(*****************************************************************************)
(* Codemap *)
(*****************************************************************************)

let root_of_project project =
  let (root, _) = List.assoc project info_projects in
  root

(* todo: memoize *)
let rects_of_project_and_path (project, path) =
  let (root, filter) = List.assoc project info_projects in

  (* todo: sanitize path, disallow '..' *)
  let paths = [Filename.concat root path] in
  let treemap = Treemap_pl.code_treemap ~filter_file:filter paths in
  let algo = Treemap.Ordered Treemap.PivotByMiddle in
  let rects = Treemap.render_treemap_algo 
    ~algo ~big_borders:false
    treemap in
  rects

(*****************************************************************************)
(* Init *)
(*****************************************************************************)

let _ = begin
  pr2 "READY";
end
