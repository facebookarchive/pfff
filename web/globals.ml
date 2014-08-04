open Common

module FT = File_type
module T = Treemap

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref true
let use_facebook = ref true

(*****************************************************************************)
(* Projects *)
(*****************************************************************************)

let filters = [
  "ocaml", (fun file ->
    match File_type.file_type_of_file file with
    | FT.PL (FT.ML _) | FT.PL (FT.Makefile)  -> true
    | _ -> false
  );
  "php", (fun file ->
    match File_type.file_type_of_file file with
    | FT.PL (FT.Web (FT.Php _)) -> true  | _ -> false
  );
  "js", (fun file ->
    match File_type.file_type_of_file file with
    | FT.PL (FT.Web (FT.Js)) -> true  | _ -> false
  );
]


(* type repo = Pfff | ... ? *)
let info_projects = [
  "pfff", ("/home/pad/pfff", (List.assoc "ocaml" filters));

  "www", ("/home/pad/www", (List.assoc "php" filters));
  "flib", ("/home/pad/www/flib", (List.assoc "php" filters));
  "hack", ("/home/pad/www/flib", (List.assoc "php" filters));

  "fbcode", ("/home/pad/local/fbcode", (fun _ -> true));
  "fb4a", ("/home/pad/local/fb4a", (fun _ -> true));
  "fbobjc", ("/home/pad/local/fbobjc/Libraries", (fun _ -> true));

  "epriestley", ("/home/pad/local/epriestley", (List.assoc "php" filters));
]

let dimensions_of_size = function
  | "small" -> 1200, 680
  | "large" -> 2500, 1490
  | "extralarge" -> 7000, 4000
  | _ -> failwith "size not regonized, try 'large' or 'small'"

(*****************************************************************************)
(* Codegraph *)
(*****************************************************************************)

let _hmemo_gopti = Hashtbl.create 101

let gopti_of_project prj =
  let (root, _) = List.assoc prj info_projects in
  let graph_file = Filename.concat root "graph_code.marshall" in
  Common.cache_computation ~verbose:!verbose graph_file ".opti"
    (fun () -> 
      let g = Graph_code.load graph_file in
      Graph_code_opti.convert g
    )

let gopti_of_project prj =
  Common.memoized _hmemo_gopti prj (fun () ->
    ref (gopti_of_project prj)
  )


(*****************************************************************************)
(* Codemap *)
(*****************************************************************************)

let root_of_project project =
  let (root, _) = List.assoc project info_projects in
  root

let _hmemo_index = Hashtbl.create 101
let layers_with_index_of_file (root, file) =
  Common.memoized _hmemo_index (root, file) (fun () ->
    
    let file = Filename.concat root file in
    let layer = Layer_code.load_layer file in
    let active = true in
    Layer_code.build_index_of_layers ~root [layer, active] 
  )

let rects_of_project_and_path (project, path) =
  let (root, filter) = List.assoc project info_projects in

  (* todo: sanitize path, disallow '..' *)
  let paths = [Filename.concat root path] in
  (* less: generate readable path directly *)
  let treemap = Treemap_pl.code_treemap ~filter_file:filter paths in
  let algo = Treemap.Ordered Treemap.PivotByMiddle in
  let rects = Treemap.render_treemap 
    ~algo ~big_borders:false
    treemap in

  let rects =
    match project with
    | "hack" ->
      let layers_with_index = 
        layers_with_index_of_file (root, "layer_hack.json") in
      rects +> List.map (fun r ->
        let file = r.T.tr_label in
        let is_file = not r.T.tr_is_node in
        let emacs_color = 
          if is_file then begin
            try 
              let xs = 
                Hashtbl.find layers_with_index.Layer_code.macro_index file in
              let (_float, emacs_color) = List.hd xs in
              emacs_color
            with
              Not_found -> "white"
          end
          else "black" 
        in
        let int_color = Simple_color.color_of_string emacs_color in
        { r with T.tr_color = int_color }
      )
    | _ -> rects
  in
  rects

let _hmemo_rects = Hashtbl.create 101
(* todo: memoize *)
let rects_of_project_and_path (a,b) =
  Common.memoized _hmemo_rects (a,b) (fun () ->
    rects_of_project_and_path (a,b)
  )

(*****************************************************************************)
(* Init *)
(*****************************************************************************)

let _ = begin
  (* only relevant in bytecode, in native the stacklimit is the os stacklimit*)
  Gc.set {(Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024};
  (* let _ = rects_of_project_and_path ("hack", "") in *)
  pr2 "READY";
end
