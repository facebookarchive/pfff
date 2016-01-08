(*
 * Please imagine a long and boring gnu-style copyright notice
 * appearing just here.
 *)
open Common

module E = Entity_code
module GC = Graph_code
module GC2 = Graph_code_opti
module DM = Dependencies_matrix_code
module DMBuild = Dependencies_matrix_build

module Model = Model3
module View = View3

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* 
 * This is the main entry point of codegraph, a package/module/type/function/...
 * hierarchical dependency visualizer using mainly a Dependency
 * Structure Matrix (DSM). This should help understand the "software
 * architecture" of a project and assist in refactoring it.
 * A node-link display of hierarchical graphs (or hypergraphs) would be nice
 * too, but it is far more complex to draw than matrices and does
 * not scale as well visually apparently.
 * See http://en.wikipedia.org/wiki/Design_structure_matrix
 * A great introduction on the use of DSM for improving a codebase is
 * http://codebetter.com/patricksmacchia/2009/08/24/identify-code-structure-patterns-at-a-glance/
 * 
 * IMHO spending time to have clean dependencies is one of the most
 * important thing to do in a software project.
 * "Organizing complexity is the most important skill"
 * http://www.johndcook.com/blog/2015/06/18/most-important-skill-in-software/
 * 
 * 
 * It seems there are a few commercial projects using DSM (Ndepend,
 * Structure101, Intellij), so this looks like a viable direction to pursue
 * to visualize a software architecture.
 * 
 * requirements:
 *  - different granularities for x-to-x relationships
 *    (packages to packages, modules to packages, functions, constructors, etc),
 *    so one can get:
 *    * package (or directory) projection to reduce the size of the graph and
 *      get a high-level view of the architecture ("package mode")
 *    * with or without external/ dependencies
 *    * possibiltiy to get a slice of the graph for just a directory
 *      with a package (or directory) projection for external dependencies
 *      ("module mode")
 *    * possiblity to zoom to see the actual functions of a package involved
 *      in a dependency. This is especially useful for edges where
 *      we don't understand why there exists a dependency.
 *  - variable arrow size (but the count number in the matrix does that too)
 *  - variable node size (but the count number in same row does that too)
 * 
 * This tool also contains some actions to generate data for different
 * graph visualizer, e.g. Gephi, Guess. todo? backend for Graphviz? Phylomel?
 * old: $ pm_depend [-lang X] [-with-extern] [-depth n] -o filename /path/dir
 * 
 * related work: 
 * - Lattix, the startup of the original paper on DSM at OOPSLA'05
 * 
 * - Ndepend, http://www.ndepend.com/Doc_VS_Arch.aspx
 *   http://codebetter.com/patricksmacchia/2009/08/24/identify-code-structure-patterns-at-a-glance/
 * - Structure101, http://www.headwaysoftware.com/products/index.php#page-top
 * - Intellij IDEA dsm tool
 *   http://blogs.jetbrains.com/idea/2008/01/intellij-idea-dependency-analysis-with-dsm/
 *   http://www.jetbrains.com/idea/features/dependency_analysis.html
 * 
 * - http://depfind.sourceforge.net/, a dependency extraction tool for Java
 * - https://github.com/sourcegraph/graphkit, a generic format for code 
 *   dependencies and language specific "graphers" (ruby, js, go, python)
 * 
 * - http://mcis.polymtl.ca/~bram/makao/index.html also use GUESS and Prolog :)
 * - http://infotectonica.com/juliet/tour/, seems more oriented on
 *   query, anserwing questions like who uses this field.
 * 
 *  - visual studio dependency graph visualizer:
 *    http://msdn.microsoft.com/en-us/library/vstudio/dd409365.aspx
 *    http://channel9.msdn.com/Series/Visual-Studio-2012-Premium-and-Ultimate-Overview/Visual-Studio-Ultimate-2012-Understand-your-code-dependencies-through-visualization
 * 
 *  - a few visualizations applied on go dependencies (including chords)
 *    http://dave.cheney.net/2014/11/21/visualising-dependencies
 * 
 * - google search images: dependency+graph+visualization, get many
 *   links from there 
 * 
 * 
 * history:
 *  - quick look at work on software architecture because of Banatre
 *    while a master student at IRISA, and later Schmidt, while a PhD,
 *    looking at work of Shaw and Garlan and the different
 *    architecture patterns (whiteboard, pipe, layers, etc).
 *  - started to draw hypergraphs of architecture while supervising a
 *    student project at EMN (submarine game), and advocated they were
 *    better than UML diagrams (I think after reading Harel's papers on
 *    history of statecharts in HOPL-III)
 *  - dir to dir dependencies during coccinelle project? 
 *    Projections were hardcoded each time for each use. 
 *    No generic framework (like the hierarchical dependency matrix).
 *    Done for C (then for PHP later, and then for OCaml far later).
 *  - very nice picture of architecture of Linux kernel sent by Gilles,
 *    the "map of the Linux kernel"
 *  - found that having a graph of module dependencies was very useful
 *    when refactored the c-- and mmm codebase, thx to ocamldot.
 *    But felt the need to have variable-size arrows (and nodes) and also
 *    the ability to get more information when clicking on an edge, 
 *    to actually see what are the functions involved in a dependency
 *    for instance.
 *  - flibotonomy by Greg Scheschte for PHP, but focused on the nodes
 *    instead of the edges (which I think are more important).
 *  - overlay, and cmf -y to display dependencies at "package" level
 *  - pm_depend, ocaml dependencies backend, ~package_depth, ~with_extern.
 *    In some ways it extracts the dependency information I have 
 *    in my Makefiles where I care about the order of the directories
 *    and files. The ~package_depth and ~with_extern parameters are just
 *    special cases of the general idea of displaying at different
 *    granularity dependencies depending on the directory.
 *    Finally it was limited to just package/module (pm_depend) but quickly
 *    you want to know the actual functions/classes that are involved in
 *    a dependency.
 *  - gephi/guess visualization, but even with -no_extern, it does not
 *    scale very well for www. It's ok for pfff, but even for 
 *    the full source of pfff the graph is quite noisy.
 *  - discover DSM of ndepend.com while doing a google search images on
 *    "dependency+graph+visualization"
 * 
 *  - gradually realize the importance of layered structures
 *    (which are actually enforced in OCaml by the linker)
 *  - gradually realize the importance of dependencies and how
 *    they are at the essence of software architecture. Code is
 *    a tree when looked locally (AST), but it's really a graph
 *    of dependencies when looked globally.
 * 
 * todo: 
 *  - can codegraph does a good job to convey the software architecture
 *    of codegraph itself? does it show clearly that Graph_code.graph and
 *    dependencies_matrix_code.dm are the essential data structures? And
 *    can it show the important fields?
 *  - maybe edge-bundles could make the node-link display approach
 *    scale better.
 *  - generate a node-link graph for the current matrix configuration; use
 *    graphviz as a first step.
 *  - http://redotheweb.com/DependencyWheel/?
 * 
 * 
 * (comments that were in graph_modules_packages_ml.ml)
 * alternatives:
 *  - ocamldoc -dot ...
 *    But if there is one parse error or a module not found, then 
 *    ocamldoc fails. Also there is no package "projection" so it's 
 *    hard to apply on large projects. There is also no with-extern view
 *    with the slice of the graph to a directory.
 *    TODO it can potentially support more code though, by using camlp4.
 *  - ocamldoc with graphviz  
 *    graphviz does not do variable node size for free as in gephi
 *  - ocamldoc -dot-reduce ... 
 *    The -dot-reduce is good for layering, but sometimes
 *    it's also good to see things without the reduction (especially
 *    with gephi). See for instance the graph for tiger with ocamldoc
 *    vs pm_depend. I can see all the real callers to option.ml.
 *    TODO the reduce and layering is also useful
 *  - ocamldoc -dot-colors
 *    TODO this is useful.
 *    It's somehow covered by the strongly-connected + coloring in gephi.
 *  - graphviz backend? graphviz is good for layers, but
 *    you lose space because the high-level stuff is at the top but alone.
 *    With gephi/fatlas, by putting high-level stuff at the center, 
 *    you lose less space? Also graphviz does not scale very well.
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

(* you can also put this in your ~/gtkrc-2.0
 * gtk-icon-theme-name = "Tango"
 * gtk-theme-name = "Murrine-Gray"
 * gtk-font-name = "DejaVu Sans 16"
 *)

let lang = ref "ml"
let deps_style = ref DM.DepsInOut

let output_dir = ref None
(* generate also tags, light db, layers, etc *)
let gen_derived_data = ref false
(* not perfect ... *)
let class_analysis = ref false

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let constraints_of_info_txt info_txt =
  let h = Hashtbl.create 101 in
  (* pr2_gen info_txt; *)
  let rec aux current node =
    match node with
    | Common2.Tree (node, xs) ->
      let title = node.Outline.title in
      let entry = 
        match title with
        | "__ROOT__" -> "."
        | _ -> Filename.concat current title
      in
      let children = xs +> List.map (fun (Common2.Tree (node, _)) ->
        (match entry with
        | "." -> node.Outline.title
        | _ -> Filename.concat entry node.Outline.title
        )
      )
      in
      if not (null children) then begin
        (* pr2_gen (entry, children); *)
        Hashtbl.add h entry children;
        let new_current =
          match entry with
          | "." -> ""
          | _ -> entry
        in
        List.iter (aux new_current) xs
      end
  in
  aux "" info_txt;
  h

let set_gc () =
  (* only relevant in bytecode, in native the stacklimit is the os stacklimit*)
  Gc.set {(Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024};
  (* see www.elehack.net/michael/blog/2010/06/ocaml-memory-tuning *)
  Gc.set { (Gc.get()) with Gc.minor_heap_size = 4_000_000 };
  (* goes from 5300s to 3000s for building db for www *)
  Gc.set { (Gc.get()) with Gc.major_heap_increment = 8_000_000 };
  Gc.set { (Gc.get()) with Gc.space_overhead = 300 };
  ()
  
(*****************************************************************************)
(* Model Helpers *)
(*****************************************************************************)

let dep_file_of_dir dir = 
  Filename.concat dir Graph_code.default_filename

let build_model root =
  let file = dep_file_of_dir root in
  let g = Graph_code.load file in
  let gopti = 
    Common.cache_computation ~verbose:!verbose file ".opti"
      (fun () -> Graph_code_opti.convert g)
  in
  (* todo: find -name "info.txt" *)
  let constraints =
    if Sys.file_exists (Filename.concat root "info.txt")
    then begin
      pr2 (spf "using %s" (Filename.concat root "info.txt"));
      let info_txt = Info_code.load (Filename.concat root "info.txt") in
      constraints_of_info_txt info_txt
    end
    else Hashtbl.create 0
  in
  { Model.g_deprecated = g; gopti = gopti; 
    root = Common.realpath root;
    constraints 
  }

let dir_node xs =     
  (Common.join "/" xs, Entity_code.Dir)
let package_node xs = 
  (Common.join "." xs, Entity_code.Package)


(*****************************************************************************)
(* Language specific, building the graph *)
(*****************************************************************************)

(* less: maybe could be moved in pfff_db.ml? so that the -lang are isolated
 * to pfff_db, and neither codegraph nor codemap need them!
 *)
let build_graph_code lang xs =
  let xs = List.map Common.realpath xs in
  let root, files = 
    match xs with
    | [root] -> 
        root, Find_source.files_of_root ~lang root
    | _ ->
        let root = Common2.common_prefix_of_files_or_dirs xs in
        let files = 
          Find_source.files_of_dir_or_files ~lang ~verbose:!verbose xs in
        root, files
  in

  let empty = Graph_code.empty_statistics () in
  let g, stats =
    try (
    match lang with
    | "ml"  -> Graph_code_ml.build ~verbose:!verbose root files, empty
#if FEATURE_CMT
    | "cmt"  -> 
          let ml_files = Find_source.files_of_root ~lang:"ml" root in
          let cmt_files = files in
          Graph_code_cmt.build ~verbose:!verbose ~root ~cmt_files ~ml_files, 
          empty
#endif

    | "php" -> 
      (* todo: better factorize *)
      let skip_file = Filename.concat root "skip_list.txt" in
      let skip_list =
        if Sys.file_exists skip_file
        then Skip_code.load skip_file
        else []
      in
      let is_skip_error_file = Skip_code.build_filter_errors_file skip_list in
      Graph_code_php.build 
        ~verbose:!verbose ~is_skip_error_file 
        ~class_analysis:!class_analysis
        root files
    | "web" -> raise Todo

    | "c" -> 
        Parse_cpp.init_defs !Flag_parsing_cpp.macros_h;
        let local = Filename.concat root "pfff_macros.h" in
        if Sys.file_exists local
        then Parse_cpp.add_defs local;
        Graph_code_c.build ~verbose:!verbose root files, empty
    | "clang2" -> Graph_code_clang.build ~verbose:!verbose root files, empty

    | "java" -> Graph_code_java.build ~verbose:!verbose root files, empty
#if FEATURE_BYTECODE
    | "bytecode" -> 
      let graph_code_java =  None 
(*        Some (Graph_code_java.build ~verbose:!verbose ~only_defs:true 
                 root skip_list)
*)
      in
      Graph_code_bytecode.build ~verbose:!verbose ~graph_code_java root files,
      empty
#endif

    | "lisp" -> Graph_code_lisp.build ~verbose:!verbose root files, empty

    | "dot" -> 
      Graph_code.graph_of_dotfile (Filename.concat root "graph.dot"), empty

    | _ -> failwith ("language not supported: " ^ lang)
    )
    with Graph_code.Error err ->
      pr2 (Graph_code.string_of_error err);
      raise (Graph_code.Error err)
  in
  let output_dir = !output_dir ||| (Sys.getcwd()) in
  Graph_code.save g (dep_file_of_dir output_dir);
  Graph_code.print_statistics stats g;

  (* Save also TAGS, light db, prolog (TODO), layers. We could also do
   * that on demand when we run codemap and there is only a 
   * graph_code.marshall file.
   *)
  if !gen_derived_data then begin
    let p f = Filename.concat output_dir f in
    Layer_graph_code.gen_rank_heatmap_layer g (GC.bottom_up_numbering g) 
      (p "layer_bottomup.json");
    Layer_graph_code.gen_rank_heatmap_layer g (GC.top_down_numbering g) 
      (p "layer_topdown.json");
    Layer_graph_code.gen_statistics_layer ~root stats 
      ~output:(p "layer_graphcode_stats.json");
    let defs = Graph_code_tags.defs_of_graph_code g in
    Tags_file.generate_TAGS_file (p "TAGS") defs;
    let db = Graph_code_database.db_of_graph_code root g in
    Database_code.save_database db (p "PFFF_DB.marshall");
    Database_code.save_database db (p "PFFF_DB.json");
  end;
  ()

(*****************************************************************************)
(* Language specific, building stdlib *)
(*****************************************************************************)
let build_stdlib lang root dst =
  let files = Find_source.files_of_root ~lang root in
  match lang with
  | "java" ->
      Builtins_java.extract_from_sources ~src:root ~dst files
  | "clang" ->
      Uninclude_clang.uninclude ~verbose:!verbose root files dst
  | _ -> failwith ("language not supported: " ^ lang)

(*****************************************************************************)
(* Main action, viewing the graph *)
(*****************************************************************************)

(* algo: 
 *  - find root of project with a dependencies.marshall file
 *  - display slice if needed of the dependency hieararchical matrix 
 *    using arguments in xs.
 * No need of -with_extern anymore, external stuff will be collapsed.
 * No need of -package_depth, can expand on demand after.
 * 
 * todo: How load graph? Build on demand? easier to test things that way ... 
 * maybe can just cache and look if we need to recompute the code graph?
 * same for the dependency matrix that we can cache too.
 *)
let main_action xs =
  set_gc ();
  Logger.log Config_pfff.logger "codegraph" None;
  let _locale = GtkMain.Main.init () in

  let dir = 
    match xs with 
    | [x] -> 
        if x = "." 
        (* getcwd() display a realpath *)
        then List.hd (Common.cmd_to_list "pwd")
        else
          if Sys.is_directory(x)
          then x
          else failwith "not a valid directory: codegraph <dir/to/graph_code.marshall>"
    | _ -> failwith "unable to parse directory: codegraph <dir/to/graph_code.marshall>"
  in
  let inits = Common2.inits_of_absolute_dir dir in
  let root =
    inits +> List.rev +> List.find (fun path -> 
      Sys.file_exists (dep_file_of_dir path))
  in
  pr2 (spf "Using root = %s" root);
  let model = build_model root in

  let path =
    if root =*= dir
    then []
    else begin
      (* Propose a specific slice of the graph.
       * If run cg from a/b/c, then Expand a, Expand a/b,
       * then Focus a/b/c, and optionally Expand a/b/c.
       *)
      let readable_subdir =
        let xs = Common.split "/" root in
        let ys = Common.split "/" dir in
        let (a, b) = Common2.splitAt (List.length xs) ys in
        assert (xs =*= a);
        b
      in
      let dir_or_package, start =
        if GC.has_node (dir_node readable_subdir) model.Model.g_deprecated
        then dir_node, readable_subdir
        else package_node, 
              try
               Common2.tails readable_subdir +> List.find (fun xs ->
                 GC.has_node (package_node xs) model.Model.g_deprecated
               )
              with Not_found ->
                failwith "could not find a Dir or Package"
      in
      let (str, kind) = dir_or_package start in
      pr2 (spf "focusing on %s %s" 
              (Entity_code.string_of_entity_kind kind) str);
      let rec aux before xs =
        match xs with
        | [] -> raise Impossible
        | [x] ->
            let node = dir_or_package (List.rev (x::before)) in
            [DM.Focus (node, !deps_style); DM.Expand node;]
        | x::xs ->
            let node = dir_or_package (List.rev (x::before)) in
            (DM.Expand node)::aux (x::before) xs
      in
      (aux [] start)
    end
  in

  let w = Model.init_world path model in
  View.mk_gui w

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)

(* Analysis *)
let analyze_backward_deps graph_file =
  let g = GC.load graph_file in
  let gopti = 
    Common.cache_computation ~verbose:!verbose graph_file ".opti"
      (fun () -> Graph_code_opti.convert g)
  in
  let config = DM.basic_config_opti gopti in
  (* DM.threshold_pack := 90; *)
  let config = DM.expand_node_opti (("flib", E.Dir)) config gopti in
  let dm, gopti = DMBuild.build config None gopti in
  let n = Array.length dm.DM.matrix in
  (* todo: filter biggest offenders? *)
  let res = ref [] in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      let n = dm.DM.matrix.(i).(j) in
      if n > 0 then begin
        let xs = DM.explain_cell_list_use_edges (i, j) dm gopti in
        pr2 (spf " (%d, %d) = %d" i j (List.length xs));
        Common.push xs res;
      end
    done
  done;
  let edges = List.flatten !res in
  pr2 (spf "total backward deps = %d" (List.length edges));
  let xxs = Common.group_by_mapped_key (fun (_n1, n2) -> n2) edges in
  pr2 (spf "#dst =%d" (List.length xxs));
  xxs +> List.map (fun (n, xs) -> (n, xs), List.length xs)
    +> Common.sort_by_val_highfirst
    +> Common.take_safe 100
    +> List.iter (fun ((n, _xs), cnt) ->
        let file = GC.file_of_node n g in
         pr2 (spf "%-30s = %d (file = %s)" (GC.string_of_node n) cnt
                file)
    );
  let file = graph_file ^ ".whitelist" in
  pr2 (spf "generating whitelist in %s" file);
  GC.save_whitelist edges file g;
  
  ()



(* Graph adjuster (overlay-ish) *)
let adjust_graph graph_file adjust_file whitelist_file dest_file =
  let g = Graph_code.load graph_file in
  let adjust = Graph_code.load_adjust adjust_file in
  let whitelist = Graph_code.load_whitelist whitelist_file in
  Graph_code.adjust_graph g adjust whitelist;
  Graph_code.save g dest_file;
  ()

(* quite similar to analyze_backward_deps *)
let test_thrift_alive graph_file =
  let g = GC.load graph_file in
  let gopti = 
    Common.cache_computation ~verbose:!verbose graph_file ".opti"
      (fun () -> Graph_code_opti.convert g)
  in
  let config = DM.basic_config_opti gopti in
  DMBuild.threshold_pack := max_int;
  let config = DM.expand_node_opti (("lib", E.Dir)) config gopti in
  let config = DM.expand_node_opti (("lib/thrift", E.Dir)) config gopti in
  let config = DM.expand_node_opti (("lib/thrift/packages", E.Dir)) config gopti in
  let dm, gopti = DMBuild.build config None gopti in
  let n = Array.length dm.DM.matrix in

  let kflib = Hashtbl.find dm.DM.name_to_i ("flib", E.Dir) in
  for j = 0 to n - 1 do
    let (s, _kind) = dm.DM.i_to_name.(j) in
    if s =~ "lib/thrift/packages/.*"
    then begin
      let v = dm.DM.matrix.(kflib).(j) in
      if v > 0 then begin
        pr2 (spf "%s is USED in flib/" s);
        let xs = DM.explain_cell_list_use_edges (kflib, j) dm gopti in
        xs +> Common.take_safe 5 +> List.iter (fun (n1, n2) ->
          pr2 (spf "    %s --> %s" 
                 (GC.string_of_node n1) (GC.string_of_node n2))
        )
      end else begin
        if DM.is_dead_column j dm
        then begin
          pr2 (spf "%s appeared DEAD" s);
        end
      end
    end
  done

(* quite similar to analyze_backward_deps *)
let test_adhoc_deps graph_file =
  let g = GC.load graph_file in
  g +> GC.iter_use_edges (fun n1 n2 ->
    let file = GC.file_of_node n2 g in
    if file =~ ".*flib/intern/thrift/lib"
    then begin
      let file2 = GC.file_of_node n1 g in
      if file2 =~ ".*tests/" || file2 =~ ".*/__tests__/.*"
      then begin
        pr2_once (spf "%s --> %s" file2 file);
        (*pr2 (spf " %s --> %s" (GC.string_of_node n1) (GC.string_of_node n2));*)
      end
    end
  )

let test_layering graph_file =
  let g = GC.load graph_file in
  let (scc, _hscc) = GC.strongly_connected_components_use_graph g in
  pr2 (spf "#scc = %d" (Array.length scc));
  let htopdown = GC.bottom_up_numbering g in
  pr2 (spf "computed numbering = %d" (Hashtbl.length htopdown));
  let xs = htopdown +> Common.hash_to_list +> List.map snd in
  let min = Common2.minimum xs in
  assert(min = 0);
  let max = Common2.maximum xs in
  pr2 (spf "max = %d" max);
  
  let file = "/tmp/rank_code.txt" in
  pr2 (spf "ranks in %s" file);
  Common.with_open_outfile file (fun (pr, _chan) ->
    let pr s = pr (s ^ "\n") in
    htopdown +> Common.hash_to_list +> Common.sort_by_val_lowfirst
    +> List.iter (fun (node, v) -> 
      pr (spf "%s: %d" (GC.string_of_node node) v)
    )
  );

  let (d,_,_) = Common2.dbe_of_filename graph_file in
  let output = Common2.filename_of_dbe (d, "layer_graph_code", "json") in
  Layer_graph_code.gen_rank_heatmap_layer g htopdown output;
  ()


let test_xta graph_file = 
  let g = Graph_code.load graph_file in
  let dag = Graph_code_class_analysis.class_hierarchy g in
  let hdepth = Graph.depth_nodes dag in
  hdepth +> Hashtbl.iter (fun k v ->
    pr2 (spf "%s = %d" (Graph_code.string_of_node k) v);
  );
  let dag = Graph_code_class_analysis.class_hierarchy g in
  let htoplevels = Graph_code_class_analysis.toplevel_methods g dag in
  htoplevels +> Common2.hkeys +> List.iter (fun k ->
      let xs = Hashtbl.find_all htoplevels k in
      pr2 (spf "%s -> %d (e.g. %s)" 
               k (List.length xs) (Graph_code.string_of_node (List.hd xs)));
  );
  ()

let test_dotfile_of_deps dir =
  let deps = Common.cmd_to_list (spf "find %s -name \"*.deps\" " dir) in
  deps +> List.iter (fun file ->
    let (_d,lib,e) = Common2.dbe_of_filename file in
    if e = "deps" then begin
      let deps = Common.cat file in
      deps +> List.iter (fun lib2 ->
        pr (spf "\"%s\" -> \"%s\"" lib lib2)
      )
    end
  )  

(* This is actually not great because the .depend contains the dependencies
 * for the .o and so it does not contain inter .h dependencies.
 * Use http://www.flourish.org/cinclude2dot/ instead.
 *)
let test_dotfile_of_dotdepend file =
  let rec remove_antislash xs =
    match xs with
    | [] -> []
    | x::y::xs ->
      if x =~ "\\(.*\\)\\\\$"
      then remove_antislash ((Common.matched1 x ^ y)::xs)
      else x::(remove_antislash (y::xs))
    | [x] -> [x]
  in
  let lines = Common.cat file in
  let lines = remove_antislash lines in
  (* lines |> List.iter pr2_gen *)
  lines |> List.iter (fun line ->
    match () with
    | _ when line =~ ".*\\.o: \\([^ ]+\\)\\.cpp\\(.*\\)$" ->
      let (src, deps_str) = Common.matched2 line in
      let deps = Common.split "[ \t]+" deps_str in
      deps |> List.iter (fun dep ->
        pr (spf "\"%s.cpp\" -> \"%s\""  src dep)
      )
    | _ when line =~ ".*\\.o: \\([^ ]+\\)\\.h\\(.*\\)$" ->
      let (src, deps_str) = Common.matched2 line in
      let deps = Common.split "[ \t]+" deps_str in
      deps |> List.iter (fun dep ->
        pr (spf "\"%s.cpp\" -> \"%s\""  src dep)
      )

    | _ -> failwith (spf "wront line format in .depend: %s" line)
  )

  
(* ---------------------------------------------------------------------- *)
let extra_actions () = [

  "-build", " <dirs> build a graph_code.marshall database",
  Common.mk_action_n_arg (fun dirs -> build_graph_code !lang dirs);
  "-build_stdlib", " <src> <dst>",
  Common.mk_action_2_arg (fun dir dst -> build_stdlib !lang dir dst);
  "-adjust_graph", " <graph> <adjust_file> <whitelist> <dstfile>\n",
  Common.mk_action_4_arg (fun graph file file2 dst -> 
    adjust_graph graph file file2 dst);


  "-test_backward_deps", " <graph>",
  Common.mk_action_1_arg (fun graph_file -> 
    analyze_backward_deps graph_file
  );
  "-test_protected_to_private", " <graph>",
  Common.mk_action_1_arg (fun graph_file ->
    let g = Graph_code.load graph_file in
    Graph_code_class_analysis.protected_to_private_candidates g
  );
  "-test_thrift_alive", " <graph>",
  Common.mk_action_1_arg test_thrift_alive;
  "-test_pad", " <graph>",
  Common.mk_action_1_arg test_adhoc_deps;
  "-test_layering", " <graph>",
  Common.mk_action_1_arg test_layering;
  "-test_xta", " <graph>",
  Common.mk_action_1_arg test_xta;
  "-test_dotfile_of_deps", " <dir>",
  Common.mk_action_1_arg test_dotfile_of_deps;
  "-test_dotfile_of_dotdepend", " <file>",
  Common.mk_action_1_arg test_dotfile_of_dotdepend;
(*
  "-test_phylomel", " <geno file>",
  Common.mk_action_1_arg test_phylomel;
*)
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  Layer_graph_code.actions() @
  extra_actions () @
  Test_program_lang.actions() @
  []

let options () = [
  "-deps", Arg.String (fun s -> 
    deps_style := 
      (match s with
      | "in" -> DM.DepsIn
      | "out" -> DM.DepsOut
      | "inout" -> DM.DepsInOut
      | _ -> failwith "wrong parameter for --deps"
      )
  ), 
  " <in|out|inout>";

  "-dots_threshold", Arg.Int (fun i -> DMBuild.threshold_pack := i),
  " <int> when do we introduce '...' entries";

  "-lang", Arg.Set_string lang, 
  (spf " <str> choose language (default = %s) (for -build)" !lang);
  "-o", Arg.String (fun s -> output_dir := Some s), 
  " <dir> save graph_code.marshall in another dir (for -build)";
  "-derived_data", Arg.Set gen_derived_data, 
  " generate also TAGS, layers, light db, etc (for -build)";

  "-class_analysis", Arg.Set class_analysis, 
  " resolve some method calls";

  "-symlinks", Arg.Unit (fun () -> Common.follow_symlinks := true;), 
  " follow symlinks (for -build) ";
 
  "-no_fake_node", Arg.Clear Graph_code_php.add_fake_node_when_undefined_entity,
  " no fake nodes when use-def mismatches (for -build)\n";

  ] @
  Common.options_of_actions action (all_actions()) @
  Common2.cmdline_flags_devel () @
  [
  "-verbose", Arg.Unit (fun () ->
    verbose := true;
    DM.verbose := true;
  ), " ";

    "-version",   Arg.Unit (fun () -> 
      pr2 (spf "CodeGraph version: %s" Config_pfff.version);
      exit 0;
    ), 
    " guess what";
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 

  (* Common_extra.set_link(); *)
  let usage_msg = 
    spf "Usage: %s [options] <dir> \nDoc: %s\nOptions:"
      (Filename.basename Sys.argv.(0))
      "https://github.com/facebook/pfff/wiki/Codegraph"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () -> 
    (match args with
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) -> 
        Common.do_action !action xs (all_actions())

    | _ when not (Common.null_string !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 
        main_action (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> 
        Common.usage usage_msg (options())
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
    main ();
  )
