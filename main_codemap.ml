(*s: main_codemap.ml *)
(*
 * Please imagine a long and boring gnu-style copyright notice 
 * appearing just here.
 *)
open Common

module Flag = Flag_visual
module FT = File_type

module Model = Model2

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This is the main entry point of codemap, a semantic source code visualizer
 * using treemaps and code thumbnails. The focus here is code understanding
 * not editing, so for instance even if features like autocompletion are
 * great for editing, they are not really helpful for understanding an existing
 * codebase. What can help is completion to help navigate and go from one
 * place to another, and this is one of the feature of this tool.
 * 
 * requirements:
 *  - get a bird's eye view of all the code (hence treemaps)
 *  - get a bird's eye view of a file (hence code thumbnails)
 *  - better syntax highlighting than Emacs, using real parsers so
 *    we can colorize differently identifiers (a function vs a field vs
 *    a constant)
 *  - important code should be bigger. Just like in google maps
 *    the important roads are more visible. So need some sort of
 *    global analysis.
 *  - show the data (the source code), but also show the relations
 *    (hence codegraph integration)
 *  - look at the code through different views (hence layers)
 * 
 * history:
 *  - saw Aspect Browser while working on aspects as an intern at IRISA
 *  - work on Poffs and idea of visualizing the same code through 
 *    different views
 *  - talked about mixing sgrep/spatch with code visualization,
 *    highlighting with a certain color different architecture aspects
 *    of the Linux kernel (influenced by work on aspect browser)
 *  - talked about fancy code visualizer while at cleanmake with YY,
 *    spiros, etc.
 *  - saw SeeSoft code visualizer while doing some bibliographic work
 *  - saw code thumbnails by MSR, and Rob Deline
 *  - saw treemap of Linux kernel by fekete => idea of mixing
 *    tree-map+code-thumbnails+seesoft = codemap
 *  - saw talk at CC about improving javadoc by putting in bigger fonts
 *    really often used API functions => idea of light db and semantic
 *    visual feedback
 *  - read hierarchical edge bundling paper and its d3 implementation to 
 *    visualize on top of a treemap the call graph
 * 
 * related work:
 *  - racket IDE (was called DrScheme before), had arrows long time ago
 *    between occurences of a variable and its definition
 *  - http://peaker.github.io/lamdu/, but focused more on AST pretty printing
 *  - light table, interesting visualization slice but now focused more
 *    on live programming a la Bret Victor
 *  - http://www.kickstarter.com/projects/296054304/zeta-code, mostly focused
 *    on code relations, so related more to codegraph
 *  - textmate, meh
 *  - sublime, has thumbnails, but people don't really care about it
 *  - http://www.hello2morrow.com/products/sotoarc
 *  - http://scg.unibe.ch/codemap
 *  - http://scg.unibe.ch/wiki/projects/rbcrawler, class blueprint
 *  - moose http://youtu.be/yvXm9LC17vk at 14min
 *  - http://redotheweb.com/CodeFlower/
 *  - code swarm, visualize git history, focused on people more than code
 *    https://code.google.com/p/gource/ 
 *    http://artzub.com/ghv/#repo=d3&climit=100&user=mbostock
 *  - http://www.codetrails.com/ctrlflow, smarter completion by infering
 *    importance of method (like I do, by #times this entity is globally used)
 * 
 * features of IDE we do want (e.g. see the list at http://xamarin.com/studio):
 *  - smart syntax highlighting
 *  - go to definition (=~ TAGS, light db and search bar completion provides it)
 *  - code navigation (directory, files, also "hypertext" go to def/uses)
 *  - find uses (funcs, classes, TODO tricky for methods in dynamic languages)
 *  - code tooltip, hover on use of an entity to display information about
 *    it (#uses, TODO: type/args, comments, code, age, methods, etc)
 *  - unified search (files, entities, TODO but also content)
 *  - debugger? it helps understand code so a coverage layer or TODO live
 *    coverage tracing would be nice (as in tracegl)
 *  - source control? extract age, number of authors, churn information in
 *    layers
 * 
 * features of IDE we care less about:
 *  - folding/outline? thumbnails make this less important
 *  - auto completion? no. One nice thing of autocomplete though is that
 *    it proposes all the possible methods of an object, the overriden
 *    as well as not overriden parent methods.  We don't want autocomplete
 *    but we want the ability to understand a class by TODO "inlining" parent 
 *    methods that are relevant to understand the local code of the class
 *    (e.g. the short command of Eiffel)
 *  - refactoring? no
 *  - UI designer? no
 *  - deploy assistant, cloud assistant? no
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(*s: main flags *)
(* on Macos lion, X11 resizes the window to a smaller size so
 * no point in starting with a big screen_size :(
 *)
let screen_size = ref 1
let legend = ref true

(* if not specified, codemap will try to use files in the current directory *)
let db_file    = ref (None: Common.filename option)
let graph_file = ref (None: Common.filename option)
let layer_file = ref (None: Common.filename option)
let layer_dir  = ref (None: Common.dirname option)

(* See also Gui.synchronous_actions *)
let test_mode = ref (None: string option)
(*e: main flags *)

(* see filters below, which files we are interested in *)
let filter = ref (fun _file -> true)
let skip_file  = ref (None: Common.filename option)
(* less: a config file: GtkMain.Rc.add_default_file "/.../pfff_browser.rc"; *)

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Shortcuts *)
(*****************************************************************************)

let filters = [
  (* pad specific, ocaml related *)
  "pfff", (fun file ->
    match FT.file_type_of_file file with
    | FT.PL (
       (FT.ML _) | FT.Makefile | FT.Opa | FT.Prolog _ | FT.Web (FT.Php _)) -> 
        (* todo: should be done in file_type_of_file *)
        not (FT.is_syncweb_obj_file file)
        && not ( 
                (* file =~ ".*commons/" || *)
                (* file =~ ".*external/" || *)
                file =~ ".*_build/")
    | _ -> false
  );
  "ocaml", (fun file ->
    match File_type.file_type_of_file file with
    | FT.PL (FT.ML _) | FT.PL (FT.Makefile)  -> 
      (* todo: should be done in file_type_of_file *)
      not (File_type.is_syncweb_obj_file file)
    | _ -> false
  );
  "mli", (fun file ->
    match File_type.file_type_of_file file with
    | FT.PL (FT.ML "mli") | FT.PL (FT.Makefile)   -> 
    (* todo: should be done in file_type_of_file *)
      not (File_type.is_syncweb_obj_file file) &&
      not (file =~ ".*/commons/")
    | _ -> false
  );
  "nw", (fun file -> 
    match FT.file_type_of_file file with
    | FT.Text "nw" -> true | _ -> false
  );

  (* other languages *)
  "php", (fun file ->
    match File_type.file_type_of_file file with
    | FT.PL (FT.Web (FT.Php _)) -> true  | _ -> false
  );
  "js", (fun file ->
    match File_type.file_type_of_file file with
    | FT.PL (FT.Web (FT.Js)) -> true  | _ -> false
  );

  "cpp", (let x = ref false in (fun file ->
    Common2.once x (fun () -> Parse_cpp.init_defs !Flag_parsing_cpp.macros_h);
    match FT.file_type_of_file file with
    | FT.PL (FT.C _ | FT.Cplusplus _) -> true 
    | _ -> false
  ));

  (* exotic languages *)
  "opa", (fun file -> 
    match FT.file_type_of_file file with
    | FT.PL (FT.Opa) (* | FT.PL (FT.ML _) *)  -> true
(*    | FT.PL (FT.Web (_)) -> true *)
    | _ -> false
  );
]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let set_gc () =
  if !Flag.debug_gc
  then Gc.set { (Gc.get()) with Gc.verbose = 0x01F };
  (* only relevant in bytecode, in native the stacklimit is the os stacklimit *)
  Gc.set {(Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024};
  (* see www.elehack.net/michael/blog/2010/06/ocaml-memory-tuning *)
  Gc.set { (Gc.get()) with Gc.minor_heap_size = 4_000_000 };
  Gc.set { (Gc.get()) with Gc.major_heap_increment = 8_000_000 };
  Gc.set { (Gc.get()) with Gc.space_overhead = 300 };
  ()

(*****************************************************************************)
(* Model helpers *)
(*****************************************************************************)

(*s: treemap_generator *)
(* this is called each time we go in a new directory (or set of dirs) *)
let treemap_generator ~filter_file = 
 fun paths ->
  let treemap = Treemap_pl.code_treemap ~filter_file paths in
  let algo = Treemap.Ordered Treemap.PivotByMiddle in
  let big_borders = !Flag.boost_label_size in
  let rects = Treemap.render_treemap ~algo ~big_borders treemap in
  Common.pr2 (spf "%d rectangles to draw" (List.length rects));
  rects
(*e: treemap_generator *)

(*s: build_model *)
(* this is currently called in the background *)
let build_model2 root dbfile_opt graphfile_opt =   

  let db_opt = dbfile_opt +> Common.map_opt Database_code.load_database in
  (* todo: and skip_list?
   * less: opti by factorizing the 'find' with treemap_generator?
  *)
  let files = 
    Common.files_of_dir_or_files_no_vcs_nofilter [root] +> List.filter !filter
  in

  let hentities = Model_database_code.hentities root db_opt in
  let all_entities = Model_database_code.all_entities ~root files db_opt in
  let big_grep_idx = Completion2.build_completion_defs_index all_entities in

  let g_opt = graphfile_opt +> Common.map_opt Graph_code.load in

  let hfile_deps_of_node, hentities_of_file =
    match g_opt with
    | None -> Hashtbl.create 0, Hashtbl.create 0
    | Some g ->
      let a = Model_graph_code.build_filedeps_of_dir_or_file g in
      let b = Model_graph_code.build_entities_of_file g in
      let b = Model_graph_code.add_headers_files_entities_of_file root b in
      a, Common.hash_of_list b
  in
  
  let model = { Model.
        root = root;
        db = db_opt;
        hentities; big_grep_idx;
        g =  g_opt;
        hfile_deps_of_node; hentities_of_file;
  }
  in
  model

let build_model a b c = 
  Common.profile_code "View.build_model" (fun () ->
    build_model2 a b c)
(*e: build_model *)

(* could also to parse all json files and filter the one which do not parse *)
let layers_in_dir dir =
  Common2.readdir_to_file_list dir +> Common.map_filter (fun file ->
    if file =~ "layer.*json"
    then Some (Filename.concat dir file)
    else None
  )

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

(*s: main_action() *)
let main_action xs = 
  set_gc ();
  Logger.log Config_pfff.logger "codemap" None;

  (* this used to be done by linking with gtkInit.cmo, but better like this *)
  let _locale = GtkMain.Main.init () in
  pr2 (spf "Using Cairo version: %s" Cairo.compile_time_version_string);

  let root = Common2.common_prefix_of_files_or_dirs xs in
  pr2 (spf "Using root = %s" root);

  let async_model = Async.async_make () in

  let layers = 
    match !layer_file, !layer_dir, xs with
    | Some file, _, _ -> 
        [Layer_code.load_layer file]
    | None, Some dir, _ | None, None, [dir] ->
        layers_in_dir dir +> List.map Layer_code.load_layer
    | _ -> []
  in
  let layers_with_index = 
    Layer_code.build_index_of_layers ~root 
      (match !layer_file, layers with 
      | Some _, [layer] -> [layer, true]
      | _ -> layers +> List.map (fun x -> x, false)
      )
  in

  let db_file = 
    match !db_file, xs with
    | Some file, _ -> Some file
    | None, [dir] ->
      let candidates = [
          Filename.concat dir Database_code.default_db_name;
          Filename.concat dir Database_code.default_db_name ^ ".json";
      ] in
      (try 
        Some (candidates +> List.find (fun file -> Sys.file_exists file))
      with Not_found -> None
      )
      | _ -> None
  in
  db_file +> Common.do_option (fun db -> 
    pr2 (spf "Using pfff light db: %s" db)
  );
  let graph_file = 
    match !graph_file, xs with
    | Some file, _ -> Some file
    | None, [dir] ->
      let candidates = [
          Filename.concat dir Graph_code.default_filename;
      ] in
      (try 
        Some (candidates +> List.find (fun file -> Sys.file_exists file))
      with Not_found -> None
      )
    | _ -> None
  in
  graph_file +> Common.do_option (fun db -> 
    pr2 (spf "Using graphcode: %s" db)
  );
  let skip_file = !skip_file ||| Filename.concat root "skip_list.txt" in
  let skip_list =
    if Sys.file_exists skip_file
    then begin
      pr2 (spf "Using skip file: %s" skip_file);
      Skip_code.load skip_file
    end
    else []
  in
  let filter_files_skip_list = Skip_code.filter_files skip_list root in
  let filter_file = (fun file -> 
    !filter file && 
    (skip_list = [] || filter_files_skip_list [file] <> []))
  in

  let treemap_func = treemap_generator ~filter_file in
  let dw = Model.init_drawing  treemap_func layers_with_index xs root in

  (* This can require lots of stack. Make sure to have ulimit -s 40000.
   * This thread also cause some Bus error on MacOS :(
   * so have to use Timeout instead when on the Mac
   *)
  (if Cairo_helpers.is_old_cairo() 
  then
    Thread.create (fun () ->
      Async.async_set (build_model root db_file graph_file) async_model;
    ) ()
    +> ignore
   else 
    Async.async_set (build_model root db_file graph_file) async_model;
   (*
    GMain.Timeout.add ~ms:2000 ~callback:(fun () ->
      Model.async_set (build_model root dbfile_opt) model;
      false
    ) +> ignore
   *)
  );

  let w = { Model.
    dw;
    dw_stack = ref [dw];
    model = async_model;
    treemap_func;
    current_node = None;
    settings = { Model.
      (* todo: too fuzzy for now *)
      draw_summary = false;
      draw_searched_rectangles = true;
    };
  }
  in

  View2.mk_gui  ~screen_size:!screen_size ~legend:!legend !test_mode w
(*e: main_action() *)
  
(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

(* related work: http://cloc.sourceforge.net/ but have skip list
 * and archi_code_lexer.mll which lower the important of some files?
 *)
let test_loc root =
  let root = Common.realpath root in
  let skip_file = !skip_file ||| Filename.concat root "skip_list.txt" in
  let skip_list =
    if Sys.file_exists skip_file
    then begin
      pr2 (spf "Using skip file: %s" skip_file);
      Skip_code.load skip_file
    end
    else []
  in
  let filter_files_skip_list = Skip_code.filter_files skip_list root in
  let filter_file = (fun file -> 
    !filter file && (skip_list = [] || filter_files_skip_list [file] <> []))
  in
  let treemap = Treemap_pl.code_treemap ~filter_file [root] in

  let res = ref [] in
  let rec aux tree =
    match tree with
    | Common2.Node (_dir, xs) ->
        List.iter aux xs
    | Common2.Leaf (leaf, _) ->
        let file = leaf.Treemap.label in
        let size = leaf.Treemap.size in
        let unix_size = (Common2.unix_stat_eff file).Unix.st_size in
        if unix_size > 0
        then begin
          let multiplier = (float_of_int size /. float_of_int unix_size) in
          let multiplier = min multiplier 1.0 in
          let loc = Common2.nblines_with_wc file in
          Common.push ((Common.readable ~root file), 
                       (float_of_int loc *. multiplier)) res;
        end
  in
  aux treemap;
  let total = !res +> List.map snd +> List.map int_of_float  +> Common2.sum in
  pr2 (spf "LOC = %d (%d files)" total (List.length !res));
  let topx = 30 in
  pr2 (spf "Top %d:" topx);
  !res +> Common.sort_by_val_highfirst +> Common.take_safe topx 
  +>  List.iter (fun (file, f) ->
      pr2 (spf "%-40s: %d" file (int_of_float f))
  )
  

let test_treemap_dirs () =
  let paths = 
    ["commons/common.ml"; "h_visualization"; "code_graph"] 
    +> List.map Common.realpath in
  let paths = List.sort String.compare paths in
  let tree = 
    paths +> Treemap.tree_of_dirs_or_files
      ~filter_dir:Lib_vcs.filter_vcs_dir
      ~filter_file:(fun file -> file =~ ".*\\.ml")
      ~file_hook:(fun _file -> 10)
  in
  pr2_gen tree


(* update: try to put ocamlgtk related tests in widgets/test_widgets.ml, not
 * here. Here it's for ... well it's for nothing I think because it's not 
 * really easy to test a gui.
 *)

(*s: visual_commitid() action *)
let test_visual_commitid id =
  let files = Common.cmd_to_list
    (spf "git show --pretty=\"format:\" --name-only %s"
        id) 
    (* not sure why git adds an extra empty line at the beginning but we
     * have to filter it
     *)
    +> Common.exclude Common.null_string
  in
  pr2_gen files;
  main_action files
(*e: visual_commitid() action *)

let width = 500
let height = 500

let test_draw cr =
  (* [0,0][1,1] world scaled to a width x height screen *)
  Cairo.scale cr (float_of_int width) (float_of_int height);

  Cairo.set_source_rgba cr ~red:0.5 ~green:0.5 ~blue:0.5 ~alpha:0.5;
  Cairo.set_line_width cr 0.001;

  Cairo.move_to cr 0.5 0.5;
  Cairo.line_to cr 0.6 0.6;
  Cairo.stroke cr;

  Cairo.select_font_face cr "serif"
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_BOLD;
  Cairo.set_font_size cr 0.1;
  Cairo.move_to cr 0.1 0.1;
  Cairo.show_text cr "THIS IS SOME TEXT";
  Cairo.move_to cr 0.1 0.2;
  Cairo.show_text cr "THIS IS SOME TEXT";
  Cairo.set_font_size cr 0.05;
  Cairo.move_to cr 0.1 0.3;
  Cairo.show_text cr "THIS IS SOME TEXT";

  Cairo.set_source_rgb cr ~red:0.1 ~green:0.1 ~blue:0.1;
  Cairo.move_to cr 0.1 0.1;
  Cairo.line_to cr 0.1 0.2;
  Cairo.stroke cr;

  let start = ref 0.0 in

  for _i = 0 to 3 do
    let end_ = !start +. 0.5 in
    Cairo.arc cr ~xc:0.5 ~yc:0.5 ~radius:0.3 ~angle1:!start
      ~angle2:end_;
    Cairo.stroke cr;
    start := end_;
  done;

  ()

let test_cairo () =
  let _locale = GtkMain.Main.init () in
  let w = GWindow.window ~title:"test" () in
  (w#connect#destroy GMain.quit) +> ignore;
  let px = GDraw.pixmap ~width ~height ~window:w () in
  px#set_foreground `WHITE;
  px#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  let cr = Cairo_lablgtk.create px#pixmap in
  test_draw cr;
  (GMisc.pixmap px ~packing:w#add ()) +> ignore;
  w#show ();
  GMain.main()
  
(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let extra_actions () = [
 (*s: actions *)
   "-test_loc", " ",
   Common.mk_action_1_arg (test_loc);
   "-test_cairo", " ",
   Common.mk_action_0_arg (test_cairo);
   "-test_commitid", " <id>",
   Common.mk_action_1_arg (test_visual_commitid);
   "-test_treemap_dirs", " <id>",
   Common.mk_action_0_arg (test_treemap_dirs);
 (*e: actions *)
]
 
(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 extra_actions()@
 []

let options () = [ 
  (*s: options *)
    "-screen_size", Arg.Set_int screen_size,
    " <int> (1 = small, 2 = big)";
    "-ss", Arg.Set_int screen_size,
    " <int> alias for -screen_size";
    "-no_legend", Arg.Clear legend,
    " do not display the legend";

    "-symlinks", Arg.Unit (fun () -> Treemap.follow_symlinks := true;),
    " follow symlinks";
    "-no_symlinks", Arg.Unit (fun () -> Treemap.follow_symlinks := false),
    " do not follow symlinks";

    "-with_graph", Arg.String (fun s -> graph_file := Some s),
    " <graph_file> dependency semantic information";
    "-with_db", Arg.String (fun s -> db_file := Some s),
    " <db_file> generic semantic information";
    "-with_layer", Arg.String (fun s -> layer_file := Some s),
    " <layer_file>";
    "-with_layers", Arg.String (fun s -> layer_dir := Some s),
    " <dir_with_layers>";

    "-filter", Arg.String (fun s -> filter := List.assoc s filters;), 
     spf " filter certain files (available = %s)" 
      (filters +> List.map fst +> Common.join ", ");
    "-extra_filter", Arg.String (fun s -> Flag.extra_filter := Some s),
    " ";
    "-skip_list", Arg.String (fun s -> skip_file := Some s), 
    " <file> skip files or directories";

    "-with_info", Arg.String (fun _s -> ()),
    " obsolete\n"; (* for codemap_www in engshare/admin/scripts *)

    "-ft", Arg.Set_float Flag.threshold_draw_content_font_size_real,
    " <float> threshold to draw content";
    "-boost_lbl", Arg.Set Flag.boost_label_size,
    " boost size of labels";
    "-no_boost_lbl", Arg.Clear Flag.boost_label_size,
    " do not boost labels\n";

  (*-------------------------------------------------------------------------*)
  (* debugging helpers *)
  (*-------------------------------------------------------------------------*)

    "-test", Arg.String (fun s -> test_mode := Some s),
    " <str> execute an internal script";

    "-verbose", Arg.Set Flag.verbose_visual,
    " ";
    "-debug_gc", Arg.Set Flag.debug_gc,
    " ";
    "-debug_handlers", Arg.Set Gui.synchronous_actions,
    " ";
    (* "-disable_ancient", Arg.Clear Flag.use_ancient, " "; *)
    "-disable_fonts", Arg.Set Flag.disable_fonts,
    " ";
  (*e: options *)
  ] @
  Common.options_of_actions action (all_actions()) @
  Common2.cmdline_flags_devel () @
  [
  "-version",   Arg.Unit (fun () -> 
    pr2 (spf "CodeMap version: %s" Config_pfff.version);
    exit 0;
  ), 
    " guess what";
  ]

(*****************************************************************************)
(* The main entry point *)
(*****************************************************************************)
let main () = 

  let usage_msg = 
    spf "Usage: %s [options] <file or dir> \nDoc: %s\nOptions:"
      (Filename.basename Sys.argv.(0))
      "https://github.com/facebook/pfff/wiki/Codemap"
  in
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
    | (x::xs) -> 
        main_action (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> Arg.usage (Arg.align (options())) usage_msg; 
    );
  )

(*****************************************************************************)
let _ = 
  Common.main_boilerplate (fun () ->
    main ()
  )
(*e: main_codemap.ml *)
