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
 * Main entry point of codemap, a semantic source code visualizer
 * using treemaps and code thumbnails.
 * 
 * requirements:
 *  - get a bird's eye view of all the code (hence treemaps)
 *  - get a bird's eye view of a file (hence code thumbnails)
 *  - better syntax highlighting than Emacs, using real parsers so
 *    we can colorize differently identifiers (a function vs a field vs
 *    a constant etc)
 *  - important code should be bigger. Just like in google maps
 *    the important roads are more visible. So need some sort of
 *    global analysis.
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
 * 
 * related work:
 *  - TODO http://www.kickstarter.com/projects/296054304/zeta-code
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

let db_file    = ref (None: Common.filename option)
let layer_file = ref (None: Common.filename option)
let layer_dir  = ref (None: Common.dirname option)

(* See also Gui.synchronous_actions *)
let test_mode = ref (None: string option)
(*e: main flags *)

let filter = ref Treemap_pl.ex_filter_file

(* todo? config file ? 
 * GtkMain.Rc.add_default_file "/home/pad/c-pfff/data/pfff_browser.rc"; 
 *)

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Shortcuts *)
(*****************************************************************************)

let filters = [
  "ocaml", Treemap_pl.ocaml_filter_file;
  "mli", Treemap_pl.ocaml_mli_filter_file;
  "php", Treemap_pl.php_filter_file;
  "nw", (fun file -> 
    match FT.file_type_of_file file with
    | FT.Text "nw" -> true | _ -> false
  );
  "pfff", (fun file ->
    match FT.file_type_of_file file with
    | FT.PL (
       (FT.ML _) | FT.Makefile | FT.Opa | FT.Prolog _ | FT.Web (FT.Php _))
      -> 
        (* todo: should be done in file_type_of_file *)
        not (FT.is_syncweb_obj_file file)
        && not ( 
                (* file =~ ".*commons/" || *)
                (* file =~ ".*external/" || *)
                file =~ ".*_build/")
    | _ -> false
  );
  "cpp", (let x = ref false in (fun file ->
    Common2.once x (fun () -> Parse_cpp.init_defs !Flag_parsing_cpp.macros_h);
    match FT.file_type_of_file file with
    | FT.PL (FT.C _ | FT.Cplusplus _) -> true 
    | _ -> false
  ));
  "opa", (fun file -> 
    match FT.file_type_of_file file with
    | FT.PL (FT.Opa) (* | FT.PL (FT.ML _) *)  -> true
(*    | FT.PL (FT.Web (_)) -> true *)
    | _ -> false
  );
  "rs", (fun file -> 
    match FT.file_type_of_file file with
    | FT.PL (FT.Rust) -> true
    | _ -> false
  );
]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let set_gc () =
  if !Flag.debug_gc
  then Gc.set { (Gc.get()) with Gc.verbose = 0x01F };
  (* see http://www.elehack.net/michael/blog/2010/06/ocaml-memory-tuning *)
  Gc.set { (Gc.get()) with Gc.minor_heap_size = 2_000_000 };
  Gc.set { (Gc.get()) with Gc.space_overhead = 200 };
  ()

(*****************************************************************************)
(* Model helpers *)
(*****************************************************************************)

(*s: treemap_generator *)
let treemap_generator paths = 
  let treemap = Treemap_pl.code_treemap ~filter_file:!filter paths in
  let algo = Treemap.Ordered Treemap.PivotByMiddle in
  let rects = Treemap.render_treemap_algo 
    ~algo ~big_borders:!Flag.boost_label_size
    treemap in
  Common.pr2 (spf "%d rectangles to draw" (List.length rects));
  rects
(*e: treemap_generator *)

(*s: build_model *)
let build_model2 root dbfile_opt =   
  let db_opt = Common2.fmap Database_code.load_database dbfile_opt in
  let hentities = Model.hentities root db_opt in
  let hfiles_entities = Model.hfiles_and_top_entities root db_opt in
  let all_entities = Model.all_entities db_opt root in
  let idx = Completion2.build_completion_defs_index all_entities in
  
  let model = { Model.
        db = db_opt;
        hentities = hentities;
        hfiles_entities = hfiles_entities;
        big_grep_idx = idx;
  }
  in
  (*
    let model = Ancient2.mark model in
    Gc.compact ();
  *)
(*
  (* sanity check *)
  let hentities = (Ancient2.follow model).Model.hentities in
  let n = Hashtbl.length hentities in
  pr2 (spf "before = %d" n);
  let cnt = ref 0 in
  Hashtbl.iter (fun k v -> pr2 k; incr cnt) hentities;
  pr2 (spf "after = %d" !cnt);
  (* let _x = Hashtbl.find hentities "kill" in *)
*)
  model

let build_model a b = 
  Common.profile_code "View.build_model" (fun () ->
    build_model2 a b)
(*e: build_model *)

(* could also try to parse all json files and filter the one which do
 * not parse *)
let layers_in_dir dir =
  Common2.readdir_to_file_list dir +> Common.map_filter (fun file ->
    if file =~ "layer.*marshall"
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

  let root = Common2.common_prefix_of_files_or_dirs xs in
  pr2 (spf "Using root = %s" root);

  let model = Async.async_make () in

  let layers = 
    match !layer_file, !layer_dir, xs with
    | Some file, _, _ -> 
        [Layer_code.load_layer file]
    | None, Some dir, _ 
    | None, None, [dir] -> 
        layers_in_dir dir +> List.map Layer_code.load_layer
    | _ -> []
  in
  let layers_with_index = 
    Layer_code.build_index_of_layers ~root 
      (match layers with 
      (* not active by default ? it causes some problems  *)
      | [layer] -> [layer, false]
      | _ -> layers +> List.map (fun x -> x, false)
      )
  in

  let dw = Model.init_drawing treemap_generator model layers_with_index xs in

  pr2 (spf "Using Cairo version: %s" Cairo.compile_time_version_string);
  let db_file = 
    (* todo: do as for layers, put this logic of marshall vs json elsewhere? *)
    match !db_file, xs with
    | None, [dir] ->
        let db = Filename.concat dir Database_code.default_db_name in
        if Sys.file_exists db 
        then begin 
          pr2 (spf "Using pfff light db: %s" db);
          Some db
        end
        else
         let db = Filename.concat dir Database_code.default_db_name ^ ".json" in
         if Sys.file_exists db 
         then begin 
           pr2 (spf "Using pfff light db: %s" db);
           Some db
         end
         else
           !db_file
    | _ -> !db_file
  in


  (* This can require lots of stack. Make sure to have ulimit -s 40000.
   * This thread also cause some Bus error on MacOS :(
   * so have to use Timeout instead when on the Mac
   *)
  (if Cairo_helpers.is_old_cairo() 
  then
    Thread.create (fun () ->
      Async.async_set (build_model root db_file) model;
    ) ()
    +> ignore
   else 
    Async.async_set (build_model root db_file) model;
   (*
    GMain.Timeout.add ~ms:2000 ~callback:(fun () ->
      Model.async_set (build_model root dbfile_opt) model;
      false
    ) +> ignore
   *)
  );

  (* the GMain.Main.init () is done by linking with gtkInit.cmo *)
  View2.mk_gui 
    ~screen_size:!screen_size
    ~legend:!legend
    !test_mode
    (root, model, dw, db_file)
(*e: main_action() *)
  
(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

(*s: visual_commitid() action *)
let visual_commitid id =
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
  (* [0,0][1,1] world *)
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

  ()

let test_cairo () =
  let w = GWindow.window ~title:"test" () in
  ignore (w#connect#destroy GMain.quit);
  let px = GDraw.pixmap ~width ~height ~window:w () in
  px#set_foreground `WHITE;
  px#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  let cr = Cairo_lablgtk.create px#pixmap in
  test_draw cr;
  
  ignore(GMisc.pixmap px ~packing:w#add ());
  w#show ();
  GMain.main();
  ()
  
(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let extra_actions () = [
 (*s: actions *)
   "-commitid", " <id>",
   Common.mk_action_1_arg (visual_commitid);
   "-test_cairo", " ",
   Common.mk_action_0_arg (test_cairo);
 (*e: actions *)
]
 
(*****************************************************************************)
(* The options *)
(*****************************************************************************)

(* update: try to put ocamlgtk related tests in widgets/test_widgets.ml, not
 * here. Here it's for ... well it's for nothing I think because it's not 
 * really easy to test a gui.
 *)
let all_actions () = 
 extra_actions()++
 []

let options () = [ 
  (*s: options *)
    "-screen_size" , Arg.Set_int screen_size,
    " <int> (1 = small, 2 = big)";
    "-ss" , Arg.Set_int screen_size,
    " alias for -screen_size";
    "-ft", Arg.Set_float Flag.threshold_draw_content_font_size_real,
    " ";
    "-boost_lbl" , Arg.Set Flag.boost_label_size,
    " ";
    "-no_boost_lbl" , Arg.Clear Flag.boost_label_size,
    " ";
    "-no_legend" , Arg.Clear legend,
    " ";

    "-symlinks", Arg.Unit (fun () -> 
      Treemap.follow_symlinks := true;
    ), " ";
    "-no_symlinks", Arg.Unit (fun () ->
      Treemap.follow_symlinks := false;
    ), " ";

    "-with_info", Arg.String (fun s -> db_file := Some s),
    " <db_light_file>";
    "-with_layer", Arg.String (fun s -> layer_file := Some s),
    " <layer_file>";
    "-with_layers", Arg.String (fun s -> layer_dir := Some s),
    " <layer_dir>";

    "-filter", Arg.String (fun s -> filter := List.assoc s filters;), 
     spf " filter certain files (available = %s)" 
      (filters +> List.map fst +> Common.join ", ");

    "-extra_filter", Arg.String (fun s -> Flag.extra_filter := Some s),
    " ";

  (*-------------------------------------------------------------------------*)
  (* debugging helpers *)
  (*-------------------------------------------------------------------------*)

    "-test" , Arg.String (fun s -> test_mode := Some s),
    " <str> execute an internal script";

    "-verbose" , Arg.Set Flag.verbose_visual,
    " ";
    "-debug_gc", Arg.Set Flag.debug_gc,
    " ";
    "-debug_handlers", Arg.Set Gui.synchronous_actions,
    " ";
    (* "-disable_ancient", Arg.Clear Flag.use_ancient, " "; *)
    "-disable_fonts", Arg.Set Flag.disable_fonts,
    " ";
  (*e: options *)
  ] ++
  Common.options_of_actions action (all_actions()) ++
  Common2.cmdline_flags_devel () ++
  Common2.cmdline_flags_verbose () ++
  [
  "-version",   Arg.Unit (fun () -> 
    pr2 (spf "CodeMap version: %s" Config_pfff.version);
    exit 0;
  ), 
    "  guess what";
  ]

(*****************************************************************************)
(* The main entry point *)
(*****************************************************************************)
let main () = 
  Common_extra.set_link ();

  let usage_msg = 
    spf "Usage: %s [options] <file or dir> \nDoc: %s\nOptions:"
      (Common2.basename Sys.argv.(0))
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
    | _ -> Arg.usage (Arg.align (options())) usage_msg; 
    );
  )

(*****************************************************************************)
let _ = 
  Common.main_boilerplate (fun () ->
    main ()
  )
(*e: main_codemap.ml *)
