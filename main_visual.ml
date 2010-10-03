(*s: main_visual.ml *)
(*
 * Please imagine a long and boring gnu-style copyright notice 
 * appearing just here.
 *)
open Common

module Flag = Flag_visual

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(*s: main flags *)
(* See also Gui.synchronous_actions *)
let test_mode = ref (None: string option)
let proto = ref false
let screen_size = ref 2
(*
let db_path = ref (Database.database_dir "/home/pad/www")
*)
let db_file = ref (None: Common.filename option)
(*e: main flags *)

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: treemap_generator *)
let treemap_generator paths = 
  let treemap = Treemap_pl.code_treemap paths in
  let algo = Treemap.Ordered Treemap.PivotByMiddle in
  let rects = Treemap.render_treemap_algo ~algo treemap in
  Common.pr2 (spf "%d rectangles to draw" (List.length rects));
  rects
(*e: treemap_generator *)

(*s: build_model *)
let build_model2 root dbfile_opt =   
  let db_opt = dbfile_opt +> Common.fmap (fun file ->
    if file =~ ".*.json"
    then Database_code.load_database file
    else Common.get_value file
  )
  in
  let hentities = Model2.hentities root db_opt in
  let hfiles_entities = Model2.hfiles_and_top_entities root db_opt in
  let all_entities = Model2.all_entities db_opt in
  let idx = Completion2.build_completion_defs_index all_entities in
  
  let model = { Model2.
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
  let hentities = (Ancient2.follow model).Model2.hentities in
  let n = Hashtbl.length hentities in
  pr2 (spf "before = %d" n);
  let cnt = ref 0 in
  Hashtbl.iter (fun k v -> pr2 k; incr cnt) hentities;
  pr2 (spf "after = %d" !cnt);
  (* let _x = Hashtbl.find hentities "kill" in *)
*)


  model

let build_model a b = 
  Common.profile_code2 "View.build_model" (fun () ->
    build_model2 a b)
(*e: build_model *)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

(*s: main_action() *)
let main_action xs = 
  (*
    GtkMain.Rc.add_default_file "/home/pad/c-pfff/data/pfff_browser.rc";
  *)
  if !Flag.debug_gc
  then Gc.set { (Gc.get()) with Gc.verbose = 0x01F };

  (* see http://www.elehack.net/michael/blog/2010/06/ocaml-memory-tuning *)
  Gc.set { (Gc.get()) with Gc.minor_heap_size = 2_000_000 };
  Gc.set { (Gc.get()) with Gc.space_overhead = 200 };
  
  let model = Model2.async_make () in

  let dw = Model2.init_drawing treemap_generator model xs in


  (* the GMain.Main.init () is done by linking with gtkInit.cmo *)
  pr2 (spf "Using Cairo version: %s" Cairo.compile_time_version_string);
  let db_file = 
    match !db_file, xs with
    | None, [dir] ->
        let db = Filename.concat dir Database_code.default_db_name in
        if Sys.file_exists db 
        then begin 
          pr2 (spf "Using pfff light db: %s" db);
          Some db
        end
        else !db_file
    | _ -> !db_file
  in

  let root = Common.common_prefix_of_files_or_dirs xs in

  (* This can require lots of stack. Make sure to have ulimit -s 40000.
   * This thread also cause some Bus error on MacOS :(
   * so have to use Timeout instead when on the Mac
   *)
  (if Cairo_helpers.is_old_cairo() 
  then
    Thread.create (fun () ->
      Model2.async_set (build_model root db_file) model;
    ) ()
    +> ignore
   else 
    Model2.async_set (build_model root db_file) model;
   (*
    GMain.Timeout.add ~ms:2000 ~callback:(fun () ->
      Model2.async_set (build_model root dbfile_opt) model;
      false
    ) +> ignore
   *)
  );


  Common.finalize (fun () -> 
    View2.mk_gui 
      (model, dw, db_file)
      ~screen_size:!screen_size 
      !test_mode
      xs
  ) (fun() -> 
    ()
  )
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
  
(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let extra_actions () = [
 (*s: actions *)
   "-commitid", " <id>",
   Common.mk_action_1_arg (visual_commitid);
 (*e: actions *)
]
 
(*****************************************************************************)
(* The options *)
(*****************************************************************************)

(* update: try put ocamlgtk related test in widgets/test_widgets.ml, not
 * here. Here it's for ... well it's for nothing I think as it's not really
 * easy to test gui.
 *)
let all_actions () = 
 extra_actions()++
 []

let options () = [ 
  (*s: options *)
    "-screen_size" , Arg.Set_int screen_size,
    " <int> 1 = small, 2 = big";
    "-ss" , Arg.Set_int screen_size,
    " alias for -screen_size";
    "-ft", Arg.Set_float Flag.threshold_draw_content_font_size_real,
    " ";

    "-filter", Arg.String (fun s -> Flag.extra_filter := Some s),
    " ";
    "-with_info", Arg.String (fun s -> db_file := Some s),
    " ";

    "-test" , Arg.String (fun s -> test_mode := Some s),
    " <str> execute an internal script";
    "-proto" , Arg.Set proto,
    " ";

    "-verbose" , Arg.Set Flag.verbose_visual,
    " ";
    "-debug_gc", Arg.Set Flag.debug_gc,
    " ";
    "-debug_handlers", Arg.Set Gui.synchronous_actions,
    " ";
   "-disable_ancient", Arg.Clear Flag.use_ancient,
    " ";
   "-enable_ancient", Arg.Set Flag.use_ancient,
    " ";
   "-disable_fonts", Arg.Set Flag.disable_fonts,
    " ";
  (*e: options *)
  ] ++
  Common.options_of_actions action (all_actions()) ++
  Flag_analyze_php.cmdline_flags_verbose () ++
  Common.cmdline_flags_devel () ++
  Common.cmdline_flags_verbose () ++
  [
  "-version",   Arg.Unit (fun () -> 
    pr2 (spf "pfff_visual version: %s" Config.version);
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
    ("Usage: " ^ basename Sys.argv.(0) ^ " [options] <path> \nOptions are:") 
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
  if Sys.argv +> Array.to_list +> List.exists (fun x -> x ="-debugger")
  then Common.debugger := true;

  Common.finalize
    (fun ()-> 
      main ()
    )
    (fun()-> 
      pr2 (Common.profile_diagnostic ());
      Common.erase_temp_files ();
    )

(*e: main_visual.ml *)
