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
(*
 * ./configure -visual
 * 
 * port install gtk2
 * port install cairo
 * port install freetype
 * port install mysql5-devel
 * 
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* See also Gui.synchronous_actions *)

let test_mode = ref (None: string option)
let proto = ref false
let screen_size = ref 2
(*
let db_path = ref (Database.database_dir "/home/pad/www")
*)
let db_file = ref (None: Common.filename option)

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs = 
  (*
    GtkMain.Rc.add_default_file "/home/pad/c-pfff/data/pfff_browser.rc";
  *)
  if !Flag.debug_gc
  then Gc.set { (Gc.get()) with Gc.verbose = 0x01F };

  (* see http://www.elehack.net/michael/blog/2010/06/ocaml-memory-tuning *)
  Gc.set { (Gc.get()) with Gc.minor_heap_size = 2_000_000 };
  Gc.set { (Gc.get()) with Gc.space_overhead = 200 };
  
  let model = () in

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

  Common.finalize (fun () -> 
    View2.mk_gui 
      model 
      db_file
      ~screen_size:!screen_size 
      !test_mode
      xs
  ) (fun() -> 
    ()
  )
  
(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

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
  
(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let extra_actions () = [
  "-commitid", " <id>",
  Common.mk_action_1_arg (visual_commitid);
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
