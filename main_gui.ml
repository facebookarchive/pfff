open Common

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let metapath = ref "/tmp/pfff_db"

let path_xpm = ref (Filename.concat Config.path "gui/xpms")

let test_mode = ref (None: string option)
(*
let db_path = ref (Database.database_dir "/home/pad/www")
*)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action dir = 
  (*
    GtkMain.Rc.add_default_file "/home/pad/c-pfff/data/pfff_browser.rc";
  *)
  
  let model = 
    Model.init_model (Common.relative_to_absolute dir) Config.path
  in
  Common.finalize (fun () -> 

    (*GMain.Main.init ();*)
    (* GtkMain.Main.init() +> ignore; *)
    (* will call GMain.Main.main() or GtkThread.Main.main() *)

    View.mk_gui model !path_xpm !test_mode;

    
  ) (fun() -> 
    Model.close_model model
  )
  
 
(*****************************************************************************)
(* The options *)
(*****************************************************************************)
let options () = [ 
  "-path_xpm_pixmaps" , Arg.Set_string path_xpm, 
  " path to pixmaps icons files";

  "-test" , Arg.String (fun s -> test_mode := Some s),
  " <str> execute an internal script";

  ] ++
  Flag_analyze_php.cmdline_flags_verbose () ++
  Common.cmdline_flags_devel () ++
  Common.cmdline_flags_verbose () ++
  []
(*****************************************************************************)
(* The actions *)
(*****************************************************************************)

(* update: try put ocamlgtk related test in widgets/test_widgets.ml, not
 * here. Here it's for ... well it's for nothing I think as it's not really
 * easy to test gui.
 *)

let all_actions () = 
  []

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
    | [dir] -> 
        main_action dir

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
