open Common

module Flag = Flag_program_visual

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(*
*)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* There are a few settable flags in flags_program_visual.ml *)

(* action mode *)
let action = ref ""

let version = "0.1"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)
let main_action dir =
  raise Todo

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

let misc_extra_actions () = [
]


(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  misc_extra_actions () @
  (* 
  Treemap_pl.actions () ++
  Treemap_ex_pfff.actions () ++
  *)
 []

let options () = 
  Flag.cmdline_flags () @
  [
  ] @
  Common.options_of_actions action (all_actions()) @
  Common.cmdline_flags_devel () @
  Common.cmdline_flags_verbose () @
  Common.cmdline_flags_other () @
  [
  "-version",   Arg.Unit (fun () -> 
    pr2 (spf "visual pl version: %s" version);
    exit 0;
  ), 
    "  guess what";

  (* this can not be factorized in Common *)
  "-date",   Arg.Unit (fun () -> 
    pr2 "version: $Date: 2008/10/26 00:44:57 $";
    raise (Common.UnixExit 0)
    ), 
  "   guess what";
  ] @
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
  let usage_msg = 
    "Usage: " ^ Common.basename Sys.argv.(0) ^ 
      " [options] <db> " ^ "\n" ^ "Options are:"
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
    | [x] -> 
        main_action x

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | _ -> 
        Common.usage usage_msg (options()); 
        failwith "too few arguments"
    )
  )


(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
      main ();
  )

