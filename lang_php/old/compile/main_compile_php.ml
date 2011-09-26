
open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(*
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* TODO look php --help for a list of options accepted by the 
 * Zend PHP interpreter
 *)


let verbose = ref false

let version = "0.1"

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs = 
  match xs with
  | [] -> raise Impossible
  | [file] -> 

      Interpret_php.interpret_file file
  | _ -> failwith "TODO: multiple files"

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)



(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 []

let options () = 
  [
    "-verbose", Arg.Set verbose, 
    " ";
  ] ++
  Common.options_of_actions action (all_actions()) ++
  Common.cmdline_flags_devel () ++
  Common.cmdline_flags_verbose () ++
  Common.cmdline_flags_other () ++
  [
  "-version",   Arg.Unit (fun () -> 
    pr2 (spf "phpcc version: %s" version);
    exit 0;
  ), 
    "  guess what";

  (* this can not be factorized in Common *)
  "-date",   Arg.Unit (fun () -> 
    pr2 "version: $Date: 2008/10/26 00:44:57 $";
    raise (Common.UnixExit 0)
    ), 
  "   guess what";
  ] ++
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
  let usage_msg = 
    "Usage: " ^ Common.basename Sys.argv.(0) ^ 
      " [options] <file or dir> " ^ "\n" ^ "Options are:"
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
        Common.usage usage_msg (options()); 
        failwith "too few arguments"
    )
  )



(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
      main ();
  )

