(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)
open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

(* ex:
Apr 10 16:51:10: access: connection for dev469.prn1.facebook.com from 2620:0:1cfe:26:3e07:54ff:fe06:d972 (Mozilla/5.0 (Macintosh; Intel Mac OS X 10.7; rv:19.0) Gecko/20100101 Firefox/19.0): 
*)

let main_action file = 
  let tuples =
    cat file +> List.map (fun s ->
    if s =~ 
  "\\(.*\\): access: connection for [^ ]+ from \\([^ ]+\\) (.*):\\(.*\\)$"
    then 
      let (date, from, path) = Common.matched3 s in
      date, from, path
    else failwith (spf "wrong format: %s" s)
  ) in
  let xxs = tuples +> Common.group_by_mapped_key (fun (a, from, c) ->
    from
  )
  in
  let h = Common.hash_of_list xxs in
  let yys = 
    xxs +> List.map (fun (from, xs) -> from, List.length xs) 
      +> Common.sort_by_val_highfirst
  in
  yys +> List.iter (fun (from, n) ->
    let _info = Hashtbl.find h from in
    pr2 (spf "%s: %d" from n)
  )


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
  [
  "-version",   Arg.Unit (fun () -> 
    pr2 (spf "XXX version: %s" Config_pfff.version);
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
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ 
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

    | [] when !action = "-yyy" -> 
        pr2 "yyy"

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
        failwith "too few or too many arguments"
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
      main ();
  )
