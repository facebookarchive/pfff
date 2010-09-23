(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

open Printf
open GMain

let pr_targets targets =
  printf "%d targets\n" (List.length targets);
  let pr atom = printf "%s\n" (Gdk.Atom.name atom) in
  List.iter pr targets;
  flush stdout

let get_contents targets =
  let rec loop ls =
    match ls with
    | [] -> []
    | atom::xs ->
        try
	  let content = (atom, clipboard#get_contents ~target:atom) in
	  content :: loop xs
        with _ -> loop xs
  in
  loop targets

let pr_contents cnt_list =
  let pr (atom, sdata) = 
    printf "-----\n";
    printf "  target [%s]\n" sdata#target;
    printf "  typ [%s]\n" sdata#typ;
    printf "  format [%d]\n" sdata#format;
    begin try
      printf "  data length (%d) [%s]\n" (String.length sdata#data) sdata#data;
    with _ -> printf "  data (NULL)\n"
    end;
    flush stdout;
  in
  List.iter pr cnt_list

let get_targets () =
  let targets = clipboard#targets in
  pr_targets targets;
  let contents = get_contents targets in
  pr_contents contents;
  ()

let main () =
  (* Create the toplevel window *)
  let window = GWindow.window ~title:"Clipboard" ~border_width:10 () in
  window#connect#destroy ~callback:GMain.quit;

  let btn = GButton.button ~label:"Get Targets" ~packing:window#add () in
  btn#connect#clicked ~callback:get_targets;

  window#show ();
  GMain.main ()

let _ = Printexc.print main ()
