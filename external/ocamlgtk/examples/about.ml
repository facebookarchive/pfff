(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* ocamlc -g -I ../src lablgtk.cma about.ml -o about *)

let show () =
  let dialog = 
    GWindow.about_dialog 
      ~name:"Name" 
      ~authors:["Me" ; 
                "Myself"; 
               ]
      ~copyright:"Copyright: copyleft"
      ~license:"Open"
      ~website:"http://www.world.com"
      ~website_label:"Questions and support"
      ~version:"0.0"
      ()
  in
  ignore (dialog#connect#response 
            ~callback:(fun _ -> dialog#show ()
                       ));

  ignore (dialog#run ())


let () = 
  GMain.Main.init ();
  show ()
    
