(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

let run () =
  let w = GWindow.dialog  ~title:"Go to page" ~modal:true ~position:`CENTER () 
  in
  ignore (GMisc.label ~text:"Page: " ~packing:w#vbox#add ());
  let sb = 
    GEdit.spin_button ~packing:w#vbox#add ~digits:0 ~numeric:true ~wrap:true ()
  in
  sb#adjustment#set_bounds ~lower:0. ~upper:50.0 ~step_incr:1. ();
  sb#set_value 22.;
  sb#connect#wrapped (fun () -> prerr_endline "Wrapped!");
  w#add_button_stock `OK `OK;
  w#add_button_stock `CANCEL `CANCEL;
  w#set_default_response `OK;
  let on_ok () = Format.printf "Ok...@." ; w#destroy () in
  match w#run () with
    | `DELETE_EVENT | `CANCEL -> w#destroy ()
    | `OK -> on_ok ()

let () = run ()
