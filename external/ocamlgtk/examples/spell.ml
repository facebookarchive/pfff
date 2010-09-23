(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

let languages = [ "fr_FR"; "en_US"; "de_DE"; "ja_JP" ]

let report_error view msg =
  let message = "<b><big>GtkSpell error:</big></b>\n" ^ (Glib.Markup.escape_text msg) in
  let dlg = GWindow.message_dialog 
      ~message ~use_markup:true ~message_type:`ERROR ~buttons:GWindow.Buttons.close
      ?parent:(GWindow.toplevel view) ~destroy_with_parent:true () in
  ignore (dlg#run ()) ;
  dlg#destroy ()

let set_lang_cb view lang =
  prerr_endline "GtkSpell.set_language" ;
  try GtkSpell.set_language view lang ; true
  with GtkSpell.Error (_, msg) -> report_error view msg ; false

type button_state = {
    mutable lang_id : int ;
    mutable error   : bool
  }

let build_language_list view packing =
  let (combo, _) as c = GEdit.combo_box_text ~strings:languages ~packing () in
  let state = { lang_id = -1 ; error = false } in
  ignore (combo#connect#changed 
	    (fun () -> 
	      if state.error
	      then state.error <- false
	      else
		if set_lang_cb view (GEdit.text_combo_get_active c)
		then state.lang_id <- combo#active
		else begin
		  state.error <- true ; 
		  combo#set_active state.lang_id
		end)) ;
  c

let attach_cb button view lang_list () =
  try
    if button#active 
    then begin
      prerr_endline "GtkSpell.attach" ;
      GtkSpell.attach ?lang:(GEdit.text_combo_get_active lang_list) view end
    else begin
      prerr_endline "GtkSpell.detach" ;
      GtkSpell.detach view end
  with GtkSpell.Error (_, msg) -> 
    button#set_active false ;
    report_error view msg

let setup packing =
  let box = GPack.vbox ~spacing:5 ~packing () in

  let scroll = GBin.scrolled_window 
      ~hpolicy:`AUTOMATIC
      ~vpolicy:`AUTOMATIC
      ~shadow_type:`IN
      ~packing:(box#pack ~expand:true) () in
  let view = GText.view ~wrap_mode:`WORD ~packing:scroll#add () in

  let hbox = GPack.hbox ~spacing:5 ~packing:box#pack () in
  let attached = GButton.toggle_button ~label:"Attached" ~packing:hbox#pack () in
  let lang_list = build_language_list view (hbox#pack ~from:`END) in
  ignore (attached#connect#toggled (attach_cb attached view lang_list)) ;
  ()


let main =
  let w = GWindow.window 
      ~title:"GtkSpell demo" 
      ~border_width:10
      ~width:400 ~height:300 () in
  ignore (w#connect#destroy GMain.quit) ;
  
  setup w#add ;

  w#show () ;
  GMain.main ()
