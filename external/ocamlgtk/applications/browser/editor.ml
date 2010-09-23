(**************************************************************************)
(*     Lablgtk - Applications                                             *)
(*                                                                        *)
(*    * You are free to do anything you want with this code as long       *)
(*      as it is for personal use.                                        *)
(*                                                                        *)
(*    * Redistribution can only be "as is".  Binary distribution          *)
(*      and bug fixes are allowed, but you cannot extensively             *)
(*      modify the code without asking the authors.                       *)
(*                                                                        *)
(*    The authors may choose to remove any of the above                   *)
(*    restrictions on a per request basis.                                *)
(*                                                                        *)
(*    Authors:                                                            *)
(*      Jacques Garrigue <garrigue@kurims.kyoto-u.ac.jp>                  *)
(*      Benjamin Monate  <Benjamin.Monate@free.fr>                        *)
(*      Olivier Andrieu  <oandrieu@nerim.net>                             *)
(*      Jun Furuse       <Jun.Furuse@inria.fr>                            *)
(*      Hubert Fauque    <hubert.fauque@wanadoo.fr>                       *)
(*      Koji Kagawa      <kagawa@eng.kagawa-u.ac.jp>                      *)
(*                                                                        *)
(**************************************************************************)

(* $Id: editor.ml 1352 2007-07-12 08:56:18Z zoggy $ *)

open StdLabels
open GMain


class editor ?packing ?show () = object (self)
  val view = GText.view ?packing ?show ()
  val mutable filename = None

  method view = view

  method buffer = view#buffer

  method load_file name =
    try
      let b = Buffer.create 1024 in
      File.with_file name ~f:(File.input_channel b);
      let s = Glib.Convert.locale_to_utf8 (Buffer.contents b) in
      let n_buff = GText.buffer ~text:s () in
      Lexical.init_tags n_buff;
      Lexical.tag n_buff;
      view#set_buffer n_buff;
      filename <- Some name;
      n_buff#place_cursor n_buff#start_iter
    with exn -> prerr_endline ("Load failed: " ^ Printexc.to_string exn)

  method open_file () = File.dialog ~title:"Open" ~callback:self#load_file ()

  method save_dialog () =
    File.dialog ~title:"Save" ?filename
      ~callback:(fun file -> self#output ~file) ()

  method save_file () =
    match filename with
      Some file -> self#output ~file
    | None -> self#save_dialog ()

  method output ~file =
    try
      if Sys.file_exists file then Sys.rename file (file ^ "~");
      let s = view#buffer#get_text () in
      let oc = open_out file in
      output_string oc (Glib.Convert.locale_from_utf8 s);
      close_out oc;
      filename <- Some file
    with _ -> prerr_endline "Save failed"

  initializer
    Lexical.init_tags view#buffer;
    view#buffer#connect#after#insert_text ~callback:
      begin fun it s ->
        let start = it#backward_chars (String.length s) in
        Lexical.tag view#buffer
          ~start:(start#set_line_index 0) ~stop:it#forward_to_line_end;
      end;
    view#buffer#connect#after#delete_range ~callback:
      begin fun ~start ~stop ->
        let start = start#set_line_index 0
        and stop = start#forward_to_line_end in
        Lexical.tag view#buffer ~start ~stop
      end;
    view#misc#modify_font_by_name "monospace";
    view#misc#set_size_chars ~width:80 ~height:25 ~lang:"C" ();
    ()
end

open GdkKeysyms

class editor_window ?(show=false) () =
  let window = GWindow.window ~title:"Program Editor" () in
  let vbox = GPack.vbox ~packing:window#add () in

  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group
  and file_menu = factory#add_submenu "File"
  and edit_menu = factory#add_submenu "Edit"
  and comp_menu = factory#add_submenu "Compiler" in

  let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~packing:vbox#add () in
  let editor = new editor ~packing:sw#add () in
object (self)
  inherit GObj.widget window#as_widget

  method window = window
  method editor = editor
  method show = window#show

  initializer
    window#connect#destroy ~callback:(fun () -> Gc.full_major(); Main.quit());
    let factory = new GMenu.factory file_menu ~accel_group in
    factory#add_item "Open..." ~key:_O ~callback:editor#open_file;
    factory#add_item "Save..." ~key:_S ~callback:editor#save_file;
    factory#add_item "Shell"
      ~callback:(fun () -> Shell.f ~prog:"ocaml" ~title:"Objective Caml Shell");
    factory#add_separator ();
    factory#add_item "Quit" ~key:_Q ~callback:window#destroy;
    let factory = new GMenu.factory edit_menu ~accel_group in
    factory#add_item "Copy" ~key:_C ~callback:
      (fun () -> editor#buffer#copy_clipboard GMain.clipboard);
    factory#add_item "Cut" ~key:_X ~callback:
      (fun () -> editor#buffer#cut_clipboard GMain.clipboard);
    factory#add_item "Paste" ~key:_V ~callback:
      (fun () -> editor#buffer#paste_clipboard GMain.clipboard);
    factory#add_separator ();
    factory#add_check_item "Word wrap" ~active:false ~callback:
      (fun b -> editor#view#set_wrap_mode (if b then `WORD else `NONE));
    factory#add_check_item "Read only" ~active:false
      ~callback:(fun b -> editor#view#set_editable (not b));
    let factory = new GMenu.factory comp_menu ~accel_group in
    factory#add_item "Lex" ~key:_L
      ~callback:(fun () -> Lexical.tag editor#buffer);
    window#add_accel_group accel_group;
    if show then self#show ();
end

let _ =
  Main.init ();
  if Array.length Sys.argv >= 2 && Sys.argv.(1) = "-shell" then
    Shell.f ~prog:"ocaml" ~title:"Objective Caml Shell"
  else
    ignore (new editor_window ~show:true ());
  Main.main ()
