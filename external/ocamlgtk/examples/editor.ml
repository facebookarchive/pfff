(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: editor.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

(* Uses the deprecated old text widget. See editor2.ml for the new one *)

open StdLabels
open GMain

let file_dialog ~title ~callback ?filename () =
  let sel =
    GWindow.file_selection ~title ~modal:true ?filename () in
  sel#cancel_button#connect#clicked ~callback:sel#destroy;
  sel#ok_button#connect#clicked ~callback:
    begin fun () ->
      let name = sel#filename in
      sel#destroy ();
      callback name
    end;
  sel#show ()

class editor ?packing ?show () = object (self)
  val text = GBroken.text ~editable:true ?packing ?show ()
  val mutable filename = None

  method text = text

  method load_file name =
    try
      let ic = open_in name in
      filename <- Some name;
      text#freeze ();
      text#delete_text ~start:0 ~stop:text#length;
      let buf = String.create 1024 and len = ref 0 in
      while len := input ic buf 0 1024; !len > 0 do
	if !len = 1024 then text#insert buf
	else text#insert (String.sub buf ~pos:0 ~len:!len)
      done;
      text#set_point 0;
      text#thaw ();
      close_in ic
    with _ -> ()

  method open_file () = file_dialog ~title:"Open" ~callback:self#load_file ()

  method save_dialog () =
    file_dialog ~title:"Save" ?filename
      ~callback:(fun file -> self#output ~file) ()

  method save_file () =
    match filename with
      Some file -> self#output ~file
    | None -> self#save_dialog ()

  method output ~file =
    try
      if Sys.file_exists file then Sys.rename file (file ^ "~");
      let oc = open_out file in
      output_string oc (text#get_chars ~start:0 ~stop:text#length);
      close_out oc;
      filename <- Some file
    with _ -> prerr_endline "Save failed"
end

let window = GWindow.window ~width:500 ~height:300 ~title:"editor" ()
let vbox = GPack.vbox ~packing:window#add ()

let menubar = GMenu.menu_bar ~packing:vbox#pack ()
let factory = new GMenu.factory menubar
let accel_group = factory#accel_group
let file_menu = factory#add_submenu "File"
let edit_menu = factory#add_submenu "Edit"

let hbox = GPack.hbox ~packing:vbox#add ()
let editor = new editor ~packing:hbox#add ()
let scrollbar = GRange.scrollbar `VERTICAL ~packing:hbox#pack ()

open GdkKeysyms

let _ =
  window#connect#destroy ~callback:Main.quit;
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Open..." ~key:_O ~callback:editor#open_file;
  factory#add_item "Save" ~key:_S ~callback:editor#save_file;
  factory#add_item "Save as..." ~callback:editor#save_dialog;
  factory#add_separator ();
  factory#add_item "Quit" ~key:_Q ~callback:window#destroy;
  let factory = new GMenu.factory edit_menu ~accel_group in
  factory#add_item "Copy" ~key:_C ~callback:editor#text#copy_clipboard;
  factory#add_item "Cut" ~key:_X ~callback:editor#text#cut_clipboard;
  factory#add_item "Paste" ~key:_V ~callback:editor#text#paste_clipboard;
  factory#add_separator ();
  factory#add_check_item "Word wrap" ~active:false
    ~callback:editor#text#set_word_wrap;
  factory#add_check_item "Read only" ~active:false
    ~callback:(fun b -> editor#text#set_editable (not b));
  window#add_accel_group accel_group;
  editor#text#event#connect#button_press
    ~callback:(fun ev ->
      let button = GdkEvent.Button.button ev in
      if button = 3 then begin
	file_menu#popup ~button ~time:(GdkEvent.Button.time ev); true
      end else false);
  editor#text#set_vadjustment scrollbar#adjustment;
  window#show ();
  Main.main ()
