(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* Compile with 
   ocamlc -o viewer -I ../../src/ lablgtk.cma lablgtksourceview2.cma gtkInit.cmo test2.ml
   Run with 
   CAML_LD_LIBRARY_PATH=../../src ./viewer
*)

open Printf

let lang_mime_type = "text/x-ocaml"
let use_mime_type = false
let font_name = "Monospace 10"

let print_lang lang = prerr_endline (sprintf "language: %s" lang#name)

let print_lang_dirs (language_manager:GSourceView2.source_language_manager) =
  let i = ref 0 in
  prerr_endline "lang_dirs:";
  List.iter
    (fun dir -> incr i; prerr_endline (sprintf "%d: %s" !i dir))
    language_manager#search_path

let win = GWindow.window ~title:"LablGtkSourceView 2 test" ()
let vbox = GPack.vbox ~packing:win#add ()
let hbox = GPack.hbox ~packing:(vbox#pack ~expand: false) ()
let bracket_button = GButton.button ~label:"( ... )" ~packing:hbox#add ()
let scrolled_win = GBin.scrolled_window
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC
    ~packing:vbox#add ()
let source_view =
  GSourceView2.source_view
    ~auto_indent:true
     ~insert_spaces_instead_of_tabs:true ~tab_width:2
    ~show_line_numbers:true
    ~right_margin_position:30 ~show_right_margin:true
    ~smart_home_end:`ALWAYS
    ~packing:scrolled_win#add ~height:500 ~width:900
    ()

let language_manager = GSourceView2.source_language_manager ~default:true

let lang =
  match language_manager#guess_language ~content_type:lang_mime_type () with
    | None -> failwith (sprintf "no language for %s" lang_mime_type)
    | Some lang -> lang

let _ =
  let text =
    let ic = open_in "test.ml" in
    let size = in_channel_length ic in
    let buf = String.create size in
    really_input ic buf 0 size;
    close_in ic;
    buf
  in
  win#set_allow_shrink true;
  source_view#misc#modify_font_by_name font_name;
  print_lang_dirs language_manager;
  print_lang lang;

  (* set a style for bracket matching *)
  source_view#source_buffer#set_highlight_matching_brackets true;
  source_view#set_show_line_marks true;

  source_view#source_buffer#set_language (Some lang);
  source_view#source_buffer#set_highlight_syntax true;
  source_view#source_buffer#set_text text;
  ignore (win#connect#destroy (fun _ -> GMain.quit ()));

  let category = "current" in
  let current_line_bookmark = 
    source_view#source_buffer#create_source_mark 
      ~category
      (source_view#source_buffer#get_iter `START) 
  in
  let pixbuf =  source_view#misc#render_icon ~size:`DIALOG `DIALOG_INFO in
  source_view#set_mark_category_background ~category (Some (GDraw.color (`NAME "light blue")));
  source_view#set_mark_category_pixbuf ~category (Some pixbuf);
  ignore (source_view#source_buffer#connect#mark_set 
	    (fun where mark ->
               if GtkText.Mark.get_name mark = Some "insert"
               then begin 
                 prerr_endline "move_cursor";
                 source_view#source_buffer#move_mark 
                   current_line_bookmark#coerce
                   ~where
                 ;
               end));
  ignore (source_view#connect#undo (fun _ -> prerr_endline "undo")); 
  win#show ();
  GMain.Main.main ()


(*
Local Variables:
compile-command: "ocamlc -o viewer -I ../../src/ lablgtk.cma lablgtksourceview2.cma gtkInit.cmo test2.ml"
End:
*)
