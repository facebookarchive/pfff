(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: glade_demo.ml 1462 2009-05-21 01:24:34Z garrigue $ *)

(* An experiment on using libglade in lablgtk *)

(* lablgladecc2 project1.glade > project1.ml *)
#use "project1.ml";;

class editor () =
  object (self)
    inherit window1 ()

    method open_file () =
      let fs = GWindow.file_selection ~title:"Open file" ~modal:true () in
      fs#cancel_button#connect#clicked ~callback:fs#destroy;
      fs#ok_button#connect#clicked ~callback:
        begin fun () ->
          self#textview1#buffer#set_text "";
          fs#destroy ()
        end;
      fs#show ()

    initializer
      self#bind ~name:"on_open1_activate" ~callback:self#open_file;
      self#bind ~name:"on_about1_activate" 
	~callback:
	(fun () -> prerr_endline "XXX")
  end

let main () =
  let editor = new editor () in
  (* show bindings *)
  Glade.print_bindings stdout editor#xml;
  editor#window1#connect#destroy ~callback:GMain.quit;
  GMain.main ()

let _ = main ()
