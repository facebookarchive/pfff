(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: fixpoint.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open GMain

let rec fix ~f ~eq x =
  let x' = f x in
  if eq x x' then x
  else fix ~f ~eq x'

let eq_float x y = abs_float (x -. y) < 1e-13

let _ =
  let top = GWindow.window () in
  top#connect#destroy ~callback:Main.quit;
  let vbox = GPack.vbox ~packing: top#add () in
  let entry = GEdit.entry ~max_length: 20 ~packing: vbox#add () in
  let tips = GData.tooltips () in
  tips#set_tip entry#coerce ~text:"Initial value for fix-point";
  let result =
    GEdit.entry ~max_length: 20 ~editable: false ~packing: vbox#add () in

  entry#connect#activate ~callback:
    begin fun () ->
      let x = try float_of_string entry#text with _ -> 0.0 in
      entry#set_text (string_of_float (cos x));
      let res = fix ~f:cos ~eq:eq_float x in
      result#set_text (string_of_float res)
    end;
  top#show ();
  Main.main ()
