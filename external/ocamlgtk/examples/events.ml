(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: events.ml 1406 2008-05-07 10:06:14Z ben_99_9 $ *)

(* This is a direct translation to Gtk2.
   This is actually meaningless, as the new text widget lets you
   obtain an iterator from coordinates, but this just demonstrates
   the use of [#event#send]. *)
(* Old comment by Benjamin:
   I cannot translate this program directly to Gtk 2. The event generation
   causes segfault and starts some drag-n-drop op. 
   The default signal for left button has probably changed.*)
(* I don't see segfaults, just Gtk-criticals. Seems the default handler
   for button 3 is still called, and I see no way to disable that.
   But this is not really relevant to [#event#send]. *)

let string_of_event x = 
 match GdkEvent.get_type x with 
  | `NOTHING -> "nothing"
  | `DELETE -> "delete"
  | `DESTROY -> "destroy"
  | `EXPOSE -> "expose"
  | `MOTION_NOTIFY -> "motion-notify"
  | `BUTTON_PRESS -> "button-press"
  | `TWO_BUTTON_PRESS -> "2 button-press"
  | `THREE_BUTTON_PRESS -> "3 button-press"
  | `BUTTON_RELEASE -> "button-release"
  | `KEY_PRESS -> "key-press"
  | `KEY_RELEASE  -> "key-release"
  | `ENTER_NOTIFY  -> "enter-notfiy"
  | `LEAVE_NOTIFY -> "leave-notify"
  | `FOCUS_CHANGE  -> "focus-change"
  | `CONFIGURE -> "configure"
  | `MAP -> "map"
  | `UNMAP -> "unmap"
  | `PROPERTY_NOTIFY -> "property-notify"
  | `SELECTION_CLEAR -> "selection-clear"
  | `SELECTION_REQUEST -> "selection-request"
  | `SELECTION_NOTIFY -> "selection-notify"
  | `PROXIMITY_IN -> "proximity-in"
  | `PROXIMITY_OUT -> "proximiy-out"
  | `DRAG_ENTER -> "drag-enter"
  | `DRAG_LEAVE -> "drag-leave"
  | `DRAG_MOTION -> "drag-motion"
  | `DRAG_STATUS -> "drag-status"
  | `DROP_START -> "drop-start"
  | `DROP_FINISHED -> "drop-finish"
  | `CLIENT_EVENT -> "client-event"
  | `VISIBILITY_NOTIFY -> "visibility-notify"
  | `NO_EXPOSE-> "no-expose" 
  | `SCROLL -> "scroll"
  | `WINDOW_STATE -> "window-state"
  | `SETTING -> "setting"

let _ =
  let window = GWindow.window ~width:200 ~height:200 () in
  window#connect#destroy ~callback:GMain.quit ;
  window#event#add [`ALL_EVENTS];
  window#event#connect#any 
   (fun x -> 
	prerr_string "before "; 
	prerr_endline (string_of_event x);
	false);
  window#event#connect#after#any 
   (fun x -> 
	prerr_string "after "; 
	prerr_endline (string_of_event x);
	false);
  window#event#connect#configure 
   (fun x -> 
	prerr_string "BEFORE CONFIGURE "; 
	prerr_endline (string_of_event x);
	false);  
  window#event#connect#after#configure 
   (fun x -> 
	prerr_string "AFTER CONFIGURE "; 
	prerr_endline (string_of_event x);
	false);
  let text = GText.view ~packing:window#add () in
  let buffer = text#buffer in
  text#event#connect#button_press ~callback:
    begin fun ev ->
      GdkEvent.Button.button ev = 3 &&
      GdkEvent.get_type ev = `BUTTON_PRESS &&
      begin
	let pos = buffer#get_iter_at_mark `INSERT in
	GdkEvent.Button.set_button ev 1;
	text#event#send (ev :> GdkEvent.any);
	Printf.printf "Position is %d.\n" pos#offset;
	flush stdout;
	buffer#move_mark `INSERT ~where:pos;
        GtkSignal.stop_emit ();
	true
      end
    end;
  window#show ();
  GMain.main ()
