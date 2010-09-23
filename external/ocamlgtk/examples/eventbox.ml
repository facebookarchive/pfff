(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: events.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

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
  let w = GWindow.window ~width:200 ~height:200 () in
  w#connect#destroy ~callback:GMain.quit ;

  let eb = GBin.event_box ~packing:w#add () in
  eb#event#add [`ALL_EVENTS];
  eb#event#connect#any 
   (fun x -> 
	prerr_string "before "; 
	prerr_endline (string_of_event x);
	false);
  eb#event#connect#after#any 
   (fun x -> 
	prerr_string "after "; 
	prerr_endline (string_of_event x);
	false);
  eb#event#connect#expose
   (fun x -> 
	prerr_string "BEFORE EXPOSE "; 
	prerr_endline (string_of_event x);
	false);  
  eb#event#connect#after#expose 
   (fun x ->
	prerr_string "AFTER EXPOSE "; 
	prerr_endline (string_of_event x);
	false);

  w#show ();
  GMain.main ()
