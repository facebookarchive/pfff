(**************************************************************************)
(*     Lablgtk - Camlirc                                                  *)
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
(*      Nobuaki Yoshida  <nyoshi@dd.iij4u.or.jp>                          *)
(*      Jacques Garrigue <garrigue@kurims.kyoto-u.ac.jp>                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: entry.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
class message_entry_signals ~(message: string GUtil.signal)=
  object
    inherit GUtil.ml_signals [message#disconnect]
    method message = message#connect ~after
  end

class message_entry ~(handler:Message_handler.irc_message_handler)
    ~(channels:Channelview.channels) ?packing ?show () =
  let e = GEdit.entry ?packing ?show ()
  and message = new GUtil.signal ()
  in
  let _ = e#connect#activate 
      ~callback:(fun () -> message#call e#text; e#set_text "")
  in
  object
    val handler = handler
    val channels = channels
    method message = message
    method connect = new message_entry_signals ~message
  end
