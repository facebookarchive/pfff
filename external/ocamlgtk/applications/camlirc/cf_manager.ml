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

(* $Id: cf_manager.ml 1354 2007-07-20 04:18:38Z garrigue $ *)

class channel_factory_manager =
  object
    val mutable channel_factory_table : 
	(string * I_channel.i_channel_factory) list = []
    method get_key = List.map (fun (s,_) -> s) channel_factory_table
    method get_constructor s = 
      (List.assoc s channel_factory_table)#new_channel_object
    method add_channel_factory f =
      channel_factory_table <- (f#module_name, f)::channel_factory_table
  end
    
let channel_factory_manager = new channel_factory_manager
