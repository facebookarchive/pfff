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

(* $Id: widgets.ml 1352 2007-07-12 08:56:18Z zoggy $ *)

open GObj

class multibox ~rows ~columns ?(row_view = rows) ?(col_view = columns)
    ?packing ?show () =
  let sw =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ?show ?packing () in
  let vp = GBin.viewport ~shadow_type:`NONE ~packing:sw#add in
  let table =
    GPack.table ~columns ~rows ~homogeneous:true ~packing:vp#add () in
  let buttons =
    Array.init ~len:columns
      ~f:(fun left -> Array.init ~len:rows
	  ~f:(fun top -> GButton.button
              ~packing:(table#attach ~top ~left ~expand:`BOTH)))
  in
  object (self)
    inherit widget sw#as_widget
    method cell ~col ~row = buttons.(col).(row)
    initializer
      let id = ref None in
      id := Some
	  (sw#event#connect#expose ~after:true ~callback:
	     begin fun _ ->
	       may !id ~f:sw#connect#disconnect;
	       let height = table#misc#allocation.height * row_view / rows
	       and width = table#misc#allocation.width * col_view / columns in
	       vp#misc#set_size ~height ~width;
	       false
	     end);
      table#focus#set_vadjustment vp#vadjustment
  end
