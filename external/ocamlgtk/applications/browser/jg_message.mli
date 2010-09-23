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

(* $Id: jg_message.mli 1352 2007-07-12 08:56:18Z zoggy $ *)

val formatted :
  title:string ->
  ?on:#GContainer.container ->
  ?ppf:Format.formatter ->
  ?width:int ->
  ?maxheight:int ->
  ?minheight:int ->
  unit -> GText.view * (unit -> unit)

val ask :
    title:string -> ?master:#GWindow.window_skel ->
    ?no:bool -> ?cancel:bool -> string -> [`Cancel|`No|`Yes]

val info :
    title:string -> ?master:#GWindow.window_skel -> string -> unit
