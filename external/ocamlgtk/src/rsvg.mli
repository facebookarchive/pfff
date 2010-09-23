(**************************************************************************)
(*                Lablgtk                                                 *)
(*                                                                        *)
(*    This program is free software; you can redistribute it              *)
(*    and/or modify it under the terms of the GNU Library General         *)
(*    Public License as published by the Free Software Foundation         *)
(*    version 2, with the exception described in file COPYING which       *)
(*    comes with the library.                                             *)
(*                                                                        *)
(*    This program is distributed in the hope that it will be useful,     *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU Library General Public License for more details.                *)
(*                                                                        *)
(*    You should have received a copy of the GNU Library General          *)
(*    Public License along with this program; if not, write to the        *)
(*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         *)
(*    Boston, MA 02111-1307  USA                                          *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: rsvg.mli 1347 2007-06-20 07:40:34Z guesdon $ *)
(** librsvg bindings *)

type size_fun = int -> int -> int * int

val at_size : int -> int -> size_fun
val at_zoom : float -> float -> size_fun
val at_max_size : int -> int -> size_fun
val at_zoom_with_max : float -> float -> int -> int -> size_fun

val set_default_dpi : float -> unit

(** @raise Error if an error occurs loading data *)
(** @raise Sys_error if an error occurs while reading from the channel *)
val render_from_string :
  ?gz:bool -> 
  ?dpi:float -> 
  ?size_cb:size_fun -> 
  ?pos:int -> ?len:int ->
  string -> GdkPixbuf.pixbuf

(** @raise Error if an error occurs loading data *)
(** @raise Sys_error if an error occurs while reading from the file *)
val render_from_file :
  ?gz:bool -> 
  ?dpi:float -> 
  ?size_cb:size_fun -> 
  string -> GdkPixbuf.pixbuf
