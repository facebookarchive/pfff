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

(* $Id: rsvg.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

type size_fun = int -> int -> int * int

let round f =
  int_of_float (if f < 0. then f -. 0.5 else f +. 0.5)

let at_size rw rh w h = 
  (if rw < 0 then w else rw), (if rh < 0 then h else rh)
let at_zoom zx zy w h =
  if w < 0 || h < 0
  then (w, h)
  else
    (round (float w *. zx)), (round (float h *. zy))
let at_max_size mw mh w h =
  if w < 0 || h < 0
  then (w, h)
  else
    let zx = float mw /. float w in
    let zy = float mh /. float h in
    let z = min zx zy in
    (round (float w *. z)), (round (float h *. z))

let at_zoom_with_max zx zy mw mh w h =
  if w < 0 || h < 0
  then (w, h)
  else 
    let rw = round (float w *. zx) in
    let rh = round (float h *. zy) in
    if rw > mw || rh > mh
    then 
      let zx = float mw /. float w in
      let zy = float mh /. float h in
      let z = min zx zy in
      (round (float w *. z)), (round (float h *. z))
    else
      (rw, rh)

type error = FAILED
exception Error of error * string
external _init : unit -> unit = "ml_rsvg_init"
let _ = 
  Callback.register_exception "ml_rsvg_exn" (Error (FAILED, "")) ;
  _init ()

type t
external new_handle : unit -> t
    = "ml_rsvg_handle_new"
external new_handle_gz : unit -> t
    = "ml_rsvg_handle_new_gz"
external set_size_callback : t -> size_fun -> unit
    = "ml_rsvg_handle_set_size_callback"
external free_handle : t -> unit
    = "ml_rsvg_handle_free"
external close : t -> unit = "ml_rsvg_handle_close"
external write : t -> string -> off:int -> len:int -> unit = "ml_rsvg_handle_write"
external get_pixbuf : t -> GdkPixbuf.pixbuf = "ml_rsvg_handle_get_pixbuf"
external set_dpi : t -> float -> unit = "ml_rsvg_handle_set_dpi"
external set_default_dpi : float -> unit = "ml_rsvg_set_default_dpi"

type input = 
  | Rsvg_SubString of string * int * int
  | Rsvg_Buffer of int * (string -> int)

let render ?(gz=false) ?dpi ?size_cb input =
  let h = if gz then new_handle_gz () else new_handle () in
  Gaux.may (set_size_callback h) size_cb ;
  Gaux.may (set_dpi h) dpi ;
  try
    begin match input with
    | Rsvg_SubString (s, off, len) ->
	write h s ~off ~len
    | Rsvg_Buffer (len, fill) ->
	let buff = String.create len in
	let c = ref (fill buff) in
	while !c > 0 do
	  write h buff 0 !c ;
	  c := fill buff
	done
    end ;
    close h ;
    let pb = get_pixbuf h in
    free_handle h ; 
    pb
  with exn ->
    free_handle h ; raise exn

let render_from_string ?gz ?dpi ?size_cb ?pos ?len s =
  let off = Gaux.default 0 ~opt:pos in
  let len = Gaux.default (String.length s - off) ~opt:len in
  render ?gz ?dpi ?size_cb 
    (Rsvg_SubString (s, off, len))

let render_from_file ?(gz=false) ?dpi ?size_cb fname =
  let ic = if gz then open_in_bin fname else open_in fname in
  let pb = 
    try 
      render ~gz ?dpi ?size_cb
	(Rsvg_Buffer (4096, (fun b -> input ic b 0 (String.length b))))
    with exn ->
      close_in ic ; raise exn in
  close_in ic ;
  pb

