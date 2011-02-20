(*
 * ex_gtk.ml
 * ---------
 *
 * Copyright (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Jeremie Dimino nor the names of his
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE AUTHOR AND CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(* Example using GTK.

   It display a counter which is incremented every second by a
   cooperative thread.
*)

open Lwt

(* The counter *)
let num = ref 0

lwt () =
  ignore (GMain.init ());

  (* Needed for using Lwt+GLib: *)
  Lwt_glib.install ();

  let waiter, wakener = Lwt.wait () in

  (* Creates the windows, and connects standard events: *)
  let window = GWindow.window ~border_width:10 () in
  let _ = window#event#connect#delete ~callback:(fun _ -> false)
  and _ = window#connect#destroy ~callback:(fun () -> wakeup wakener ()) in

  (* Creates a box, the decrement button and the label: *)
  let box = GPack.vbox ~packing:window#add () in
  let button = GButton.button ~label:"decrement" ~packing:box#add () in
  let label = GMisc.label ~packing:box#add () in

  (* The cooperative thread which increment the counter every
     seconds: *)
  let rec loop _ =
    incr num;
    label#set_label (string_of_int !num);
    Lwt.bind (Lwt_unix.sleep 1.0) loop
  in

  (* Starts it, without waiting for the result: *)
  Lwt.ignore_result (loop ());

  (* Connects clicks on the decrement button: *)
  let _ = button#connect#clicked ~callback:(fun _ ->
                                              decr num;
                                              label#set_label (string_of_int !num)) in

  (* Display the main window of the application: *)
  window#show ();

  waiter
