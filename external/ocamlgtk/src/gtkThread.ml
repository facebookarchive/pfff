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

(* $Id: gtkThread.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open GtkMain

(* Job handling for Windows *)

let jobs : (unit -> unit) Queue.t = Queue.create ()
let m = Mutex.create ()
let with_jobs f =
  Mutex.lock m; let y = f jobs in Mutex.unlock m; y

let loop_id = ref None
let reset () = loop_id := None
let cannot_sync () =
  match !loop_id with None -> true
  | Some id -> Thread.id (Thread.self ()) = id

let gui_safe () =
  not (Sys.os_type = "Win32") || !loop_id = Some(Thread.id (Thread.self ()))

let has_jobs () = not (with_jobs Queue.is_empty)
let n_jobs () = with_jobs Queue.length
let do_next_job () = with_jobs Queue.take ()
let async j x = with_jobs (Queue.add (fun () -> j x))
let sync f x =
  if cannot_sync () then f x else
  let m = Mutex.create () in
  let res = ref None in
  Mutex.lock m;
  let c = Condition.create () in
  let j x =
    let y = f x in Mutex.lock m; res := Some y; Mutex.unlock m;
    Condition.signal c
  in
  async j x;
  while !res = None do Condition.wait c m done;
  match !res with Some y -> y | None -> assert false

(* We check first whether there are some event pending, and run
   some iterations. We then need to delay, thus focing a thread switch. *)

let thread_main_real () =
  try
    let loop = (Glib.Main.create true) in
    Main.loops := loop :: !Main.loops;
    loop_id := Some (Thread.id (Thread.self ()));
    while Glib.Main.is_running loop do
      let i = ref 0 in
      while !i < 100 && Glib.Main.pending () do
	Glib.Main.iteration true;
	incr i
      done;
      Thread.delay 0.001;
      for i = 1 to n_jobs () do do_next_job () done
    done;
    Main.loops := List.tl !Main.loops;
  with exn ->
    Main.loops := List.tl !Main.loops;
    raise exn

let thread_main () =
  sync thread_main_real ()

let main () =
  GtkMain.Main.main_func := thread_main;
  thread_main ()
      
let start () =
  reset ();
  Thread.create main ()

(* The code below would do nothing...
let _ =
  let mutex = Mutex.create () in
  let depth = ref 0 in
  GtkSignal.enter_callback :=
    (fun () -> if !depth = 0 then Mutex.lock mutex; incr depth);
  GtkSignal.exit_callback :=
    (fun () -> decr depth; if !depth = 0 then Mutex.unlock mutex)
*)
