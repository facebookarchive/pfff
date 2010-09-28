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

(* $Id: gtkThread.mli 1518 2010-06-25 09:23:44Z garrigue $ *)

(* Basic functions *)

(** The main loop to use with threads. [GMain.main] does not work!
    This changes [GMain.main] to call [threaded_main] rather than
    [GtkMain.Main.default_main], so subsequent calls will work.
    The first call sets the GUI thread, and subsequent calls
    to [main] will be automatically routed through [sync] *)
val main : unit -> unit
(** Start the main loop in a new GUI thread. Do not use recursively. *)
val start : unit -> Thread.t
(** The real main function *)
val thread_main : unit -> unit
(** Forget the current GUI thread. The next call to [main]
    will register its caller as GUI thread. *)
val reset : unit -> unit

(* Jobs are needed for windows, as you cannot do GTK work from
   another thread.
   Even under Unix some calls need to come from the main thread.
   The basic idea is to either use async (if you don't need a result)
   or sync whenever you call a GTK related function from another thread
   (for instance with the threaded toplevel).
   With sync, beware of deadlocks!
*)

(** Add an asynchronous job (to do in the main thread) *)
val async : ('a -> unit) -> 'a -> unit
(** Add a synchronous job (to do in the main thread) *)
val sync : ('a -> 'b) -> 'a -> 'b
(** Whether it is safe to call most GTK functions directly from
    the current thread *)
val gui_safe : unit -> bool
(** Allow other threads to run, and process the message queue.
    The following ensures that messages will be processed even
    if another main loop is running:
      [Glib.Timeout.add ~ms:100 ~callback:GtkThread.do_jobs] *)
val do_jobs : unit -> bool
