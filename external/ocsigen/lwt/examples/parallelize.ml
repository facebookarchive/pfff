(*
 * parallelize.ml
 * --------------
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

(* Reads commands from standard input and launch them in parallel,
   using as many processes as the number of CPUs. *)

open Lwt_unix
open Lwt
open Lwt_io
open Lwt_process

(* Reads one command, launch it and waits for when it termination,
   then start again: *)
let rec launch () =
  read_line_opt stdin >>= function
    | None ->
        return ()
    | Some line ->
        exec (shell line) >> launch ()

(* Creates the initial <N> threads, where <N> is the number of
   CPUs: *)
let rec create_threads = function
  | 0 ->
      return ()
  | n ->
      launch () <&> create_threads (n - 1)

(* Counts the number of CPUs using "/proc/cpuinfo": *)
let cpus_count () =
  Lwt_stream.fold (fun _ n -> succ n)
    (Lwt_stream.filter
       (fun line -> match Text.words line with
          | "processor" :: _ -> true
          | _ -> false)
       (lines_of_file "/proc/cpuinfo")) 0

let _ = Lwt_main.run (cpus_count () >>= create_threads)
