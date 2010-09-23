(***********************************************************************)
(*                                                                     *)
(*                         The Caml/MPI interface                      *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(* $Id: test_mandel.ml,v 1.2 2003/03/31 14:22:57 xleroy Exp $ *)

open Graphics

(* compute the color of a pixel *)
let color_pixel cr ci res =
  let zr = ref cr in
  let zi = ref ci in
  let c = ref 0 in
  while !c < res && !zr *. !zr +. !zi *. !zi <= 4.0 do
    let nzr = !zr *. !zr -. !zi *. !zi -. cr
    and nzi = 2.0 *. !zr *. !zi -. ci in
    zr := nzr;
    zi := nzi;
    c := !c + 1
  done;
  !c

(* compute a displayable color *)
let color_factor = 255*255*255+255*255+255
let colorof c res =  c * color_factor / res

(* produce a line *)
let mandel_row (x0,y0,x1,y1) n res j =
  let dx = (x1-.x0)/.(float n) in
  let dy = (y1-.y0)/.(float n) in
  let zi = y0 +. (dy *. (float j)) in
  let line = Array.create n black in
  for i = 0 to n - 1 do
    let zr = x0 +. (dx *. (float i)) in
    line.(i) <- colorof (color_pixel zr zi res) res
  done;
  (j, line)

(* Worker function: produce lines and send them to display *)

let worker window n res =
  try
    while true do
      let j = Mpi.receive_int 0 0 Mpi.comm_world in
      if j >= n then raise Exit;
      Mpi.send (mandel_row window n res j) 0 0 Mpi.comm_world
    done
  with Exit -> ()

(* Plot one line *)
let plot_row (j, line) =
  draw_image (make_image [| line |]) 0 j

(* Server function: distribute work and plot the lines *)

let server n =
  open_graph (Printf.sprintf " %dx%d" n n);
  let numworkers = Mpi.comm_size Mpi.comm_world - 1 in
  (* Send initial work *)
  for i = 1 to numworkers do
    Mpi.send_int (i - 1) i 0 Mpi.comm_world
  done;
  (* Enter server loop *)
  let numlines = ref n in
  let nextline = ref numworkers in
  while !numlines > 0 do
    let (row, src, _) = Mpi.receive_status Mpi.any_source 0 Mpi.comm_world in
    Mpi.send_int !nextline src 0 Mpi.comm_world;
    incr nextline;
    plot_row row;
    decr numlines
  done;
  print_string "Press <RETURN> to terminate..."; flush stdout; read_line(); ()

(* Entry point *)

let _ =
  let window = (-1.0, -1.0, 2.0, 1.0) in
  let n = 500 in
  if Mpi.comm_rank Mpi.comm_world = 0
  then server n
  else worker window n 500;
  Mpi.barrier Mpi.comm_world
