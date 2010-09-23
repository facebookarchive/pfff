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

(* $Id: test.ml,v 1.7 2003/03/31 14:22:57 xleroy Exp $ *)

(* Regression test *)

open Printf
open Mpi

(* comm_size, comm_rank *)

let size = comm_size comm_world
let myrank = comm_rank comm_world

let _ =
  printf "%d: comm_size = %d" myrank size; print_newline()

(* Barrier *)

let _ = barrier comm_world

(* Simple send/receive *)

let _ =
  if myrank = 0 then begin
    printf "%d: sending %s" myrank "aa"; print_newline();
    send "aa" 1 0 comm_world;
    let n = receive any_source any_tag comm_world in
    printf "%d: received %s" myrank n; print_newline()
  end else begin
    let n = receive any_source any_tag comm_world in
    let n' = n ^ "a" in
    printf "%d: received %s, resending %s" myrank n n'; print_newline();
    send n' ((myrank + 1) mod size) 0 comm_world
  end

let _ = barrier comm_world

(* Send and receive with tags *)

let _ =
  if myrank = 0 then begin
    printf "%d: sending %s (tag 0)" myrank "aa"; print_newline();
    send "aa" 1 0 comm_world;
    printf "%d: sending %s (tag 1)" myrank "bb"; print_newline();
    send "bb" 1 1 comm_world;
    let (n, src, tag) = receive_status any_source any_tag comm_world in
    printf "%d: received %s (tag %d) from %d" myrank n tag src;
    print_newline();
    let (n, src, tag) = receive_status any_source any_tag comm_world in
    printf "%d: received %s (tag %d) from %d" myrank n tag src;
    print_newline()
  end else begin
    let (n1, src, tag1) = receive_status any_source 0 comm_world in
    let n1' = n1 ^ "a" in
    printf "%d: received %s (tag %d) from %d, resending %s"
           myrank n1 tag1 src n1'; print_newline();
    let (n2, src, tag2) = receive_status any_source 1 comm_world in
    let n2' = n2 ^ "b" in
    printf "%d: received %s (tag %d) from %d, resending %s"
           myrank n2 tag2 src n2'; print_newline();
    send n2'  ((myrank + 1) mod size) 1 comm_world;
    send n1'  ((myrank + 1) mod size) 0 comm_world
  end

let _ = barrier comm_world

(* Send and receive base types *)

let test_send_recv sendfun recvfun transf printfun data =
  if myrank = 0 then begin
    for i = 1 to size - 1 do
      printf "0: sending %a to %d" printfun data.(i-1) i; print_newline();
      sendfun data.(i-1) i 0 comm_world
    done;
    for i = 1 to size - 1 do
      let x = recvfun i 0 comm_world in
      printf "0: received %a" printfun x; print_newline()
    done
  end else begin
    let x = recvfun 0 0 comm_world in
    let y = transf x in
    printf "%d: received %a, sending %a" myrank printfun x printfun y;
    print_newline();
    sendfun y 0 0 comm_world
  end

let output_int o i = output_string o (string_of_int i)
let output_float o f = output_string o (string_of_float f)
let output_array fn o a =
  output_string o "[ ";
  for i = 0 to Array.length a - 1 do
    fn o a.(i); output_char o ' '
  done;
  output_string o "]"
let output_int_array = output_array output_int
let output_float_array = output_array output_float

let _ =
  test_send_recv send_int receive_int (fun n -> n+1) output_int
    [| 10; 20; 30; 40; 50; 60; 70; 80; 90 |];
  test_send_recv send_float receive_float (fun n -> n *. 2.0) output_float
    [| 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9 |];
  let ia = Array.make 3 0 in
  test_send_recv send_int_array
               (fun src tag comm -> receive_int_array ia src tag comm; ia)
               (Array.map (fun n -> n+1))
               output_int_array
               [| [|10;11;12|]; [|20;21;22|]; [|30;31;32|]; [|40;41;42|] |];
  let fa = Array.make 2 0.0 in
  test_send_recv send_float_array
               (fun src tag comm -> receive_float_array fa src tag comm; fa)
               (Array.map (fun n -> n +. 0.01))
               output_float_array
               [| [|1.1; 1.2|]; [|2.1; 2.2|]; [|3.1; 3.2|]; [|4.1; 4.2|] |]

let _ = barrier comm_world

(* Barrier, 2 *)

let _ =
  if myrank > 0 then Unix.sleep myrank;
  printf "%d: hitting barrier" myrank; print_newline();
  barrier comm_world;
  if myrank = 0 then begin printf "Jumped barrier"; print_newline() end

(* Broadcast *)

let test_broadcast broadcastfun printfun data =
  if myrank = 0 then begin
    printf "0: broadcasting %a" printfun data; print_newline()
  end;
  let res = broadcastfun data 0 comm_world in
  printf "%d: received %a" myrank printfun data; print_newline()

let _ =
  test_broadcast broadcast output_string "Hello!";
  test_broadcast broadcast_int output_int 123456;
  test_broadcast broadcast_float output_float 3.141592654;
  let ia = if myrank = 0 then [| 123; 456; 789 |] else Array.make 3 0 in
  test_broadcast (fun x r c -> broadcast_int_array x r c; x)
                 output_int_array ia;
  let fa = if myrank = 0 then [| 3.14; 2.718 |] else Array.make 2 0.0 in
  test_broadcast (fun x r c -> broadcast_float_array x r c; x)
                 output_float_array fa

let _ = barrier comm_world

(* Scatter *)

let test_scatter scatterfun printfun1 printfun2 data =
  if myrank = 0 then begin
    printf "0: scattering %a" printfun1 data;
    print_newline()
  end;
  let res = scatterfun data 0 comm_world in
  printf "%d: received %a" myrank printfun2 res; print_newline();
  barrier comm_world
  
let _ =
  test_scatter scatter (output_array output_string) output_string
    [| "Six"; "scies"; "scient"; "six"; "cigares" |];
  test_scatter scatter_int output_int_array output_int
    [| 12; 34; 56; 78; 90 |];
  test_scatter scatter_float output_float_array output_float
    [| 1.2; 3.4; 5.6; 7.8; 9.1 |];
  let ia = Array.make 3 0 in
  test_scatter (fun d r c -> scatter_int_array d ia r c; ia)
               output_int_array output_int_array
               [| 10;11;12; 20;21;22; 30;31;32; 40;41;42; 50;51;52 |];
  let fa = Array.make 3 0.0 in
  test_scatter (fun d r c -> scatter_float_array d fa r c; fa)
               output_float_array output_float_array
               [| 1.0;1.1;1.2; 2.0;2.1;2.2; 3.0;3.1;3.2;
                  4.0;4.1;4.2; 5.0;5.1;5.2 |]


(* Gather *)

let test_gather gatherfun printfun1 printfun2 data =
  printf "%d: sending %a" myrank printfun2 data; print_newline();
  let res = gatherfun data 0 comm_world in
  if myrank = 0 then begin
    printf "0: gathered %a" printfun1 res;
    print_newline()
  end;
  barrier comm_world
  
let _ =
  test_gather gather (output_array output_string) output_string
    [| "The"; "quick"; "fox"; "jumps"; "over" |].(myrank);
  let ia = Array.make size 0 in
  test_gather (fun d r c -> gather_int d ia r c; ia) 
              output_int_array output_int
              [| 12; 34; 56; 78; 90 |].(myrank);
  let fa = Array.make size 0.0 in
  test_gather (fun d r c -> gather_float d fa r c; fa) 
              output_float_array output_float
              [| 1.2; 3.4; 5.6; 7.8; 9.1 |].(myrank);
  let ia = Array.make (3 * size) 0 in
  test_gather (fun d r c -> gather_int_array d ia r c; ia) 
              output_int_array output_int_array
              [| myrank*10; myrank*10 + 1; myrank*10 + 2 |];               
  let fa = Array.make (3 * size) 0.0 in
  test_gather (fun d r c -> gather_float_array d fa r c; fa) 
              output_float_array output_float_array
              [| float myrank; float myrank +. 0.1; float myrank +. 0.2 |]

(* Gather to all *)

let test_allgather gatherfun printfun1 printfun2 data =
  printf "%d: sending %a" myrank printfun2 data; print_newline();
  let res = gatherfun data comm_world in
  printf "%d: gathered %a" myrank printfun1 res;
  print_newline();
  barrier comm_world
  
let _ =
  test_allgather allgather (output_array output_string) output_string
    [| "The"; "quick"; "fox"; "jumps"; "over" |].(myrank);
  let ia = Array.make size 0 in
  test_allgather (fun d c -> allgather_int d ia c; ia) 
              output_int_array output_int
              [| 12; 34; 56; 78; 90 |].(myrank);
  let fa = Array.make size 0.0 in
  test_allgather (fun d c -> allgather_float d fa c; fa) 
              output_float_array output_float
              [| 1.2; 3.4; 5.6; 7.8; 9.1 |].(myrank);
  let ia = Array.make (3 * size) 0 in
  test_allgather (fun d c -> allgather_int_array d ia c; ia) 
              output_int_array output_int_array
              [| myrank*10; myrank*10 + 1; myrank*10 + 2 |];               
  let fa = Array.make (3 * size) 0.0 in
  test_allgather (fun d c -> allgather_float_array d fa c; fa) 
              output_float_array output_float_array
              [| float myrank; float myrank +. 0.1; float myrank +. 0.2 |]

(* Reduce *)

let name_of_int_reduce = function
    Int_max -> "Int_max"
  | Int_min -> "Int_min"
  | Int_sum -> "Int_sum"
  | Int_prod -> "Int_prod"
  | Int_land -> "Int_land"
  | Int_lor -> "Int_lor"
  | Int_xor -> "Int_xor"

let name_of_float_reduce = function
    Float_max -> "Float_max"
  | Float_min -> "Float_min"
  | Float_sum -> "Float_sum"
  | Float_prod -> "Float_prod"

let test_reduce reducefun reduceops printfun printop data =
  printf "%d: my data is %a" myrank printfun data; print_newline();
  List.iter
    (fun op ->
      let res = reducefun data op 0 comm_world in
      if myrank = 0 then begin
        printf "0: result of reduction %s is %a" (printop op) printfun res;
        print_newline()
      end)
    reduceops;
  barrier comm_world

let _ =
  test_reduce reduce_int
              [Int_max; Int_min; Int_sum; Int_prod; Int_land; Int_lor; Int_xor]
              output_int name_of_int_reduce
              (myrank + 1);
  test_reduce reduce_float
              [Float_max; Float_min; Float_sum; Float_prod]
              output_float name_of_float_reduce
              (float myrank +. 1.0);
  let ia = Array.make 3 0 in
  test_reduce (fun d op r c -> reduce_int_array d ia op r c; ia)
              [Int_max; Int_min; Int_sum; Int_prod; Int_land; Int_lor; Int_xor]
              output_int_array name_of_int_reduce
              [| myrank * 10; myrank * 10 + 1; myrank * 10 + 2 |];
  let fa = Array.make 3 0.0 in
  test_reduce (fun d op r c -> reduce_float_array d fa op r c; fa)
              [Float_max; Float_min; Float_sum; Float_prod]
              output_float_array name_of_float_reduce
              [| float myrank; float myrank +. 0.1; float myrank +. 0.2 |]

(* Reduce all *)

let test_reduceall reducefun reduceop printfun data =
  printf "%d: my data is %a" myrank printfun data; print_newline();
  let res = reducefun data reduceop comm_world in
  barrier comm_world;
  printf "%d: result of reduction is %a" myrank printfun res;
  print_newline();
  barrier comm_world

let _ =
  test_reduceall allreduce_int Int_sum
              output_int
              (myrank + 1);
  test_reduceall allreduce_float Float_prod
              output_float
              (float myrank +. 1.0);
  let ia = Array.make 3 0 in
  test_reduceall (fun d op c -> allreduce_int_array d ia op c; ia)
              Int_sum
              output_int_array
              [| myrank * 10; myrank * 10 + 1; myrank * 10 + 2 |];
  let fa = Array.make 3 0.0 in
  test_reduceall (fun d op c -> allreduce_float_array d fa op c; fa)
              Float_sum
              output_float_array
              [| float myrank; float myrank +. 0.1; float myrank +. 0.2 |]


(* Scan *)

let test_scan scanfun reduceop printfun data =
  printf "%d: my data is %a" myrank printfun data; print_newline();
  let res = scanfun data reduceop comm_world in
  barrier comm_world;
  printf "%d: result of scanning is %a" myrank printfun res;
  print_newline();
  barrier comm_world

let _ =
  test_scan scan_int
              Int_sum
              output_int
              (myrank + 1);
  test_scan scan_float
              Float_sum
              output_float
              (float myrank +. 1.0);
  let ia = Array.make 3 0 in
  test_scan (fun d op c -> scan_int_array d ia op c; ia)
              Int_sum
              output_int_array
              [| myrank * 10; myrank * 10 + 1; myrank * 10 + 2 |];
  let fa = Array.make 3 0.0 in
  test_scan (fun d op c -> scan_float_array d fa op c; fa)
              Float_sum
              output_float_array
              [| float myrank; float myrank +. 0.1; float myrank +. 0.2 |]

(* Comm split *)

let send_in_comm c init incr =
  let rank_in_c = comm_rank c
  and size_of_c = comm_size c in
  if rank_in_c = 0 then begin
    printf "%d[%d]: sending %s" rank_in_c myrank init; print_newline();
    send init 1 0 c;
    let n = receive any_source any_tag c in
    printf "%d[%d]: received %s" rank_in_c myrank n; print_newline()
  end else begin
    let n = receive any_source any_tag c in
    let n' = n ^ incr in
    printf "%d[%d]: received %s, resending %s" rank_in_c myrank n n';
    print_newline();
    send n' ((rank_in_c + 1) mod size_of_c) 0 c
  end

let _ =
  let c = comm_split comm_world (myrank mod 2) 0 in
  if myrank mod 2 = 0
  then send_in_comm c "aa" "a"
  else send_in_comm c "bb" "b";
  barrier comm_world

(* Cartesian topology *)

let cart = cart_create comm_world [|2;2|] [|false;false|] true

let test_dims_create n hints =
  printf "dims_create %d %a = %a" n output_int_array hints
                                    output_int_array (dims_create n hints);
  print_newline()

let _ =
  if myrank = 0 then begin
    for x = 0 to 1 do for y = 0 to 1 do
      printf "(%d, %d) -> rank %d" x y (cart_rank cart [|x;y|]);
      print_newline()
    done done;
    for r = 0 to comm_size cart - 1 do
      let c = cart_coords cart r in
      printf "rank %d -> (%d, %d)" r c.(0) c.(1);
      print_newline()
    done;
    test_dims_create 60 [|0;0;0|];
    test_dims_create 60 [|3;0;0|];
    test_dims_create 60 [|0;4;0|];
    test_dims_create 60 [|3;0;5|]
  end;
  barrier comm_world

(* Wtime *)

let _ =
  printf "%d: my wtime is %.3f" myrank (wtime()); print_newline()
