open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * related work:  https://gitorious.org/parmap
 *)

(*****************************************************************************)
(* Building block *)
(*****************************************************************************)

(* src: harrop article on fork-based parallelism *)
let invoke f x =
  let input, output = Unix.pipe() in
  match Unix.fork() with
  (* pad: what is this ?? *)
  | -1 -> (let v = f x in fun () -> v)
  (* child *)
  | 0 ->
      Unix.close input;
      let output = Unix.out_channel_of_descr output in
      Marshal.to_channel output (try `Res(f x) with e -> `Exn e) [];
      close_out output;
      exit 0
  (* parent *)
  | pid ->
      Unix.close output;
      let input = Unix.in_channel_of_descr input in
      fun () ->
        let v = Marshal.from_channel input in
        ignore (Unix.waitpid [] pid);
        close_in input;
        match v with
        | `Res x -> x
        | `Exn e -> raise e;;

let parallel_map f xs =
  (* create all the fork *)
  let futures = List.map (invoke f) xs in
  (* sync, get all parents to waitpid *)
  List.map (fun futur -> futur ()) futures

(*****************************************************************************)
(* Poor's man job scheduler *)
(*****************************************************************************)

type 'a job = unit -> 'a
type 'a jobs = ('a job) list

(* 
 * This is a very naive job scheduler. One limitation is that before
 * launching another round we must wait for the slowest process. A
 * set of workers and a master model would be more efficient by always
 * feeding processors. A partial fix is to give a tasks number that
 * is quite superior to the actual number of processors.
 *)
let map_jobs ~tasks xs =
  if tasks = 1 
  then List.map (fun job -> job ()) xs
  else 
    let xxs = Common.pack_safe tasks xs in
    xxs +> List.map (fun xs ->
      (* do in parallel a batch of job *)
      parallel_map (fun job -> job ()) xs
    ) +> List.flatten
