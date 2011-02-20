open Lwt
open Bigarray

let file = ref ""
let () = Arg.parse [] (fun x -> file := x) "plop"
let file_fd = Unix.openfile !file [] 0

let len = (Unix.stat !file).Unix.st_size

let input = Lwt_io.of_unix_fd ~mode:Lwt_io.input file_fd

let main () =
  let finished = ref false in
  let sleeps = ref 0 in
  let max_sleep = ref 0. in
  let rec ping () =
    if !finished
    then return ()
    else
      let start = Unix.gettimeofday () in
      lwt () = Lwt_unix.sleep 0.001 in
      let stop = Unix.gettimeofday () in
      sleeps := !sleeps + 1;
      max_sleep := max (!max_sleep) (stop -. start);
      lwt () = Lwt_io.print "." in
      lwt () = Lwt_io.flush Lwt_io.stdout in
      ping ()
  in
  let buf = String.create (4096*256) in
  let rec do_read () =
    lwt n = Lwt_io.read_into input buf 0 (4096*256) in
    if n = 0
    then
      ( finished := true;
	return () )
    else do_read ()
  in
  let _ = ping () in
  lwt () = do_read () in
  return (!sleeps,!max_sleep)


let start = Unix.gettimeofday ()
let sleeps,max_sleep = Lwt_unix.run (main ())
let stop = Unix.gettimeofday ()
let time = stop -. start
let _ = Printf.printf "time: %fs speed: %fmo/s\nsleeps: %i max sleep: %fs\n%!" time (((float len)/.time) /. 1048576.) sleeps max_sleep

