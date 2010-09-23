(* ********************************************************************** *)
(* Communicating Between Related Processes *)
(* ********************************************************************** *)
let pleac_Communicating_Between_Related_Processes () = 
  (* pipe1 - use pipe and fork so parent can send to child *)
  #load "unix.cma"
  open Unix
  
  let reader, writer = pipe ()
  
  let () =
    match fork () with
      | 0 ->
          close writer;
          let input = in_channel_of_descr reader in
          let line = input_line input in
          Printf.printf "Child Pid %d just read this: `%s'\n" (getpid ()) line;
          close reader;  (* this will happen anyway *)
          exit 0
      | pid ->
          close reader;
          let output = out_channel_of_descr writer in
          Printf.fprintf output "Parent Pid %d is sending this\n" (getpid ());
          flush output;
          close writer;
          ignore (waitpid [] pid)
  
  (*-----------------------------*)
  
  (* pipe2 - use pipe and fork so child can send to parent *)
  #load "unix.cma"
  open Unix
  
  let reader, writer = pipe ()
  
  let () =
    match fork () with
      | 0 ->
          close reader;
          let output = out_channel_of_descr writer in
          Printf.fprintf output "Child Pid %d is sending this\n" (getpid ());
          flush output;
          close writer;  (* this will happen anyway *)
          exit 0
      | pid ->
          close writer;
          let input = in_channel_of_descr reader in
          let line = input_line input in
          Printf.printf "Parent Pid %d just read this: `%s'\n" (getpid ()) line;
          close reader;
          ignore (waitpid [] pid)
  
  (*-----------------------------*)
  
  (* pipe3 and pipe4 demonstrate the use of perl's "forking open" feature to
   * reimplement pipe1 and pipe2. Since OCaml does not support such a feature,
   * these are skipped here. *)
  
  (*-----------------------------*)
  
  (* pipe5 - bidirectional communication using two pipe pairs
             designed for the socketpair-challenged *)
  #load "unix.cma"
  open Unix
  
  let parent_rdr, child_wtr = pipe ()
  let child_rdr, parent_wtr = pipe ()
  
  let () =
    match fork () with
      | 0 ->
          close child_rdr;
          close child_wtr;
          let input = in_channel_of_descr parent_rdr in
          let output = out_channel_of_descr parent_wtr in
          let line = input_line input in
          Printf.printf "Child Pid %d just read this: `%s'\n" (getpid ()) line;
          Printf.fprintf output "Child Pid %d is sending this\n" (getpid ());
          flush output;
          close parent_rdr;
          close parent_wtr;
          exit 0
      | pid ->
          close parent_rdr;
          close parent_wtr;
          let input = in_channel_of_descr child_rdr in
          let output = out_channel_of_descr child_wtr in
          Printf.fprintf output "Parent Pid %d is sending this\n" (getpid());
          flush output;
          let line = input_line input in
          Printf.printf "Parent Pid %d just read this: `%s'\n" (getpid ()) line;
          close child_rdr;
          close child_wtr;
          ignore (waitpid [] pid)
  
  (*-----------------------------*)
  
  (* pipe6 - bidirectional communication using socketpair
             "the best ones always go both ways" *)
  #load "unix.cma"
  open Unix
  
  let child, parent = socketpair PF_UNIX SOCK_STREAM 0
  
  let () =
    match fork () with
      | 0 ->
          close child;
          let input = in_channel_of_descr parent in
          let output = out_channel_of_descr parent in
          let line = input_line input in
          Printf.printf "Child Pid %d just read this: `%s'\n" (getpid ()) line;
          Printf.fprintf output "Child Pid %d is sending this\n" (getpid ());
          flush output;
          close parent;
          exit 0
      | pid ->
          close parent;
          let input = in_channel_of_descr child in
          let output = out_channel_of_descr child in
          Printf.fprintf output "Parent Pid %d is sending this\n" (getpid ());
          flush output;
          let line = input_line input in
          Printf.printf "Parent Pid %d just read this: `%s'\n" (getpid ()) line;
          close child;
          ignore (waitpid [] pid)
  
  (*-----------------------------*)
  
  (* Simulating a pipe using a socketpair. *)
  let reader, writer = socketpair PF_UNIX SOCK_STREAM 0 in
  shutdown reader SHUTDOWN_SEND;      (* no more writing for reader *)
  shutdown writer SHUTDOWN_RECEIVE;   (* no more reading for writer *)
  

