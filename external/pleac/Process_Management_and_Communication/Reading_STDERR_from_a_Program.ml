(* ********************************************************************** *)
(* Reading STDERR from a Program *)
(* ********************************************************************** *)
let pleac_Reading_STDERR_from_a_Program () = 
  #load "unix.cma";;
  
  (* Read STDERR and STDOUT at the same time. *)
  let () =
    let ph = Unix.open_process_in "cmd 2>&1" in
    while true do
      let line = input_line ph in
      (* ... *)
      ()
    done
  
  (*-----------------------------*)
  
  (* Read STDOUT and discard STDERR. *)
  let output = read_process "cmd 2>/dev/null"
  (* or *)
  let () =
    let ph = Unix.open_process_in "cmd 2>/dev/null" in
    while true do
      let line = input_line ph in
      (* ... *)
      ()
    done
  
  (*-----------------------------*)
  
  (* Read STDERR and discard STDOUT. *)
  let output = read_process "cmd 2>&1 1>/dev/null"
  (* or *)
  let () =
    let ph = Unix.open_process_in "cmd 2>&1 1>/dev/null" in
    while true do
      let line = input_line ph in
      (* ... *)
      ()
    done
  
  (*-----------------------------*)
  
  (* Swap STDOUT with STDERR and read original STDERR. *)
  let output = read_process "cmd 3>&1 1>&2 2>&3 3>&-"
  (* or *)
  let () =
    let ph = Unix.open_process_in "cmd 3>&1 1>&2 2>&3 3>&-" in
    while true do
      let line = input_line ph in
      (* ... *)
      ()
    done
  
  (*-----------------------------*)
  
  (* Redirect STDOUT and STDERR to temporary files. *)
  let () =
    ignore
      (Sys.command
         "program args 1>/tmp/program.stdout 2>/tmp/program.stderr")
  
  (*-----------------------------*)
  
  (* If the following redirections were done in OCaml... *)
  let output = read_process "cmd 3>&1 1>&2 2>&3 3>&-"
  
  (* ...they would look something like this: *)
  let fd3 = fd1
  let fd1 = fd2
  let fd2 = fd3
  let fd3 = undef
  
  (*-----------------------------*)
  
  (* Send STDOUT and STDERR to a temporary file. *)
  let () = ignore (Sys.command "prog args 1>tmpfile 2>&1")
  
  (* Send STDOUT to a temporary file and redirect STDERR to STDOUT. *)
  let () = ignore (Sys.command "prog args 2>&1 1>tmpfile")
  
  (*-----------------------------*)
  
  (* If the following redirections were done in OCaml... *)
  let () = ignore (Sys.command "prog args 1>tmpfile 2>&1")
  
  (* ...they would look something like this: *)
  let fd1 = "tmpfile"       (* change stdout destination first *)
  let fd2 = fd1             (* now point stderr there, too *)
  
  (*-----------------------------*)
  
  (* If the following redirections were done in OCaml... *)
  let () = ignore (Sys.command "prog args 2>&1 1>tmpfile")
  
  (* ...they would look something like this: *)
  let fd2 = fd1             (* stderr same destination as stdout *)
  let fd1 = "tmpfile"       (* but change stdout destination  *)
  

