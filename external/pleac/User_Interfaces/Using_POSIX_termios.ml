(* ********************************************************************** *)
(* Using POSIX termios *)
(* ********************************************************************** *)
let pleac_Using_POSIX_termios () = 
  #!/usr/bin/ocaml
  (* demo POSIX termios *)
  
  #load "unix.cma";;
  
  let uncontrol c =
    if c >= '\128' && c <= '\255'
    then Printf.sprintf "M-%c" (Char.chr (Char.code c land 127))
    else if (c >= '\000' && c < '\031') || c = '\127'
    then Printf.sprintf "^%c" (Char.chr (Char.code c lxor 64))
    else String.make 1 c
  
  let term = Unix.tcgetattr Unix.stdin
  let erase = term.Unix.c_verase
  let kill = term.Unix.c_vkill
  
  let () =
    Printf.printf "Erase is character %d, %s\n"
      (Char.code erase)
      (uncontrol erase);
    Printf.printf "Kill is character %d, %s\n"
      (Char.code kill)
      (uncontrol kill)
  
  let () =
    term.Unix.c_verase <- '#';
    term.Unix.c_vkill <- '@';
    Unix.tcsetattr Unix.stdin Unix.TCSANOW term;
    Printf.printf "erase is #, kill is @; type something: %!";
    let line = input_line stdin in
    Printf.printf "You typed: %s\n" line;
    term.Unix.c_verase <- erase;
    term.Unix.c_vkill <- kill;
    Unix.tcsetattr Unix.stdin Unix.TCSANOW term
  
  (*-----------------------------*)
  
  module HotKey :
  sig
    val cbreak : unit -> unit
    val cooked : unit -> unit
    val readkey : unit -> char
  end =
  struct
    open Unix
  
    let oterm = {(tcgetattr stdin) with c_vtime = 0}
    let noecho = {oterm with
                    c_vtime = 1;
                    c_echo = false;
                    c_echok = false;
                    c_icanon = false}
  
    let cbreak () = tcsetattr stdin TCSANOW noecho
    let cooked () = tcsetattr stdin TCSANOW oterm
  
    let readkey () =
      cbreak ();
      let key = input_char (Pervasives.stdin) in
      cooked ();
      key
  
    let () = cooked ()
  end
  

