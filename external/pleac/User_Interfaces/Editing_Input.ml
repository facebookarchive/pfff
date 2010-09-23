(* ********************************************************************** *)
(* Editing Input *)
(* ********************************************************************** *)
let pleac_Editing_Input () = 
  (* ledit is a pure-OCaml readline clone by Daniel de Rauglaudre.
     Source is available here: http://pauillac.inria.fr/~ddr/ledit/
  
     It is designed to be used as a command-line wrapper, but it
     can also be embedded in another program by building it normally
     and copying cursor.cmo, ledit.cmi, ledit.cmo, and ledit.mli into
     your project.
  
     A guide to compiling and embedding ledit can be found on the
     OCaml Tutorial Wiki: http://www.ocaml-tutorial.org/ledit
     At present, this guide applies to ledit 1.11. This recipe uses
     ledit 1.15, which is slightly different due to the addition of
     Unicode support (Ledit.input_char now returns a string instead
     of a char). *)
  
  #load "unix.cma";;
  #load "cursor.cmo";;
  #load "ledit.cmo";;
  
  let readline prompt =
    Ledit.set_prompt prompt;
    let buffer = Buffer.create 256 in
    let rec loop = function
      | "\n" ->
          Buffer.contents buffer
      | string ->
          Buffer.add_string buffer string;
          loop (Ledit.input_char stdin) in
    loop (Ledit.input_char stdin)
  
  let () =
    let prompt = "Prompt: " in
    let line = readline prompt in
    Printf.printf "You said: %s\n" line
  
  (*-----------------------------*)
  
  (* If you would prefer to use the real GNU Readline library, you can use
    camlidl to generate an interface to it. Here's a basic readline.idl: *)
  
  quote(c, "#include <stdio.h>");
  quote(c, "#include <readline/readline.h>");
  quote(c, "#include <readline/history.h>");
  
  [string, unique] char * readline ([string, unique] const char *prompt)
      quote(dealloc, "free(_res);");
  
  void add_history ([string] const char *string);
  
  (* And here is a test program: *)
  
  let () =
    while true do
      Printf.printf "You said: %s\n%!"
        (match Readline.readline (Some "Prompt: ") with
           | Some s -> Readline.add_history s; s
           | None -> exit 0)
    done
  
  (*-----------------------------*)
  
  (* vbsh - very bad shell *)
  let () =
    try
      while true do
        let cmd = readline "$ " in
        begin
          match Unix.system cmd with
            | Unix.WEXITED _ -> ()
            | Unix.WSIGNALED signal_num ->
                Printf.printf "Program killed by signal %d\n"
                  signal_num
            | Unix.WSTOPPED signal_num ->
                Printf.printf "Program stopped by signal %d\n"
                  signal_num
        end;
        flush stdout
      done
    with End_of_file -> ()
  

