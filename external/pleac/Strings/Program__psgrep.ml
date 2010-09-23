(* ********************************************************************** *)
(* Program: psgrep *)
(* ********************************************************************** *)
let pleac_Program__psgrep () = 
  #!/usr/bin/ocaml
  (* psgrep - print selected lines of ps output by
              compiling user queries into code *)
  #load "unix.cma";;
  
  (* Warning: In order to closely approximate the original recipe, this
     example performs dynamic evaluation using the toplevel. This mechanism
     is undocumented and not type-safe. Use at your own risk.
  
     The "psgrep" utility, defined below, can be used to filter the results
     of the command-line "ps" program. Here are some examples:
  
     Processes whose command names start with "sh":
  
     % psgrep 'String.sub command 0 2 = "sh"'
  
     Processes running with a user ID below 10:
  
     % psgrep 'uid < 10'
  
     Login shells with active ttys:
  
     % psgrep "command.[0] = '-'" 'tty <> "?"'
  
     Processes running on pseudo-ttys:
  
     % psgrep 'String.contains "pqrst" tty.[0]'
  
     Non-superuser processes running detached:
  
     % psgrep 'uid > 0 && tty = "?"'
  
     Huge processes that aren't owned by the superuser:
  
     % psgrep 'vsz > 50000' 'uid <> 0'
  *)
  
  (* Eval recipe thanks to Clément Capel. *)
  let () = Toploop.initialize_toplevel_env ()
  let eval text = let lexbuf = (Lexing.from_string text) in
    let phrase = !Toploop.parse_toplevel_phrase lexbuf in
    ignore (Toploop.execute_phrase false Format.std_formatter phrase)
  let get name = Obj.obj (Toploop.getvalue name)
  let set name value = Toploop.setvalue name (Obj.repr value)
  
  (* Type for "ps" results. *)
  type ps =
      {f : int; uid : int; pid : int; ppid : int; pri : int; ni : string;
       vsz : int; rss : int; wchan : string; stat : string; tty : string;
       time : string; command : string}
  
  (* Based on the GNU ps from Debian Linux. Other OSs will most likely
     require changes to this format. *)
  let parse_ps_line line =
    Scanf.sscanf line "%d %d %d %d %d %s %d %d %6s %4s %10s %4s %s@\000"
      (fun f uid pid ppid pri ni vsz rss wchan stat tty time command ->
         {f=f; uid=uid; pid=pid; ppid=ppid; pri=pri; ni=ni;
          vsz=vsz; rss=rss; wchan=wchan; stat=stat; tty=tty;
          time=time; command=command})
  
  let eval_predicate ps pred =
    (* Use "eval" to initialize each variable's name and type,
       then use "set" to set a value. *)
    eval "let f = 0;;";          set "f" ps.f;
    eval "let uid = 0;;";        set "uid" ps.uid;
    eval "let pid = 0;;";        set "pid" ps.pid;
    eval "let ppid = 0;;";       set "ppid" ps.ppid;
    eval "let pri = 0;;";        set "pri" ps.pri;
    eval "let ni = \"\";;";      set "ni" ps.ni;
    eval "let vsz = 0;;";        set "vsz" ps.vsz;
    eval "let rss = 0;;";        set "rss" ps.rss;
    eval "let wchan = \"\";;";   set "wchan" ps.wchan;
    eval "let stat = \"\";;";    set "stat" ps.stat;
    eval "let tty = \"\";;";     set "tty" ps.tty;
    eval "let time = \"\";;";    set "time" ps.time;
    eval "let command = \"\";;"; set "command" ps.command;
    (* Evaluate expression and return result as boolean. *)
    eval ("let result = (" ^ pred ^ ");;");
    (get "result" : bool)
  
  exception TypeError of string
  exception SyntaxError of string
  
  let preds = List.tl (Array.to_list Sys.argv)
  let () =
    if preds = []
    then (Printf.eprintf "usage: %s criterion ...
      Each criterion is an OCaml expression involving:
       f uid pid ppid pri ni vsz rss wchan stat tty time command
      All criteria must be met for a line to be printed.
  " Sys.argv.(0); exit 0)
  
  let () =
    let proc = Unix.open_process_in "ps wwaxl" in
    try
      print_endline (input_line proc);
      while true do
        let line = input_line proc in
        let ps = parse_ps_line line in
        if List.for_all
          (fun pred ->
             try eval_predicate ps pred
             with e ->
               (* Convert exceptions to strings to avoid depending on
                  additional toplevel libraries. *)
               match Printexc.to_string e with
                 | "Typecore.Error(_, _)" -> raise (TypeError pred)
                 | "Syntaxerr.Error(_)"
                 | "Lexer.Error(1, _)"
                 | "Lexer.Error(_, _)" -> raise (SyntaxError pred)
                 | "Misc.Fatal_error" -> failwith pred
                 | _ -> raise e)
          preds
        then print_endline line
      done
    with
      | End_of_file ->
          ignore (Unix.close_process_in proc)
      | e ->
          ignore (Unix.close_process_in proc);
          raise e
  
  

