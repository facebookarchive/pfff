(* ********************************************************************** *)
(* Program: Small termcap program *)
(* ********************************************************************** *)
let pleac_Program__Small_termcap_program () = 
  #!/usr/bin/ocaml
  
  #directory "+curses";;
  #load "curses.cma";;
  #load "unix.cma";;
  
  let delay = 0.005
  
  (* Bounce lines around the screen until the user interrupts with
     Ctrl-C. *)
  let zip () =
    Curses.clear ();
    let maxcol, maxrow = Curses.get_size () in
  
    let chars = ref ['*'; '-'; '/'; '|'; '\\'; '_'] in
    let circle () = chars := List.tl !chars @ [List.hd !chars] in
  
    let row, col = ref 0, ref 0 in
    let row_sign, col_sign = ref 1, ref 1 in
  
    while true do
      ignore (Curses.mvaddch !col !row (Char.code (List.hd !chars)));
      ignore (Curses.refresh ());
      (try ignore (Unix.select [] [] [] delay) with _ -> ());
      row := !row + !row_sign;
      col := !col + !col_sign;
      if !row = maxrow then (row_sign := -1; circle ())
      else if !row = 0 then (row_sign :=  1; circle ());
      if !col = maxcol then (col_sign := -1; circle ())
      else if !col = 0 then (col_sign :=  1; circle ())
    done
  
  let () =
    ignore (Curses.initscr ());
    at_exit Curses.endwin;
    zip ()
  

