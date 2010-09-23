(* ********************************************************************** *)
(* Changing Text Color *)
(* ********************************************************************** *)
let pleac_Changing_Text_Color () = 
  (* Requires the ANSITerminal library by Christophe Troestler,
     available at http://math.umh.ac.be/an/software.php#x4-80007 *)
  
  #load "ANSITerminal.cma";;
  open ANSITerminal
  
  let () =
    print_string [red] "Danger Will Robinson!\n";
    print_string [] "This is just normal text.\n";
    print_string [Blink] "<BLINK>Do you hurt yet?</BLINK>\n"
  
  (*-----------------------------*)
  
  let () =
    set_autoreset false;
    (* rhyme for the deadly coral snake *)
    print_string [red; on_black] "venom lack\n";
    print_string [red; on_yellow] "kill that fellow\n";
    print_string [green; on_cyan; Blink] "garish!\n";
    print_string [Reset] ""
  
  (*-----------------------------*)
  
  let () =
    set_autoreset true;
    List.iter
      (print_string [red; on_white; Bold; Blink])
      ["This way\n";
       "each line\n";
       "has its own\n";
       "attribute set.\n"]
  

