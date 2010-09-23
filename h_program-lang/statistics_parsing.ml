open Common 

(* clone with parsing_c/parsing_stat.ml *) 

(*****************************************************************************)
(* Stat *)
(*****************************************************************************)
type parsing_stat = {
    filename: filename;
    mutable have_timeout: bool;

    mutable correct: int;  
    mutable bad: int;

    mutable commentized: int; (* by our cpp commentizer *)

    (* if want to know exactly what was passed through, uncomment:
     *  
     * mutable passing_through_lines: int;
     * 
     * it differs from bad by starting from the error to
     * the synchro point instead of starting from start of
     * function to end of function.
     *)

  } 

let default_stat file =  { 
    filename = file;
    have_timeout = false;
    correct = 0; bad = 0;
    commentized = 0;
  }

(* todo: stat per dir ?  give in terms of func_or_decl numbers:   
 * nbfunc_or_decl pbs / nbfunc_or_decl total ?/ 
 *
 * note: cela dit si y'a des fichiers avec des #ifdef dont on connait pas les 
 * valeurs alors on parsera correctement tout le fichier et pourtant y'aura 
 * aucune def  et donc aucune couverture en fait.   
 * ==> TODO evaluer les parties non parsé ? 
 *)

let print_parsing_stat_list ?(verbose=false) = fun statxs -> 
  let total = (List.length statxs) in
  let perfect = 
    statxs 
      +> List.filter (function 
          {have_timeout = false; bad = 0} -> true | _ -> false)
      +> List.length 
  in

  if verbose then begin 
  pr "\n\n\n---------------------------------------------------------------";
  pr "pbs with files:";
  statxs 
    +> List.filter (function 
      | {have_timeout = true} -> true 
      | {bad = n} when n > 0 -> true 
      | _ -> false)
    +> List.iter (function 
        {filename = file; have_timeout = timeout; bad = n} -> 
          pr (file ^ "  " ^ (if timeout then "TIMEOUT" else i_to_s n));
        );

  pr "\n\n\n";
  pr "files with lots of tokens passed/commentized:";
  let threshold_passed = 100 in
  statxs 
    +> List.filter (function 
      | {commentized = n} when n > threshold_passed -> true
      | _ -> false)
    +> List.iter (function 
        {filename = file; commentized = n} -> 
          pr (file ^ "  " ^ (i_to_s n));
        );

  pr "\n\n\n";
  end;

  let good = statxs +> List.fold_left (fun acc {correct = x} -> acc+x) 0 in
  let bad  = statxs +> List.fold_left (fun acc {bad = x} -> acc+x) 0  in
  let passed = statxs +> List.fold_left (fun acc {commentized = x} -> acc+x) 0
  in
  let total_lines = good + bad in

  pr "---------------------------------------------------------------";
  pr (
  (sprintf "NB total files = %d; " total) ^
  (sprintf "NB total lines = %d; " total_lines) ^
  (sprintf "perfect = %d; " perfect) ^
  (sprintf "pbs = %d; "     (statxs +> List.filter (function 
      {have_timeout = b; bad = n} when n > 0 -> true | _ -> false) 
                               +> List.length)) ^
  (sprintf "timeout = %d; " (statxs +> List.filter (function 
      {have_timeout = true; bad = n} -> true | _ -> false) 
                               +> List.length)) ^
  (sprintf "=========> %d" ((100 * perfect) / total)) ^ "%"
                                                          
 );
  let gf, badf = float_of_int good, float_of_int bad in
  let passedf = float_of_int passed in
  pr (
  (sprintf "nb good = %d,  nb passed = %d " good passed) ^
  (sprintf "=========> %f"  (100.0 *. (passedf /. gf)) ^ "%")
   );
  pr (
  (sprintf "nb good = %d,  nb bad = %d " good bad) ^
  (sprintf "=========> %f"  (100.0 *. (gf /. (gf +. badf))) ^ "%"
   )
  )
