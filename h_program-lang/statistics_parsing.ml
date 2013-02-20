open Common 

(* clone with parsing_c/parsing_stat.ml *) 

(*****************************************************************************)
(* Types *)
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

  (* for instance to report most problematic macros when parse c/c++ *)
  mutable problematic_lines:
    (string list (* ident in error line *) * int (* line_error *)) list;
  
} 

let default_stat file =  { 
  filename = file;
  have_timeout = false;
  correct = 0; bad = 0;
  commentized = 0;
  problematic_lines = [];
}

(*****************************************************************************)
(* Stat *)
(*****************************************************************************)

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
          {have_timeout = false; bad = 0; _} -> true | _ -> false)
      +> List.length 
  in

  if verbose then begin 
  pr "\n\n\n---------------------------------------------------------------";
  pr "pbs with files:";
  statxs 
    +> List.filter (function 
      | {have_timeout = true; _} -> true 
      | {bad = n; _} when n > 0 -> true 
      | _ -> false)
    +> List.iter (function 
        {filename = file; have_timeout = timeout; bad = n; _} -> 
          pr (file ^ "  " ^ (if timeout then "TIMEOUT" else i_to_s n));
        );

  pr "\n\n\n";
  pr "files with lots of tokens passed/commentized:";
  let threshold_passed = 100 in
  statxs 
    +> List.filter (function 
      | {commentized = n; _} when n > threshold_passed -> true
      | _ -> false)
    +> List.iter (function 
        {filename = file; commentized = n; _} -> 
          pr (file ^ "  " ^ (i_to_s n));
        );

  pr "\n\n\n";
  end;

  let good = statxs +> List.fold_left (fun acc {correct = x; _} -> acc+x) 0 in
  let bad  = statxs +> List.fold_left (fun acc {bad = x; _} -> acc+x) 0  in
  let passed = statxs +> List.fold_left (fun acc {commentized = x; _} -> acc+x) 0
  in
  let total_lines = good + bad in

  pr "---------------------------------------------------------------";
  pr (
  (spf "NB total files = %d; " total) ^
  (spf "NB total lines = %d; " total_lines) ^
  (spf "perfect = %d; " perfect) ^
  (spf "pbs = %d; "     (statxs +> List.filter (function 
      {have_timeout = b; bad = n; _} when n > 0 -> true | _ -> false) 
                               +> List.length)) ^
  (spf "timeout = %d; " (statxs +> List.filter (function 
      {have_timeout = true; bad = n; _} -> true | _ -> false) 
                               +> List.length)) ^
  (spf "=========> %d" ((100 * perfect) / total)) ^ "%"
                                                          
 );
  let gf, badf = float_of_int good, float_of_int bad in
  let passedf = float_of_int passed in
  pr (
  (spf "nb good = %d,  nb passed = %d " good passed) ^
  (spf "=========> %f"  (100.0 *. (passedf /. gf)) ^ "%")
   );
  pr (
  (spf "nb good = %d,  nb bad = %d " good bad) ^
  (spf "=========> %f"  (100.0 *. (gf /. (gf +. badf))) ^ "%"
   )
  )

(*****************************************************************************)
(* Most problematic tokens *)
(*****************************************************************************)

(* inspired by a comment by a reviewer of my CC'09 paper *)
let lines_around_error_line ~context (file, line) =
  let arr = Common2.cat_array file in

  let startl = max 0 (line - context) in
  let endl   = min (Array.length arr) (line + context) in
  let res = ref [] in

  for i = startl to endl -1 do
    Common.push2 arr.(i) res
  done;
  List.rev !res

let print_recurring_problematic_tokens xs =
  let h = Hashtbl.create 101 in
  xs +> List.iter (fun x ->
    let file = x.filename in
    x.problematic_lines +> List.iter (fun (xs, line_error) ->
      xs +> List.iter (fun s ->
        Common2.hupdate_default s
          (fun (old, example)  -> old + 1, example)
          (fun() -> 0, (file, line_error)) h;
      )));
  Common2.pr2_xxxxxxxxxxxxxxxxx();
  pr2 ("maybe 10 most problematic tokens");
  Common2.pr2_xxxxxxxxxxxxxxxxx();
  Common.hash_to_list h
  +> List.sort (fun (k1,(v1,_)) (k2,(v2,_)) -> compare v2 v1)
  +> Common.take_safe 10
  +> List.iter (fun (k,(i, (file_ex, line_ex))) ->
    pr2 (spf "%s: present in %d parsing errors" k i);
    pr2 ("example: ");
    let lines = lines_around_error_line ~context:2 (file_ex, line_ex) in
    lines +> List.iter (fun s -> pr2 ("       " ^ s));
  );
  Common2.pr2_xxxxxxxxxxxxxxxxx();
  ()
