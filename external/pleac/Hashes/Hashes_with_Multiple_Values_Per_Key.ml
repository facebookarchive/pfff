(* ********************************************************************** *)
(* Hashes with Multiple Values Per Key *)
(* ********************************************************************** *)
let pleac_Hashes_with_Multiple_Values_Per_Key () = 
  (*-----------------------------*)
  let re = Str.regexp "^\([^ ]*\) *\([^ ]*\)" in
  let lines = readlines (Unix.open_process_in "who") in
  let ttys = filter_some (List.map (fun line ->
    if (Str.string_match re line 0) then
      Some(Str.matched_group 1 line, Str.matched_group 2 line)
    else None) lines) in
  List.iter
    (fun user ->
      printf "%s: %s\n" user (String.concat " " (all_assoc user ttys))
    ) (sort_ (uniq (List.map fst ttys)))
  ;
  (*-----------------------------*)
  List.iter
    (fun user ->
      let ttylist = all_assoc user ttys in
      printf "%s: %d ttys.\n" user (List.length ttylist);
      List.iter
        (fun tty ->
          let uname =
            try
              let uid = (Unix.stat ("/dev/" ^ tty)).Unix.st_uid in
              (Unix.getpwuid uid).Unix.pw_name
            with Unix.Unix_error _ -> "(not available)"
          in
          printf "%s (owned by %s)\n" tty uname
        ) ttylist
    ) (sort_ (uniq (List.map fst ttys)))
  (*-----------------------------*)

