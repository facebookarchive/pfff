(* ********************************************************************** *)
(* Approximate Matching *)
(* ********************************************************************** *)
let pleac_Approximate_Matching () = 
  (* Calculates the Levenshtein, or edit distance, between two strings. *)
  let levenshtein s t =
    let n = String.length s in
    let m = String.length t in
    match (m, n) with
      | (m, 0) -> m
      | (0, n) -> n
      | (m, n) ->
          let d = Array.init (m + 1) (fun x -> x) in
          let x = ref 0 in
          for i = 0 to n - 1 do
            let e = ref (i + 1) in
            for j = 0 to m - 1 do
              let cost = if s.[i] = t.[j] then 0 else 1 in
              x :=
                min
                  (d.(j + 1) + 1)      (* insertion *)
                  (min
                     (!e + 1)          (* deletion *)
                     (d.(j) + cost));  (* substitution *)
              d.(j) <- !e;
              e := !x
            done;
            d.(m) <- !x
          done;
          !x
  
  (* Determines if two strings are an approximate match. *)
  let amatch ?(percentage=20) s t =
    levenshtein s t * 100 / String.length s <= percentage
  
  let () =
    let dict = open_in "/usr/dict/words" in
    try
      while true do
        let word = input_line dict in
        if amatch "balast" word
        then print_endline word
      done
    with End_of_file -> close_in dict
  
  (*
    ballast
    blast
  *)
  

