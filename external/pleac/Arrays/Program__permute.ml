(* ********************************************************************** *)
(* Program: permute *)
(* ********************************************************************** *)
let pleac_Program__permute () = 
  
  (* Note: This routine uses the splice routine written in section 4.9 *)
  
  let tsc_permute arr =
    if Array.length arr > 0 then print_endline "Perms:";
    let rec permute arr perms =
      match Array.length arr with
        0 -> Array.iter (printf "%s ") perms; print_newline ();
      | _ ->
          for i = 0 to Array.length arr - 1 do
            let v,ni = splice arr i ~length:1 in
            permute ni (Array.append v perms);
          done in
    permute arr [||];;
  
  (* Note: This example is going to permute the words of a given string - also, I
   * don't feel like bringing in the BigInt module, so we will trim any array
   * longer than 12 elements down to 12 before permuting *)
  
  let fact = Array.append [|Some 1|] (Array.make 11 None);;
  let rec factorial n =
    match fact.(n) with
      Some f -> f
    | None -> let f = n*(factorial (n-1)) in fact.(n) <- Some f; f;;
  
  let n2pat n len =
    let rec nh n i pat =
      if i > len+1 then pat
      else
        nh (n/i) (i+1) ((n mod i)::pat) in
    nh n 1 [];;
  
  let pat2perm pat =
    let rec ph source pat perm =
      match pat with
        [] -> perm
      | h::t ->
          let v,s = splice source h ~length:1 in
          ph s t (v.(0)::perm) in
    Array.of_list (ph (Array.init (List.length pat) (fun i -> i)) pat []);;
    
  let n2perm n len =
    pat2perm (n2pat n len);;
  
  let mjd_permute s =
    let arr = 
      let arr = Array.of_list (Str.split (Str.regexp "[ \t]+") s) in
      try
        Array.sub arr 0 12
      with Invalid_argument _ -> arr in
    let len = Array.length arr - 1 in
    for i = 0 to factorial (len+1) do
      let perm = Array.map (fun i -> arr.(i)) (n2perm i len) in
      Array.iter (printf "%s ") perm; print_newline ();
    done;;
  
  

