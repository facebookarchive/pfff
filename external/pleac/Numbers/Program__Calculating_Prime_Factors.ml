(* ********************************************************************** *)
(* Program: Calculating Prime Factors *)
(* ********************************************************************** *)
let pleac_Program__Calculating_Prime_Factors () = 
  (* Note: the OS needs to support dynamic loading of C libraries for this
     otherwise you will need to link the nums library with the code at comple time *)
  #load "nums.cma";;
  open Big_int;;
  
  let cmd = [|"bigfact"; "8"; "9"; "96"; "2178"; 
              "239322000000000000000000"; "25000000000000000000000000"; "17"|];;
  
  (* This will raise an exception if a nonnumeric string is in the argument list
  *)
  let argList = 
    Array.map big_int_of_string (Array.sub cmd 1 ((Array.length cmd) - 1));;
  
  let factorize num = 
    let two = big_int_of_int 2 and four = big_int_of_int 4 in
    let rec genFactors (i,sqi) n fList =
      if eq_big_int n unit_big_int then fList else
      if lt_big_int n sqi then ((n,1)::fList) else
        let newn = ref n and fcount = ref 0 in
        while  (eq_big_int (mod_big_int !newn i) zero_big_int) do
            newn := div_big_int !newn i;
            fcount := !fcount + 1;
        done;
        let nexti,nextsqi = 
            if eq_big_int i two then
                (add_big_int i unit_big_int),
                  (add_big_int sqi (add_big_int (mult_big_int i two)
                   unit_big_int))
            else
                (add_big_int i two),
                  (add_big_int sqi (add_big_int (mult_big_int i four) two)) in
        genFactors (nexti,nextsqi) !newn (if !fcount = 0 then fList else
            ((i,!fcount)::fList)) in
     genFactors (two,four) num [];;
  
  let _ = 
    Array.iter
    (fun n ->
      let l = factorize n in
      match l with
        [(x,1)] -> printf "%s\tPrime!\n" (string_of_big_int x)
      | _ -> 
          printf "%s\t" (string_of_big_int n);
          List.iter
            (fun (x,count) -> let sx = string_of_big_int x in
              if count = 1 then printf "%s " sx
              else printf "%s**%d " sx count)
            (List.rev l);
      print_newline()) argList;;
  
  

