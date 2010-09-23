(* ********************************************************************** *)
(* Generating Random Numbers *)
(* ********************************************************************** *)
let pleac_Generating_Random_Numbers () = 
  (*-----------------------------*)
  let random_int lo hi =
    (Random.int (hi - lo + 1)) + lo;;
  
  let random_float lo hi =
    (Random.float (hi -. lo +. 1.)) +. lo;;
  (*-----------------------------*)
  let random_number = random_int 25 75 in
    printf "%d\n" random_number;;
  (*-----------------------------*)
  let elem = arr.(Random.int (Arry.length arr))
  (*-----------------------------*)
  let uc = Array.init 26 (fun i -> Char.chr (i+ (Char.code 'A')))
  and lc = Array.init 26 (fun i -> Char.chr (i+ (Char.code 'a')))
  and nums = Array.init 10 (fun i -> Char.chr (i + (Char.code '0')))
  and puncs = [| '!'; '@'; '$'; '%'; '^'; '&'; '*' |];;
  let chars = Array.concat [uc; lc; nums; puncs];;
  
  (* to generate the random password as a char array *)
  let password = Array.init 8 (fun i -> chars.(Random.int (Array.length chars)));;
  (* to generate the random password as a string *)
  let passString = 
    let s = String.make 8 ' ' in
    for i=0 to 7 do 
  	s.[i] <- chars.(Random.int (Array.length chars))
    done;
    s;;
  
  (*-----------------------------*)
  
  

