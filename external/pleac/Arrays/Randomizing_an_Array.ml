(* ********************************************************************** *)
(* Randomizing an Array *)
(* ********************************************************************** *)
let pleac_Randomizing_an_Array () = 
  
  let fisher_yates_shuffle a =
    for i = Array.length a - 1 downto 1 do
      let x = a.(i)
      and r = Random.int (i+1) in
      a.(i) <- a.(r);
      a.(r) <- x;
    done;;
  

