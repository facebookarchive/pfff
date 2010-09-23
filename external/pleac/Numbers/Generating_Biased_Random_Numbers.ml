(* ********************************************************************** *)
(* Generating Biased Random Numbers *)
(* ********************************************************************** *)
let pleac_Generating_Biased_Random_Numbers () = 
  (* Note that this will return just one of the numbers, as returning either one
  * or the other would requires always constructing an array or a list -- this
  * just returns a float *)
  
  let gaussianRand () =
    let rec getW () =
      let u1 = 2. *. (Random.float 1.) -. 1.
      and u2 = 2. *. (Random.float 1.) -. 1. in
      let w = u1 *. u1 +. u2 *. u2 in
      if w >= 0. then w,u1,u2 else getW () in
    let w,u1,u2 = getW () in
    let w = sqrt((-2. *. (log w)) /. w) in
    let g2 = u1 *. w
    and g1 = u2 *. w in
    g1;; 
  
  
  (* note that because of the way dist is used, it makes the most sense to return
  * it as a sorted associative list rather than another hash table *)
  let weightToDist whash =
    let total = Hashtbl.fold (fun k v b -> b +. v) whash 0. in
    let dist = Hashtbl.fold (fun k v b -> (v,k)::b) whash [] in
    List.sort compare dist;;
  
  let rec weightedRand dhash =
    let r = ref (Random.float 1.) in
    try 
      let v,k = List.find (fun (v,k) -> r := !r -. v; !r < 0.) dhash in k
    with Not_found -> weightedRand dhash;;  
  
  let mean,dev = 25.,2. in
  let salary = gaussianRand () *. sdev +. mean;;
  printf "You have been hired at $%.2f\n" salary;;
  

