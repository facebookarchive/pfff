(* ********************************************************************** *)
(* Multiplying Matrices *)
(* ********************************************************************** *)
let pleac_Multiplying_Matrices () = 
  let mmult m1 m2 =
    let dim m =
      Array.length m,Array.length m.(0) in
    let r1,c1 = dim m1
    and r2,c2 = dim m2 in
    if c1 <> r2 then raise (Invalid_argument "Matrix dimensions don't match")
    else
      begin
        let dotP v1 v2 =
          let sum = ref 0. in
          for i = 0 to Array.length v1 - 1 do 
            sum := !sum +. (v1.(i) *. v2.(i))
          done;
          !sum in
        let row m i = m.(i)
        and col m i = Array.init (Array.length m) (fun r -> m.(r).(i)) in
        let res = Array.make_matrix r1 c2 0. in
        for r = 0 to pred r1 do
          for c = 0 to pred c2 do
            res.(r).(c) <- dotP (row m1 r) (col m2 c)
          done
        done;
        res
      end;;
  

