(* ********************************************************************** *)
(* Passing by Named Parameter *)
(* ********************************************************************** *)
let pleac_Passing_by_Named_Parameter () = 
  (* To name the arguments of a function, use labels *)
  let thefunc ~increment ~finish ~start =
    ... ;;
  
  (* It can be called like *)
  thefunc ~increment:"20s" ~start:"+5m" ~finish:"+30m";;
  
  (* Note that you can use different names for the labels and variables, and if
   * the application is total, the labels can be omitted *)
  let divide ~numerator:x ~denominator:y = 
    x / y;;
  
  (*
  # divide ~denominator:2 ~numerator:100;;
   - : int = 50
  
  # divide 20 4;;
   - : int = 5
  *)
  
  (* If you want to provide default values, you need to use optional arguments,
   * but this requires at least one unlabelled argument *)
  
  let fraction ?(y = 2) x =
    x / y;;
  
  (*
  fraction 30 ~y:3;;
   - : int = 10
  
  fraction 30;;
   - : int = 15
  *)
  

