(* ********************************************************************** *)
(* Making Variables Private to a Function *)
(* ********************************************************************** *)
let pleac_Making_Variables_Private_to_a_Function () = 
  
  (* to declare a variable local to a function, simply use let inside the function
   * body *)
  
  let somefunc () =
    let variable = ... in
    let another,anarray,ahash = ... in
    ... ;;
  
  let check_x x =
    let y = "whatever" in
    run_check ();
    if condition then printf "got %s" x;;
  
  let save_array arguments =
    global_list := arguments @ !global_list;;
  

