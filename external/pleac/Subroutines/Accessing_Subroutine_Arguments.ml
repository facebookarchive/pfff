(* ********************************************************************** *)
(* Accessing Subroutine Arguments *)
(* ********************************************************************** *)
let pleac_Accessing_Subroutine_Arguments () = 
  
  (* All values passed to a function must be named in the paramater list to the
   * function *)
  
  let hypotenuse side1 side2 =
    sqrt ((side1 ** 2.) +. (side2 ** 2.));;
  
  (* Note, however, that if the parameters are defined/sent as a tuple then they
   * can be accessed in one of two equivalent ways *)
  
  let hypotenuse (side1,side2) =
    sqrt ((side1 ** 2.) +. (side2 ** 2.));;
  
  let hypotenuse sides =
    let side1,side2 = sides in
    sqrt ((side1 ** 2.) +. (side2 ** 2.));;
  
  (* In both of these cases, however, we must pass the arguments as a tuple *)
  
  print_float hypotenuse (3.,4.);;
  
  (* since most data structures are immutable, one generally does not need to copy
   * the parameters into local variables *)
  
  let nums = [1.4; 3.5; 6.7];;
  let int_all l =
    List.map int_of_float l;;
  
  (*
  # let ints = int_all nums;;
  val ints : int list = [1; 3; 6]
  
  # nums;;
  - : float list = [1.4; 3.5; 6.7]
  *)
  
  (* However, one needs to be careful when mutable data is passed in and
   * operations that alter that data are used *)
  
  let nums = [|1.4; 3.5; 6.7 |];;
  let int_all2 a =
    Array.iteri (fun i x -> a.(i) <- 10. *. x) a;
    a;;
  let int_all3 a = 
    Array.map int_of_float a;;
  
  (*
  # let a2 = int_all2 nums;;
  val a2 : int array = [|1; 3; 6|]
  
  # nums;;
  - : float array = [|1.4; 3.5; 6.7|]
  
  # let a3 = times10 nums;;
  val a3 : float array = [|14.; 35.; 67.|]
  
  # nums;;
  - : float array = [|14.; 35.; 67.|]
  *)
  
  (* To write functions that change their caller's variables, those variables must
   * be mutable structures, such as references *)
  let nums = ref [1.4; 3.5; 6.7];;
  let trunc_em l =
    l:= List.map floor !l;
    !l;;
  
  (*
  
  # let n2 = trunc_em nums;;
  val n2 : float list = [1.; 3.; 6.]
  
  # !nums;;
  - : float list = [1.; 3.; 6.]
  *)
  

