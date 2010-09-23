(* ********************************************************************** *)
(* Detecting Return Context *)
(* ********************************************************************** *)
let pleac_Detecting_Return_Context () = 
  (* OCaml's type safety doesn't allow this kind of shenanigans unless you bring
   * union types into play -- but you still need to ensure that the return type of
   * all three contexts is the same *)
  
  type 'a lORs =
      List of 'a list
    | Scalar of 'a
    | Void of unit ;;
  
  let mysub arg =
    match arg with
      List l -> (* list context, do something with l *)
    | Scalar s -> (* scalar context, do something with s *)
    | Void _ -> (* void context, do something with nothing *);;
  
  (* or equivalently *)
  let mysub = function
      List l -> (* list context, do something with l *)
    | Scalar s -> s (* scalar context, do something with s *)
    | Void _ -> (* void context, do something with nothing *);;
  
  mysub (Void ());;         (* void context *)
  mysub (Scalar arg);;      (* scalar context *)
  mysub (List arg);;        (* list context *)
  

