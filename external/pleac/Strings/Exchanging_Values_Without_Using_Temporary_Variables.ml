(* ********************************************************************** *)
(* Exchanging Values Without Using Temporary Variables *)
(* ********************************************************************** *)
let pleac_Exchanging_Values_Without_Using_Temporary_Variables () = 
  (*-----------------------------*)
  let var1, var2 = var2, var1
  (*-----------------------------*)
  let temp    = a
  let a       = b
  let b       = temp
  (*-----------------------------*)
  let a       = "alpha"
  let b       = "omega"
  let a, b = b, a      (* the first shall be last -- and versa vice *)
  (*-----------------------------*)
  let alpha, beta, production = "January", "March", "August"
  (* move beta       to alpha,
   * move production to beta,
   * move alpha      to production *)
  let alpha, beta, production = beta, production, alpha
  (*-----------------------------*)
  

