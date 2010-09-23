(* ********************************************************************** *)
(* Using Complex Numbers *)
(* ********************************************************************** *)
let pleac_Using_Complex_Numbers () = 
  (*-----------------------------*)
  (* c = a * b manually *)
  type cplx = { real : float; imag : float; };;
  let c = {real = a.real *. b.real -. a.imag *. b.imag;
           imag = a.imag *. b.real +. b.imag *. a.real};;
  (*-----------------------------*)
  
  (* c = a * b using the Complex module *)
  open Complex;;
  
  let c = Complex.mul a b;;
  (* Note that we could have simply said let c = mul a b, but a later binding of a value to the
     name mul would render the complex mul invisible after that, Complex.mul is
     less ambiguous. *)
  (*-----------------------------*)
  let a = {real=3.; imag=5.};;
  let b = {real=2.; imag=(-. 2.);}
  let c = {real = a.real *. b.real -. a.imag *. b.imag;
           imag = a.imag *. b.real +. b.imag *. a.real};;
  printf "c = %f+%fi\n" c.real c.imag;;
           
  (* c = 16.000000+4.000000i *)
  
  let a = {re=3.; im=5.};;
  let b = {re=2.; im=(-. 2.);}
  let c = mul a b;;
  printf "c = %f+%fi\n" c.re c.im;;
  
  (* c = 16.000000+4.000000i *)
  
  let d = {re=3.; im=4.};;
  let s = sqrt d in
  printf "sqrt(%.2f+%.2fi) = %.2f+%.2fi\n" d.re d.im s.re s.im;;
  
  (* sqrt(3.00+4.00i) = 2.00+1.00i *)
  

