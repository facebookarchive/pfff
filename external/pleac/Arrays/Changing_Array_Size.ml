(* ********************************************************************** *)
(* Changing Array Size *)
(* ********************************************************************** *)
let pleac_Changing_Array_Size () = 
  
  (*
     OK, OCaml just doesn't work with arrays the same way tha Perl does.  In
     Ocaml, Arrays are immutable in their shape, while containing mutable
     contents.  You can simulate this example as shown below (which only works for
     string arrays), or you can get resizeable arrays from a library such as
     extlib <http://ocaml-lib.sourceforge.net/>
  *)
  
  let what_about_that_array a =
    let len = Array.length a in
    printf "The array now has %d elements.\n" len;
    printf "The index of the last element is %d.\n" (if len=0 then 0 else len-1);
    printf "Element 3 is \"%s\".\n" a.(3);; 
  
  let resizeArray a s =
    (* begin stupid hack to work like the Perl example *)
    let s = s + 1 in
    (* end stupid hack to work like the Perl example *)
    assert (s >= 0);
    let len = Array.length a in
    if s = len then a else
      if s < len then
        Array.sub a 0 s
      else
        Array.append a (Array.make (s - len) "");;
  
  let people = [|"Crosby"; "Stills"; "Nash"; "Young"|];;
  what_about_that_array people;;
  
  (*
  The array now has 4 elements.
  The index of the last element is 3.
  Element 3 is "Young".
  *)
  
  let people = resizeArray people 2;;
  what_about_that_array people;;
  
  (*
  The array now has 3 elements.
  The index of the last element is 2.
  Exception: Invalid_argument "index out of bounds".
  *)
  
  let people = resizeArray people 10000;;
  what_about_that_array people;;
  (*
  The array now has 10001 elements.
  The index of the last element is 10000.
  Element 3 is "".
  *)
  

