(* ********************************************************************** *)
(* Processing Multiple Elements of an Array *)
(* ********************************************************************** *)
let pleac_Processing_Multiple_Elements_of_an_Array () = 
  
  (* To remove multiple elements from an array at once, one can use the splice
   * function from section 4.9 *)
  
  (* Remove n elements from the front of arr *)
  front,arr = splice arr 0 ~length:n;;
  rear,arr = splice arr (-n);;
  
  (* this can also be wrapped as an explicit function *)
  
  let shift2 a = splice a 0 ~length:2;;
  let pop2 a = splice a (-2);;
  
  (* This lets you do something like Perl's hinkey pattern matching *)
  let friends = [|"Peter"; "Paul"; "Mary"; "Jim"; "Tim" |];;
  let [|this; that|],friends = shift2 friends;;
  
  let beverages = [|"Dew"; "Jolt"; "Cola"; "Sprite"; "Fresca"|];;;
  let pair,beverages =  pop2 beverages;;
  

