(* ********************************************************************** *)
(* Introduction *)
(* ********************************************************************** *)
let pleac_Introduction () = 
  (* Create a reference to an integer *)
  let aref = ref 0
  
  let () =
    (* Assign to aref's contents *)
    aref := 3;
  
    (* Print the value that the reference "aref" refers to *)
    Printf.printf "%d\n" !aref;
  
    (* Since references are just records with a single field, "contents",
       the following operations have the same effect as above *)
    aref.contents <- 3;
    Printf.printf "%d\n" aref.contents;
  
    (* Fast increment and decrement operations are available for int refs *)
    incr aref; Printf.printf "after incr: %d\n" !aref;
    decr aref; Printf.printf "after decr: %d\n" !aref
  
  (* Create a type for "person" records *)
  type person = { name : string;
                  address : string;
                  birthday : int;
                }
  
  let () =
    (* Create a "person" record *)
    let nat = { name     = "Leonhard Euler";
                address  = "1729 Ramunjan Lane\nMathword, PI 31416";
                birthday = 0x5bb5580;
              } in
  
    (* Display the person's name and address *)
    Printf.printf "\nname: %s\naddress: %s\n" nat.name nat.address;
  
    (* Same as above, using pattern-matching *)
    let {name=n; address=a} = nat in
    Printf.printf "\nname: %s\naddress: %s\n" n a
  

