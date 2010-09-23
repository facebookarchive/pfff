(* ********************************************************************** *)
(* Using Classes as Structs *)
(* ********************************************************************** *)
let pleac_Using_Classes_as_Structs () = 
  (* Immediate objects can be used like records, and their types are
     inferred automatically. Unlike with records, object fields names
     do not have to be unique to a module, which can be convenient. *)
  let p = object
    method name = "Jason Smythe"
    method age = 13
    method peers = [| "Wilbur"; "Ralph"; "Fred" |]
  end
  (* val p : < age : int; name : string; peers : string array > = <obj> *)
  
  (* Fetch various values, including the zeroth friend. *)
  let () =
    Printf.printf "At age %d, %s's first friend is %s.\n"
      p#age p#name p#peers.(0)
  (* At age 13, Jason Smythe's first friend is Wilbur. *)
  
  (*---------------------------*)
  
  (* Immediate objects can be nested. *)
  let folks = object
    method head = object
      method name = "John"
      method age = 34
    end
  end
  (* val folks : < head : < age : int; name : string > > = <obj> *)
  let () =
    Printf.printf "%s's age is %d\n"
      folks#head#name folks#head#age
  (* John's age is 34 *)
  
  (*---------------------------*)
  
  (* If you want to maintain an invariant, it's better to use a class. *)
  exception Unreasonable_age of int
  class person init_name init_age = object (self)
    val mutable name = ""
    val mutable age = 0
    method name = name
    method age = age
    method set_name name' = name <- name'
    method set_age age' =
      if age' > 150 then raise (Unreasonable_age age') else age <- age'
    initializer
      self#set_name init_name;
      self#set_age init_age
  end
  

