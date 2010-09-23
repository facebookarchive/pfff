(* ********************************************************************** *)
(* Introduction *)
(* ********************************************************************** *)
let pleac_Introduction () = 
  (* The simplest object possible. This object has no data, no methods,
     and does not belong to any class. *)
  let obj = object end
  
  (* The simplest class possible, and an instance of it. *)
  class encoder = object end
  let obj = new encoder
  
  (* A class living inside of a module. *)
  module Data = struct
    class encoder = object end
  end
  let obj = new Data.encoder
  
  (* An object with data and a method. *)
  let obj = object
    val data = [3; 5]
    method at n = List.nth data n
  end
  let () =
    (* Display the object's identity (an integer) and call a method. *)
    Printf.printf "%d %d\n" (Oo.id obj) (obj#at 1)
  
  (* A module containing a class with data and a method. *)
  module Human = struct
    class ['a] cannibal data = object
      val data : 'a list = data
      method at n = List.nth data n
    end
  end
  let () =
    let obj = new Human.cannibal [3; 5] in
    Printf.printf "%d %d\n" (Oo.id obj) (obj#at 1)
  
  (* Method calls are indicated by the '#' operator. *)
  let encoded = obj#encode "data"
  
  (* There is no notion of a class method in OCaml.
     Use a module-level function instead. *)
  let encoded = Data.Encoder.encode "data"
  
  (* Using the "class" keyword is much like defining a function. *)
  class klass (initial_name : string) = object
    val mutable my_name = initial_name
    method name = my_name
    method set_name name = my_name <- name
  end
  let () =
    let obj = new klass "Class" in
    print_endline obj#name;
    obj#set_name "Clown";
    print_endline obj#name
  
  (* Initialization can be performed prior to object creation. *)
  class random n =
    let rng = Random.State.make_self_init () in
  object
    method next () = Random.State.int rng n
  end
  let () =
    let r = new random 10 in
    Printf.printf "Three random numbers: %d, %d, %d.\n"
      (r#next ()) (r#next ()) (r#next ())
  
  (* Initialization can also be performed after object creation.
     Note the "self" parameter, which can be used much like the
     "this" reference in other OO languages. *)
  class late_initializer name = object (self)
    val my_name = name
    method prepare_name () = String.capitalize my_name
    initializer Printf.printf "%s is ready\n" (self#prepare_name ())
  end
  let obj = new late_initializer "object"
  
  (* Methods are curried just like functions. This allows them to
     be used just like functions in many cases. It is customary for
     methods to take at least one argument (even if it is unit)
     unless they represent an object's attribute. *)
  module Human = struct
    class cannibal (name : string) = object
      val mutable name = name
      method name = name
      method feed who = print_endline ("Feeding " ^ who)
      method move where = print_endline ("Moving to " ^ where)
      method die () = print_endline "Dying"
    end
  end
  let () =
    let lector = new Human.cannibal "Hannibal" in
    let feed, move, die = lector#feed, lector#move, lector#die in
    Printf.printf "Cannibal's name is %s\n" lector#name;
    feed "Zak";
    move "New York";
    die ()
  

