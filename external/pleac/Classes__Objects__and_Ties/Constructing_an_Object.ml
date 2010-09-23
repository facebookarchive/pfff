(* ********************************************************************** *)
(* Constructing an Object *)
(* ********************************************************************** *)
let pleac_Constructing_an_Object () = 
  #load "unix.cma";;
  
  class klass args = object (self)
    val mutable start = 0.
    val mutable age = 0
    val extra = Hashtbl.create 0
  
    (* Private method to initialize fields. Sets start to
       the current time, and age to 0. If called with arguments,
       init interprets them as key+value pairs to initialize the
       hashtable "extra" with. *)
    method private init () =
      start <- Unix.time ();
      List.iter
        (fun (k, v) -> Hashtbl.replace extra k v)
        args
  
    initializer
      self#init ()
  end
  

