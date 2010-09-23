(* ********************************************************************** *)
(* Managing Instance Data *)
(* ********************************************************************** *)
let pleac_Managing_Instance_Data () = 
  (* Using a get and set method. *)
  class person = object
    val mutable name = ""
    method name = name
    method set_name name' = name <- name'
  end
  
  (* Using a single method that does both get and set. *)
  class person = object
    val mutable age = 0
  
    (* Unit argument required due to optional argument. *)
    method age ?set () =
      match set with Some age' -> (age <- age'; age) | None -> age
  end
  
  (* Sample call of get and set: happy birthday! *)
  let () =
    let obj = new person in
    ignore (obj#age ~set:(obj#age () + 1) ())
  
  (* This class converts input when the name is set. *)
  #load "str.cma";;
  class person =
    let funny_chars = Str.regexp ".*[^\n\r\t A-Za-z0-9'-]" in
    let numbers = Str.regexp ".*[0-9]" in
    let not_blank = Str.regexp ".*[^\n\r\t ]" in
    let multiword = Str.regexp ".*[^\n\r\t ]+[\n\r\t ]+[^\n\r\t ]" in
  object
    val mutable name = ""
    method name = name
    method set_name name' =
      if Str.string_match funny_chars name' 0
      then failwith "funny characters in name"
      else if Str.string_match numbers name' 0
      then failwith "numbers in name"
      else if not (Str.string_match not_blank name' 0)
      then failwith "name is blank"
      else if not (Str.string_match multiword name' 0)
      then failwith "prefer multiword name"
      else name <- String.capitalize name'
  end
  
  (* A typical class with attributes and methods. *)
  class person = object
    (* Instance variables *)
    val mutable name = ""
    val mutable age = 0
    val mutable peers = []
  
    (* Accessors *)
    method name = name
    method set_name name' = name <- name'
    method age = age
    method set_age age' = age <- age'
    method peers = peers
    method set_peers peers' = peers <- peers'
  
    (* Behavioral methods *)
    method exclaim () =
      Printf.sprintf "Hi, I'm %s age %d, working with %s"
        name age (String.concat ", " peers)
    method happy_birthday () =
      age <- age + 1
  end
  

