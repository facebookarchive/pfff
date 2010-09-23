(* ********************************************************************** *)
(* Making Hashes of Arrays *)
(* ********************************************************************** *)
let pleac_Making_Hashes_of_Arrays () = 
  (* Create a hash that maps strings to string lists *)
  let (hash : (string, string list) Hashtbl.t) = Hashtbl.create 0
  
  (* Define a function to add a string to the string list associated
     with a key in the hash creating the string list if necessary *)
  let add hash key value =
    Hashtbl.replace hash key
      (try value :: Hashtbl.find hash key
       with Not_found -> [value])
  
  let () =
    (* Populate the hash with some data *)
    add hash "fruit" "apple";
    add hash "fruit" "banana";
    add hash "wine" "merlot";
    add hash "cheese" "cheddar";
    add hash "cheese" "brie";
    add hash "cheese" "havarti";
  
    (* Iterate and print out the hash's contents *)
    Hashtbl.iter
      (fun key values ->
         Printf.printf "%s: %s\n" key
           (String.concat ", " values))
      hash
  
  (* Hashtbl is somewhat unusual in that it allows multiple values for
     a given key. By using Hashtbl.add instead of Hashtbl.replace, and
     using strings as values instead of string lists, we can save some
     memory *)
  let (hash : (string, string) Hashtbl.t) = Hashtbl.create 0
  let () =
    Hashtbl.add hash "foo" "bar";
    Hashtbl.add hash "foo" "baz";
    Hashtbl.add hash "goo" "arc";
    Hashtbl.iter (Printf.printf "%s => %s\n") hash
  

