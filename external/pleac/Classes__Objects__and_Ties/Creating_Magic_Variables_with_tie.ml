(* ********************************************************************** *)
(* Creating Magic Variables with tie *)
(* ********************************************************************** *)
let pleac_Creating_Magic_Variables_with_tie () = 
  (* OCaml does not have anything like Perl's "tie" feature; you can't
     make an identifier evaluate to anything other than itself. Since
     "tie" is just syntax sugar anyway, all of the examples can be done
     with regular classes and objects. *)
  
  class ['a] value_ring values = object
    val mutable values = (values : 'a list)
    method get =
      match values with
        | h :: t -> values <- t @ [h]; h
        | [] -> raise Not_found
    method add value =
      values <- value :: values
  end
  
  (*-----------------------------*)
  
  let () =
    let colors = new value_ring ["red"; "blue"] in
    Printf.printf "%s %s %s %s %s %s\n"
      colors#get colors#get colors#get
      colors#get colors#get colors#get;
    (* blue red blue red blue red *)
  
    colors#add "green";
    Printf.printf "%s %s %s %s %s %s\n"
      colors#get colors#get colors#get
      colors#get colors#get colors#get
    (* blue red green blue red green *)
  
  (*-----------------------------*)
  
  (* Magic hash that autoappends. *)
  
  class ['a, 'b] append_hash size = object
    val hash = (Hashtbl.create size : ('a, 'b) Hashtbl.t)
    method get k = Hashtbl.find hash k
    method set k v =
      Hashtbl.replace hash k
        (try v :: Hashtbl.find hash k with Not_found -> [v])
    method each f = Hashtbl.iter f hash
  end
  
  (*-----------------------------*)
  
  let () =
    let tab = new append_hash 3 in
    tab#set "beer" "guinness";
    tab#set "food" "potatoes";
    tab#set "food" "peas";
    tab#each
      (fun k vs ->
         Printf.printf "%s => [%s]\n"
           k (String.concat " " (List.rev vs)))
  
  (*-----------------------------*)
  
  (*
    beer => [guinness]
    food => [potatoes peas]
  *)
  
  (*-----------------------------*)
  
  (* For a more lightweight syntax, you can override the .{}
     operator--which normally works with Bigarrays--to work on
     any object with "get" and "set" methods. *)
  
  module Bigarray = struct
    module Array1 = struct
      let get obj = obj#get
      let set obj = obj#set
    end
  end
  
  let () =
    let tab = new append_hash 3 in
    tab.{"beer"} <- "guinness";
    tab.{"food"} <- "potatoes";
    tab.{"food"} <- "peas";
    tab#each
      (fun k vs ->
         Printf.printf "%s => [%s]\n"
           k (String.concat " " (List.rev vs)));
    print_endline (List.hd tab.{"beer"})
  
  (*-----------------------------*)
  
  (* Hash that magically folds case. *)
  
  class ['a] folded_hash size = object
    val hash = (Hashtbl.create size : (string, 'a) Hashtbl.t)
    method get k = Hashtbl.find hash (String.lowercase k)
    method set k v = Hashtbl.replace hash (String.lowercase k) v
    method each f = Hashtbl.iter f hash
  end
  
  (*-----------------------------*)
  
  let () =
    let tab = new folded_hash 2 in
    tab.{"VILLAIN"} <- "big ";
    tab.{"herOine"} <- "red riding hood";
    tab.{"villain"} <- tab.{"villain"} ^ "bad wolf";
    tab#each (Printf.printf "%s is %s\n")
  
  (*
    heroine is red riding hood
    villain is big bad wolf
  *)
  
  (*-----------------------------*)
  
  (* Hash that permits key *or* value lookups. *)
  class ['a] rev_hash size = object
    val hash = (Hashtbl.create size : ('a, 'a) Hashtbl.t)
    method get k = Hashtbl.find hash k
    method set k v =
      Hashtbl.replace hash k v;
      Hashtbl.replace hash v k
    method each f = Hashtbl.iter f hash
  end
  
  (*-----------------------------*)
  
  let () =
    let tab = new rev_hash 8 in
    tab.{`Str "Red"} <- `Str "Rojo";
    tab.{`Str "Blue"} <- `Str "Azul";
    tab.{`Str "Green"} <- `Str "Verde";
    tab.{`Str "EVIL"} <- `StrList [ "No way!"; "Way!!" ];
    let to_string = function
      | `Str s -> s
      | `StrList ss -> "[" ^ String.concat " " ss ^ "]" in
    tab#each
      (fun k v ->
         Printf.printf "%s => %s\n" (to_string k) (to_string v))
  
  (*
    Verde => Green
    Azul => Blue
    Green => Verde
    Blue => Azul
    Red => Rojo
    [No way! Way!!] => EVIL
    EVIL => [No way! Way!!]
    Rojo => Red
  *)
  
  (*-----------------------------*)
  
  (* Simple counter. *)
  
  class counter start = object
    val mutable value = (start : int)
    method next = value <- value + 1; value
  end
  
  let () =
    let c = new counter 0 in
    while true do
      Printf.printf "Got %d\n" c#next
    done
  
  (*-----------------------------*)
  
  (* Tee-like class that outputs to multiple channels at once. *)
  
  class tee channels = object
    method print s = List.iter (fun ch -> output_string ch s) channels
  end
  
  let () =
    let tee = new tee [stdout; stderr] in
    tee#print "This line goes to both places.\n";
    flush_all ()
  
  let () =
    let tee = new tee
      (stdout ::
         (Array.to_list
            (Array.init 10
               (fun _ ->
                  snd (Filename.open_temp_file "teetest." ""))))) in
    tee#print "This lines goes many places.\n";
    flush_all ()
  
  

