open Hashtbl

let hash = create 1973
let add_string s = try incr (find hash s) with Not_found -> add hash s (ref 1);;
Pcre.foreach_line (fun line -> List.iter add_string (Pcre.split line));
iter (fun k v -> Printf.printf "%4d\t%s\n" !v k) hash
