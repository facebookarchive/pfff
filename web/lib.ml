
(* color is in format "#rrggbb" *)
let rgb_from_string color = 
  let get_color i = 
    (float_of_string ("0x"^(String.sub color (1+2*i) 2))) /. 255. 
  in
  try get_color 0, get_color 1, get_color 2 
  with _ -> 0.,0.,0.

let memoized ?(use_cache=true) h k f = 
  if not use_cache 
  then f ()
  else 
    try Hashtbl.find h k 
    with Not_found -> 
      let v = f () in
      begin
        Hashtbl.add h k v;
        v
      end
