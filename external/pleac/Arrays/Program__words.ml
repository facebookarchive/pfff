(* ********************************************************************** *)
(* Program: words *)
(* ********************************************************************** *)
let pleac_Program__words () = 
  
  (* Assuming we start with a list of all the data called data, and assuming we
   * already have the curent number of screen columns in a variable cols *)
  
  let words data cols =
    let strippedData = 
      Array.of_list 
        (List.map (Str.replace_first (Str.regexp "[ \t\n]+$") "") data) in
    let maxlen = 
      (Array.fold_left (fun m s -> max m (String.length s)) 0 strippedData) + 1 in
    let cols = if cols < maxlen then 1 else cols / maxlen in
    let rows = ((Array.length strippedData - 1) + cols)/cols in
    let bufs = Array.init rows (fun x -> Buffer.create (cols * maxlen)) in
    for i = 0 to Array.length strippedData - 1 do
      let dst = String.make maxlen ' '
      and src = strippedData.(i) in
      String.blit src 0 dst 0 (String.length src);
      Buffer.add_string bufs.(i mod rows) dst
    done;
    Array.iter (fun x -> print_endline (Buffer.contents x)) bufs;;
  

