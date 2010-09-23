(* ********************************************************************** *)
(* Appending One Array to Another *)
(* ********************************************************************** *)
let pleac_Appending_One_Array_to_Another () = 
  
  (* For lists, use the @ operator for two lists, or List.concat for a list of
   * lists, for arrays, use Array.append for two arrays, or Array.concat for a
   * list of arrays*)
  
  let list1 = list1 @ list2;;
  let array1 = Array.append array1 array2;;
  
  let members = [| "Time"; "Flies" |];;
  let initiates = [| "An"; "Arrow" |];;
  let members = Array.append members initiates;;
  
  (* It is easiest to write a splice workalike and then just use the new function
   * much like in Perl *)
  
  let splice ?length ?list arr off =
    let len = Array.length arr in
    let off = if off < 0 then len + off else off in
    let l,back =
      match length with
        None -> (len - off),[||]
      | Some l -> 
          l,
          (let boff = off + l in
          try Array.sub arr boff (len - boff)  with Invalid_argument _ -> [||]) in
    let front = Array.sub arr 0 off
    and mid = 
      match list with 
        None -> [||] 
      | Some a -> a
    and sp = Array.sub arr off l in
    sp,Array.concat [front;mid;back];;
  
  let _,members = 
    splice members 2 ~length:0 ~list:(Array.append [|"Like"|] initiates);;
  Array.iter (printf "%s ") members; print_newline ();;
  
  let _,members = splice members 0 ~length:1 ~list:[|"Fruit"|];;
  let _,members = splice members (-2) ~length:2 ~list:[|"A"; "Banana"|];;
  Array.iter (printf "%s ") members; print_newline ();;
  

