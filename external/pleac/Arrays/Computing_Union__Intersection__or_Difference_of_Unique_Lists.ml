(* ********************************************************************** *)
(* Computing Union, Intersection, or Difference of Unique Lists *)
(* ********************************************************************** *)
let pleac_Computing_Union__Intersection__or_Difference_of_Unique_Lists () = 
  
  let a = [ 1;3;5;6;7;8 ];;
  let b = [ 2;3;5;7;9 ];;
  
  let union = Hashtbl.create 13
  and isect = Hashtbl.create 13
  and diff = Hashtbl.create 13;;
  
  (* simple solution for union and intersection *)
  List.iter (fun x -> Hashtbl.add union x 1) a;;
  List.iter 
    (fun x -> hashtbl.add (if Hashtbl.mem union x then isect else union) x 1) b;;
  let u = Hashtbl.fold (fun k v b -> k::b) union []
  and i = Hashtbl.fold (fun k v b -> k::b) isect [];;
  
  (* Union, intersection, and symmetric difference *)
  let hincr h x = 
    let v = try Hashtbl.find h x with Not_found -> 0 in
    Hashtbl.replace h x (v+1);;
  
  let count = Hashtbl.create 13;;
  List.iter (fun x -> Hashtbl.add count x 1) a;;
  List.iter (hincr count) b;;
  let u,i,d =
    let u = Hashtbl.fold (fun k v b -> (k,v)::b) count [] in
    let i,d = List.partition(fun x -> snd x = 2) u in
    let vo l = List.map fst l in
    (vo u),(vo i),(vo d);;
  
  

