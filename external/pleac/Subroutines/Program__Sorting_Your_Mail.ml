(* ********************************************************************** *)
(* Program: Sorting Your Mail *)
(* ********************************************************************** *)
let pleac_Program__Sorting_Your_Mail () = 
  
  let slurp_to_string filename =
    let ic = open_in filename and
    buf = Buffer.create 4096 in
    let rec loop () =
      let line = input_line ic in
      Buffer.add_string buf line;
      Buffer.add_string buf "\n";
      loop () in
    try loop () with End_of_file -> close_in ic; Buffer.contents buf;;
  
  (* Note: The following function does something slightly different than the Perl
   * version, as it returns a subject,message #,refrence to the message tuple
   * sorted by subject -> message number instead of just a list of messages sorted
   * by subject -> message number -- it's trivial to get just what the Perl
   * version does from this... *)
  
  let sortedMail fn =
    let msglist = 
      (* I had to add this filtering step due to some wierd structure in my mbox
       * file. go figure... *)
      List.filter (fun s -> String.sub s 0 5 = "From:")
        (List.map (fun x -> "From" ^ x) 
          (Str.split (Str.regexp "^From") (slurp_to_string fn)))
    and counter = ref (-1) in
  (*  let subjList = *)
      List.sort compare
        (List.map 
          (fun s -> 
            ignore (Str.search_forward 
              (* Not positive this regex is equivalent to the Perl version, but it
               * seems to work -- you can use the third party PCRE module if you
               * want to be positive *)
              (Str.regexp "^Subject:[ \t]*\(:?[Rr][Ee]:[ \t]*\)*\(.*\)") s 0);
            incr counter;
            (try (String.lowercase (Str.matched_group 2 s)) with Not_found -> ""),
             !counter,
             ref s)
          msglist);;
  
  List.iter (fun (_,_,rm) -> print_endline !rm) (sortedMail "mbox");;
  
  (* To sort by using a hashtable *)
  
  let keys h =
    let k = Hashtbl.fold (fun k v b -> k::b) h [] in
    (* filter out duplicates *)
    List.fold_left (fun b x -> if List.mem x b then b else x::b) [] k;;
  
  let sortedMailByHash fn =
    let msglist = 
      (* I had to add this filtering step due to some wierd structure in my mbox
       * file. go figure... *)
      List.filter (fun s -> String.sub s 0 5 = "From:")
        (List.map (fun x -> "From" ^ x) 
          (Str.split (Str.regexp "^From") (slurp_to_string fn)))
    and counter = ref (-1) in
    let h = Hashtbl.create (List.length msglist) in
  (*  let subjList = *)
  (*    List.sort compare *)
        (List.iter 
          (fun s -> 
            ignore (Str.search_forward 
              (* Not positive this regex is equivalent to the Perl version, but it
               * seems to work -- you can use the third party PCRE module if you
               * want to be positive *)
              (Str.regexp "^Subject:[ \t]*\(:?[Rr][Ee]:[ \t]*\)*\(.*\)") s 0);
            incr counter;
            let sub =
              try 
                (String.lowercase (Str.matched_group 2 s)) 
            with Not_found -> "" in
            Hashtbl.add h sub s))
          msglist;
        List.flatten 
          (List.map (fun x -> List.rev (Hashtbl.find_all h x)) 
            (List.sort (keys h)));;
  
  List.iter (fun m -> print_endline m) (sortedMailByHash "mbox");;
  
  

