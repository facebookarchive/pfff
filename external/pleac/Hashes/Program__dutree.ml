(* ********************************************************************** *)
(* Program: dutree *)
(* ********************************************************************** *)
let pleac_Program__dutree () = 
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* dutree - print sorted indented rendition of du output *)
  #load "str.cma";;
  #load "unix.cma";;
  
  let dirsize = Hashtbl.create 0
  let kids = Hashtbl.create 0
  
  (* run du, read in input, save sizes and kids *)
  (* return last directory (file?) read *)
  let input () =
    let last_name = ref "" in
    let last_push = ref None in
    let argv = "du" :: List.tl (Array.to_list Sys.argv) in
    let ch = Unix.open_process_in (String.concat " " argv) in
    begin
      try
        while true do
          let line = input_line ch in
          match Str.bounded_split (Str.regexp "[ \t]+") line 2 with
            | [size; name] ->
                let size = int_of_string size in
                Hashtbl.replace dirsize name size;
                let parent =
                  Str.replace_first (Str.regexp "/[^/]+$") "" name in
                last_name := name;
                last_push :=
                  Some (parent,
                        try Some (Hashtbl.find kids parent)
                        with Not_found -> None);
                Hashtbl.replace kids parent
                  (name ::
                     (try Hashtbl.find kids parent
                      with Not_found -> []))
            | _ -> failwith line
        done
      with End_of_file ->
        ignore (Unix.close_process_in ch)
    end;
    begin
      match !last_push with
        | None -> ()
        | Some (parent, None) ->
            Hashtbl.remove kids parent
        | Some (parent, Some previous) ->
            Hashtbl.replace kids parent previous
    end;
    !last_name
  
  (* figure out how much is taken up in each directory *)
  (* that isn't stored in subdirectories.  add a new *)
  (* fake kid called "." containing that much. *)
  let rec getdots root =
    let size = Hashtbl.find dirsize root in
    let cursize = ref size in
    if Hashtbl.mem kids root
    then
      begin
        List.iter
          (fun kid ->
             cursize := !cursize - Hashtbl.find dirsize kid;
             getdots kid)
          (Hashtbl.find kids root)
      end;
    if size <> !cursize
    then
      begin
        let dot = root ^ "/." in
        Hashtbl.replace dirsize dot !cursize;
        Hashtbl.replace kids root
          (dot ::
             (try Hashtbl.find kids root
              with Not_found -> []))
      end
  
  (* recursively output everything, *)
  (* passing padding and number width in as well *)
  (* on recursive calls *)
  let rec output ?(prefix="") ?(width=0) root =
    let path = Str.replace_first (Str.regexp ".*/") "" root in
    let size = Hashtbl.find dirsize root in
    let line = Printf.sprintf "%*d %s" width size path in
    Printf.printf "%s%s\n" prefix line;
    let prefix =
      Str.global_replace (Str.regexp "[^|]") " "
        (Str.replace_first (Str.regexp "[0-9] ") "| "
           (prefix ^ line)) in
    if Hashtbl.mem kids root
    then
      begin
        let kids = Hashtbl.find kids root in
        let kids =
          List.rev_map
            (fun kid -> (Hashtbl.find dirsize kid, kid)) kids in
        let kids = List.sort compare kids in
        let kids = List.rev_map (fun (_, kid) -> kid) kids in
        let width =
          String.length
            (string_of_int (Hashtbl.find dirsize (List.hd kids))) in
        List.iter (output ~prefix ~width) kids
      end
  
  let () =
    let topdir = input () in
    getdots topdir;
    output topdir
  
  

