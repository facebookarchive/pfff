(* ********************************************************************** *)
(* Caching Open Output Filehandles *)
(* ********************************************************************** *)
let pleac_Caching_Open_Output_Filehandles () = 
  module FileCache = struct
    let isopen = Hashtbl.create 0
    let maxopen = ref 16
  
    let resize () =
      if Hashtbl.length isopen >= !maxopen
      then
        begin
          let newlen = !maxopen / 3 in
          let items = ref [] in
          Hashtbl.iter
            (fun filename (chan, count) ->
               items := (count, filename, chan) :: !items)
            isopen;
          let items = Array.of_list !items in
          Array.sort compare items;
          let pivot = Array.length items - newlen in
          for i = 0 to Array.length items - 1 do
            let (count, filename, chan) = items.(i) in
            if i < pivot
            then (close_out chan;
                  Hashtbl.remove isopen filename)
            else (Hashtbl.replace isopen filename (chan, 0))
          done
        end
  
    let output ?(mode=[Open_creat; Open_append]) ?(perm=0o640) filename data =
      let (chan, count) =
        try Hashtbl.find isopen filename
        with Not_found ->
          resize ();
          (open_out_gen mode perm filename, 0) in
      output_string chan data;
      flush chan;
      Hashtbl.replace isopen filename (chan, count + 1)
  
    let close filename =
      try
        match Hashtbl.find isopen filename with (chan, _) ->
          close_out chan;
          Hashtbl.remove isopen filename
      with Not_found -> ()
  end
  
  (*-----------------------------*)
  
  (* splitwulog - split wuftpd log by authenticated user *)
  #load "str.cma";;
  let outdir = "/var/log/ftp/by-user"
  let regexp = Str.regexp " "
  let () =
    try
      while true do
        let line = input_line stdin in
        let chunks = Array.of_list (Str.split regexp line) in
        let user = chunks.(Array.length chunks - 5) in
        let path = Filename.concat outdir user in
        FileCache.output path (line ^ "\n")
      done
    with End_of_file -> ()
  

