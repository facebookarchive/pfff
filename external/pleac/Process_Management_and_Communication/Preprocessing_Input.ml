(* ********************************************************************** *)
(* Preprocessing Input *)
(* ********************************************************************** *)
let pleac_Preprocessing_Input () = 
  #load "unix.cma";;
  #load "str.cma";;
  
  (* Tagged filename or URL type. *)
  type filename =
    | Uncompressed of string
    | Compressed of string
    | URL of string
  
  (* try/finally-like construct to ensure we dispose of resources properly. *)
  let finally handler f x =
    let result = try f x with e -> handler (); raise e in handler (); result
  
  (* Call f with an in_channel given a tagged filename. If the filename is
     tagged Uncompressed, open it normally. If it is tagged Compressed then
     pipe it through gzip. If it is tagged URL, pipe it through "lynx -dump".
     Ensure that the channel is closed and any created processes have
     terminated before returning. As a special case, a filename of
     Uncompressed "-" will result in stdin being passed, and no channel
     will be closed. *)
  let with_in_channel filename f =
    let pipe_input args f =
      let reader, writer = Unix.pipe () in
      let pid =
        Unix.create_process args.(0) args Unix.stdin writer Unix.stderr in
      Unix.close writer;
      let in_channel = Unix.in_channel_of_descr reader in
      finally
        (fun () -> close_in in_channel; ignore (Unix.waitpid [] pid))
        f in_channel in
    match filename with
      | Uncompressed "-" ->
          f stdin
      | Uncompressed filename ->
          let in_channel = open_in filename in
          finally
            (fun () -> close_in in_channel)
            f in_channel
      | Compressed filename ->
          pipe_input [| "gzip"; "-dc"; filename |] f
      | URL url ->
          pipe_input [| "lynx"; "-dump"; url |] f
  
  (* Return true if the string s starts with the given prefix. *)
  let starts_with s prefix =
    try Str.first_chars s (String.length prefix) = prefix
    with Invalid_argument _ -> false
  
  (* Return true if the string s ends with the given suffix. *)
  let ends_with s suffix =
    try Str.last_chars s (String.length suffix) = suffix
    with Invalid_argument _ -> false
  
  (* Return true if the string s contains the given substring. *)
  let contains s substring =
    try ignore (Str.search_forward (Str.regexp_string substring) s 0); true
    with Not_found -> false
  
  (* Tag the filename depending on its contents or extension. *)
  let tag_filename filename =
    if contains filename "://"
    then URL filename
    else if List.exists (ends_with filename) [".gz"; ".Z"]
    then Compressed filename
    else Uncompressed filename
  
  (* Process a tagged filename. *)
  let process filename =
    with_in_channel
      filename
      (fun in_channel ->
         try
           while true do
             let line = input_line in_channel in
             (* ... *)
             ()
           done
         with End_of_file -> ())
  
  (* Parse the command-line arguments and process each file or URL. *)
  let () =
    let args =
      if Array.length Sys.argv > 1
      then (List.tl (Array.to_list Sys.argv))
      else ["-"] in
    List.iter process (List.map tag_filename args)
  

