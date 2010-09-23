(* ********************************************************************** *)
(* Soundex Matching *)
(* ********************************************************************** *)
let pleac_Soundex_Matching () = 
  
  let soundex =
    let code_1 = Char.code '1' in
    let code_A = Char.code 'A' in
    let code_Z = Char.code 'Z' in
  
    let trans = Array.make (code_Z - code_A + 1) 0 in
    let add_letters number letters =
      let add letter =
        trans.(Char.code letter - code_A) <- (number + code_1) in
      String.iter add letters in
    Array.iteri add_letters [| "BFPV"; "CGJKQSXZ"; "DT"; "L"; "MN"; "R" |];
  
    fun ?(length=4) s ->
      let slength = String.length s in
      let soundex = String.make length '0' in
      let rec loop i j last =
        if i < slength && j < length then begin
          let code = Char.code (Char.uppercase s.[i]) in
          if code >= code_A && code <= code_Z
          then (if j = 0
                then (soundex.[j] <- Char.chr code;
                      loop (i + 1) (j + 1) trans.(code - code_A))
                else (match trans.(code - code_A) with
                        | 0 -> loop (i + 1) j 0
                        | code when code <> last ->
                            soundex.[j] <- Char.chr code;
                            loop (i + 1) (j + 1) code
                        | _ -> loop (i + 1) j last))
          else loop (i + 1) j last
        end in
      loop 0 0 0;
      soundex
  
  (*-----------------------------*)
  
  let code = soundex string;;
  let codes = List.map soundex list;;
  
  (*-----------------------------*)
  
  #load "str.cma"
  #load "unix.cma"
  
  let () =
    print_string "Lookup user: ";
    let user = read_line () in
    if user <> "" then begin
      let name_code = soundex user in
      let regexp = Str.regexp ("\\([a-zA-Z_0-9]+\\)[^,]*[^a-zA-Z_0-9]+"
                               ^ "\\([a-zA-Z_0-9]+\\)") in
      let passwd = open_in "/etc/passwd" in
      try
        while true do
          let line = input_line passwd in
          let name = String.sub line 0 (String.index line ':') in
          let {Unix.pw_gecos=gecos} = Unix.getpwnam name in
          let (firstname, lastname) =
            if Str.string_match regexp gecos 0
            then (Str.matched_group 1 gecos, Str.matched_group 2 gecos)
            else ("", "") in
          if (name_code = soundex name
              || name_code = soundex lastname
              || name_code = soundex firstname)
          then Printf.printf "%s: %s %s\n" name firstname lastname
        done
      with End_of_file ->
        close_in passwd
    end
  

