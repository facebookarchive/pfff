(* ********************************************************************** *)
(* Reversing a String by Word or Character *)
(* ********************************************************************** *)
let pleac_Reversing_a_String_by_Word_or_Character () = 
  
  (* To flip the characters of a string, we can use a for loop.
   * Note that this version does not destructively update the string *)
  
  let reverse s = 
    let len = String.length s - 1 in
    let s' = String.create (len + 1) in
    for i = 0 to len do
      s'.[i] <- s.[len - i]
    done;
    s';;
  
  (* to modify the string in place, we can use the following function *)
  let reverse_in_place s =
    let len = String.length s - 1 in
    for i = 0 to (len + 1)/ 2 - 1 do
      let t = s.[i] in
      s.[i] <- s.[len - i];
      s.[len - i] <- t
    done;;
  
  (* To reverse the words in a string, we can use String.concat, Str.split and
   * List.rev.  Note that this requires us to load in the Str module -- 
   * use `#load "str.cma"' in* the toplevel, or be sure to include str.cma in the
   * list of object files when compiling your code.  E.g.:
   *      ocamlc other options str.cma other files   -or-
   *      ocamlopt other options str.cmxa other files 
  *)
  
  let reverse_words s =
    String.concat " " (List.rev (Str.split (Str.regexp " ") s));;
  
  let is_palindrome s = 
    s = reverse s;;
  
  (* We do need to do a bit more work that Perl to find the big palindromes in
   * /usr/share/dict/words ... *)
  
  let findBigPals () = 
    let words = open_in "/usr/share/dict/words" in
    let rec loop () =
      let w = input_line words in
      if String.length w > 5 && w = reverse w then
        print_endline w;
      loop () in
    try loop () with End_of_file -> close_in words;;
  

