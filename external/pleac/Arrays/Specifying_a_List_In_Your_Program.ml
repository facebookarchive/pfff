(* ********************************************************************** *)
(* Specifying a List In Your Program *)
(* ********************************************************************** *)
let pleac_Specifying_a_List_In_Your_Program () = 
  (* Note that Perl sort of munges OCaml lists and arrays into a single data
   * structure.  In OCaml, they are two distinct data structures, and one needs to
   * learn when it is best to use lists vs. arrays. *)
  
  (* To initialize a list *)
  let l = ["quick"; "brown"; "fox"];;
  
  (* To initialize an array *)
  let a = [|"quick"; "brown"; "fox"|];;
  
  (*-----------------------------*)
  let words s = Str.split (Str.regexp "[ \t]+") s;;
  let l = words "Why are you teasing me?";;
  (*-----------------------------*)
  let str = "  The boy stood on the burning deck,
    It was as hot as glass.
  " in
  let f l =
    let sep = Str.regexp "[ \t\n]*\\(.+\\)" in
    List.map (fun s ->
      if (Str.string_match sep s 0) then
        Str.matched_group 1 s
      else
        ""
    ) l
  in
  f (Str.split (Str.regexp_string "\n") str);;
  (*
   * - : string list =
   * ["The boy stood on the burning deck,"; "It was as hot as glass."]
   *)
  
  let data = open_in "mydatafile" in
  let bigarray = readlines data in
  bigarray;;
  
  

