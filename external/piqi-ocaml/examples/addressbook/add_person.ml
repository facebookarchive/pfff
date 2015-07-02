
module A = Addressbook_piqi


let default_phone_number = A.default_person_phone_number ()


let read_phone_type () =
  print_endline "Is this a mobile, home, or work phone? ";
  match read_line () with
    | "mobile" -> `mobile
    | "home" -> `home
    | "work" -> `work
    | _ ->
        print_endline "Unknown phone type.  Using default.";
        default_phone_number.A.Person_phone_number.phone_type


let read_phone_numbers () =
  let rec aux accu =
    match read_line () with
      | "" -> List.rev accu
      | number ->
          let phone_type = read_phone_type () in
          let res =
            A.Person_phone_number.({
              number = number;
              phone_type = phone_type;
            })
          in aux (res :: accu)
  in aux []


(* This function fills in a Person message based on user input. *)
let prompt_for_address address_book =
  print_endline "Enter person ID number: ";
  let id = Int32.of_string (read_line ()) in

  print_endline "Enter name: ";
  let name = read_line () in

  print_endline "Enter email address (blank for none): ";
  let email =
    match read_line () with
      | "" -> None
      | x -> Some x
  in

  print_endline "Enter a phone number (or leave blank to finish): ";
  let phone_numbers = read_phone_numbers () in
  let person =
    A.Person.({
      id = id;
      name = name;
      email = email;
      phone = phone_numbers;
    })
  in
  A.Address_book.({
    (* address_book with *)
    person = address_book.person @ [person]
  })


(*
 * Main function: Reads the entire address book from a file,
 * adds one person based on user input, then writes it back out to the same
 * file.
 *)
let _ =
  if Array.length Sys.argv <> 2
  then
    ( Printf.eprintf "Usage: %s ADDRESS_BOOK_FILE\n" Sys.argv.(0);
      exit (-1)
    );

  let address_book =
    try
      (* Read the existing address book. *)
      let ch = open_in_bin Sys.argv.(1) in
      let buf = Piqirun.init_from_channel ch in
      let res = A.parse_address_book buf in
      close_in ch;
      res
    with 
      | Sys_error _ ->
          Printf.printf "%s: File not found.  Creating a new file.\n" Sys.argv.(0);
          A.Address_book.({person = []})
      | Piqirun.Error _ ->
          Printf.eprintf "Failed to parse address book.\n";
          exit (-1)
  in

  (* Add an address. *)
  let address_book = prompt_for_address address_book in

  (* Write the new address book back to disk. *)
  let och = open_out_bin Sys.argv.(1) in
  (* NOTE: specifying -1 as the field code has a special meaning: it tells
   * generator not to generate the header (code/tag/len) -- just generate the
   * contents *)
  let data = A.gen_address_book address_book in
  Piqirun.to_channel och data;
  close_out och

