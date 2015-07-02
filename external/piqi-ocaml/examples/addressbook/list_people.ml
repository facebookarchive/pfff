
open Addressbook_piqi


let printf = Printf.printf


(* Iterates though all people in the AddressBook and prints info about them. *)
let list_people address_book =
  let print_email = function
    | Some email ->
        printf "  E-mail address: %s\n" email
    | None -> ()
  in
  let print_phone_number x =
    (match x.Person_phone_number.phone_type with
      | `mobile ->
          printf "  Mobile phone #: "
      | `home ->
          printf "  Home phone #: "
      | `work ->
          printf "  Work phone #: "
    );
    printf "%s\n" x.Person_phone_number.number
  in
  let list_person x =
    printf "Person ID: %ld\n" x.Person.id;
    printf "  Name: %s\n" x.Person.name;
    print_email x.Person.email;
    List.iter print_phone_number x.Person.phone
  in
  List.iter list_person address_book.Address_book.person


(* Main function:  Reads the entire address book from a file and prints all
 * the information inside. *)
let _ =
  if Array.length Sys.argv <> 2
  then
    ( Printf.eprintf "Usage: %s ADDRESS_BOOK_FILE\n" Sys.argv.(0);
      exit (-1)
    );
  (* Read the existing address book. *)
  let ch = open_in_bin Sys.argv.(1) in
  let buf = Piqirun.init_from_channel ch in
  let address_book = parse_address_book buf in
  close_in ch;

  list_people address_book

