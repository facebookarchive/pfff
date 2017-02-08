

module Ab = Addressbook_piqi_ext


(* Main function:  Reads the entire address book from a file and prints all
 * the information inside in various formats *)
let _ =
  if Array.length Sys.argv <> 2
  then
    ( Printf.eprintf "Usage: %s ADDRESS_BOOK_FILE\n" Sys.argv.(0);
      exit (-1)
    );

  (* Read the existing address book in binary Protocol Buffers format *)
  let ch = open_in_bin Sys.argv.(1) in
  let buf = Piqirun.init_from_channel ch in
  let address_book = Addressbook_piqi.parse_address_book buf in
  close_in ch;

  (* Serialize addressbook to JSON format *)
  let json = Ab.gen_address_book address_book `json in
  Printf.printf "\n\nJSON: \n\n%s\n" json;
  (* Read back from JSON *)
  let address_book' = Ab.parse_address_book json `json in
  assert (address_book' = address_book);

  (* Serialize addressbook to pretty-printed JSON format *)
  let json = Ab.gen_address_book address_book `json_pretty in
  Printf.printf "\n\npretty-printed JSON: \n\n%s\n" json;
  (* Read back from JSON *)
  let address_book' = Ab.parse_address_book json `json in
  assert (address_book' = address_book);

  (* Serialize addressbook to pretty-printed JSON format and include "null" JSON
   * fields for missing optional and [] for missing required fields *)
  let opts = Piqirun_ext.make_options ~json_omit_missing_fields:false () in
  let json = Ab.gen_address_book address_book `json ~opts in
  Printf.printf "\n\nJSON with null fields: \n\n%s\n" json;
  (* Read back from JSON *)
  let address_book' = Ab.parse_address_book json `json in
  assert (address_book' = address_book);

  (* Serialize addressbook to XML format *)
  let xml = Ab.gen_address_book address_book `xml in
  Printf.printf "\n\nXML: \n\n%s\n" xml;
  (* Read back from XML *)
  let address_book' = Ab.parse_address_book xml `xml in
  assert (address_book' = address_book);

  (* Serialize addressbook to pretty-printed XML format *)
  let xml = Ab.gen_address_book address_book `xml_pretty in
  Printf.printf "\n\npretty-printed XML: \n\n%s\n" xml;
  (* Read back from XML *)
  let address_book' = Ab.parse_address_book xml `xml in
  assert (address_book' = address_book);


  (* Serialize addressbook to Piq format *)
  let piq = Ab.gen_address_book address_book `piq in
  Printf.printf "\n\nPiq: \n\n%s\n" piq;
  (* Read back from XML *)
  let address_book' = Ab.parse_address_book piq `piq in
  assert (address_book' = address_book);


  (* Print addressbook to stdout in Piq format *)
  Printf.printf "\n\nPrinting to stdout:\n\n";
  Ab.print_address_book address_book;


  (* Print addressbook to stderr in Piq format *)
  Printf.eprintf "\n\nPrinting to stderr:\n\n";
  Ab.prerr_address_book address_book;

  ()

