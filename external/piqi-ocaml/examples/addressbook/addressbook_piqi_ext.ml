let piqi = Addressbook_piqi.piqi


let _ = Piqirun_ext.init_piqi piqi


let _person_piqi_type = Piqirun_ext.find_piqi_type "addressbook/person"
let _person_phone_number_piqi_type = Piqirun_ext.find_piqi_type "addressbook/person-phone-number"
let _person_phone_type_piqi_type = Piqirun_ext.find_piqi_type "addressbook/person-phone-type"
let _address_book_piqi_type = Piqirun_ext.find_piqi_type "addressbook/address-book"


let parse_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Addressbook_piqi.parse_person buf

let parse_person_phone_number ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_phone_number_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Addressbook_piqi.parse_person_phone_number buf

let parse_person_phone_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_phone_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Addressbook_piqi.parse_person_phone_type buf

let parse_address_book ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _address_book_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Addressbook_piqi.parse_address_book buf


let gen_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Addressbook_piqi.gen_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_piqi_type `pb format x_pb ?opts

let gen_person_phone_number ?opts x (format :Piqirun_ext.output_format) =
  let buf = Addressbook_piqi.gen_person_phone_number x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_phone_number_piqi_type `pb format x_pb ?opts

let gen_person_phone_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Addressbook_piqi.gen_person_phone_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_phone_type_piqi_type `pb format x_pb ?opts

let gen_address_book ?opts x (format :Piqirun_ext.output_format) =
  let buf = Addressbook_piqi.gen_address_book x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _address_book_piqi_type `pb format x_pb ?opts


let print_person ?opts x =
  Pervasives.print_endline (gen_person x `piq ?opts)
let prerr_person ?opts x =
  Pervasives.prerr_endline (gen_person x `piq ?opts)

let print_person_phone_number ?opts x =
  Pervasives.print_endline (gen_person_phone_number x `piq ?opts)
let prerr_person_phone_number ?opts x =
  Pervasives.prerr_endline (gen_person_phone_number x `piq ?opts)

let print_person_phone_type ?opts x =
  Pervasives.print_endline (gen_person_phone_type x `piq ?opts)
let prerr_person_phone_type ?opts x =
  Pervasives.prerr_endline (gen_person_phone_type x `piq ?opts)

let print_address_book ?opts x =
  Pervasives.print_endline (gen_address_book x `piq ?opts)
let prerr_address_book ?opts x =
  Pervasives.prerr_endline (gen_address_book x `piq ?opts)


