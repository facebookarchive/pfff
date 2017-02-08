module rec Addressbook_piqi:
  sig
    type protobuf_int32 = int32
    type person_phone_type =
      [
        | `mobile
        | `home
        | `work
      ]
    type person = Person.t
    type person_phone_number = Person_phone_number.t
    type address_book = Address_book.t
  end = Addressbook_piqi
and Person:
  sig
    type t = {
      mutable name: string;
      mutable id: Addressbook_piqi.protobuf_int32;
      mutable email: string option;
      mutable phone: Addressbook_piqi.person_phone_number list;
    }
  end = Person
and Person_phone_number:
  sig
    type t = {
      mutable number: string;
      mutable phone_type: Addressbook_piqi.person_phone_type;
    }
  end = Person_phone_number
and Address_book:
  sig
    type t = {
      mutable person: Addressbook_piqi.person list;
    }
  end = Address_book


let rec parse_int32 x = Piqirun.int32_of_zigzag_varint x
and packed_parse_int32 x = Piqirun.int32_of_packed_zigzag_varint x

and parse_string x = Piqirun.string_of_block x

and parse_protobuf_int32 x = Piqirun.int32_of_signed_varint x
and packed_parse_protobuf_int32 x = Piqirun.int32_of_packed_signed_varint x

and parse_person x =
  let x = Piqirun.parse_record x in
  let _name, x = Piqirun.parse_required_field 1 parse_string x in
  let _id, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _email, x = Piqirun.parse_optional_field 3 parse_string x in
  let _phone, x = Piqirun.parse_repeated_field 4 parse_person_phone_number x in
  Piqirun.check_unparsed_fields x;
  {
    Person.name = _name;
    Person.id = _id;
    Person.email = _email;
    Person.phone = _phone;
  }

and parse_person_phone_number x =
  let x = Piqirun.parse_record x in
  let _number, x = Piqirun.parse_required_field 1 parse_string x in
  let _phone_type, x = Piqirun.parse_required_field 2 parse_person_phone_type x ~default:"\b\001" in
  Piqirun.check_unparsed_fields x;
  {
    Person_phone_number.number = _number;
    Person_phone_number.phone_type = _phone_type;
  }

and parse_person_phone_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `mobile
    | 1l -> `home
    | 2l -> `work
    | x -> Piqirun.error_enum_const x
and packed_parse_person_phone_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `mobile
    | 1l -> `home
    | 2l -> `work
    | x -> Piqirun.error_enum_const x

and parse_address_book x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_repeated_field 1 parse_person x in
  Piqirun.check_unparsed_fields x;
  {
    Address_book.person = _person;
  }


let rec gen__int32 code x = Piqirun.int32_to_zigzag_varint code x
and packed_gen__int32 x = Piqirun.int32_to_packed_zigzag_varint x

and gen__string code x = Piqirun.string_to_block code x

and gen__protobuf_int32 code x = Piqirun.int32_to_signed_varint code x
and packed_gen__protobuf_int32 x = Piqirun.int32_to_packed_signed_varint x

and gen__person code x =
  let _name = Piqirun.gen_required_field 1 gen__string x.Person.name in
  let _id = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Person.id in
  let _email = Piqirun.gen_optional_field 3 gen__string x.Person.email in
  let _phone = Piqirun.gen_repeated_field 4 gen__person_phone_number x.Person.phone in
  Piqirun.gen_record code (_name :: _id :: _email :: _phone :: [])

and gen__person_phone_number code x =
  let _number = Piqirun.gen_required_field 1 gen__string x.Person_phone_number.number in
  let _phone_type = Piqirun.gen_required_field 2 gen__person_phone_type x.Person_phone_number.phone_type in
  Piqirun.gen_record code (_number :: _phone_type :: [])

and gen__person_phone_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `mobile -> 0l
    | `home -> 1l
    | `work -> 2l
  )
and packed_gen__person_phone_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `mobile -> 0l
    | `home -> 1l
    | `work -> 2l
  )

and gen__address_book code x =
  let _person = Piqirun.gen_repeated_field 1 gen__person x.Address_book.person in
  Piqirun.gen_record code (_person :: [])


let gen_int32 x = gen__int32 (-1) x
let gen_string x = gen__string (-1) x
let gen_protobuf_int32 x = gen__protobuf_int32 (-1) x
let gen_person x = gen__person (-1) x
let gen_person_phone_number x = gen__person_phone_number (-1) x
let gen_person_phone_type x = gen__person_phone_type (-1) x
let gen_address_book x = gen__address_book (-1) x


let rec default_int32 () = 0l
and default_string () = ""
and default_protobuf_int32 () = default_int32 ()
and default_person () =
  {
    Person.name = default_string ();
    Person.id = default_protobuf_int32 ();
    Person.email = None;
    Person.phone = [];
  }
and default_person_phone_number () =
  {
    Person_phone_number.number = default_string ();
    Person_phone_number.phone_type = parse_person_phone_type (Piqirun.parse_default "\b\001");
  }
and default_person_phone_type () = `mobile
and default_address_book () =
  {
    Address_book.person = [];
  }


let piqi = "\226\202\2304\011addressbook\226\231\249\238\001\022addressbook.proto.piqi\162\244\146\155\011\btutorial\218\244\134\182\012\208\001\138\233\142\251\014\201\001\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004name\210\171\158\194\006\006string\210\203\242$+\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002id\210\171\158\194\006\014protobuf-int32\210\203\242$&\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005email\210\171\158\194\006\006string\210\203\242$3\232\146\150q\b\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\005phone\210\171\158\194\006\019person-phone-number\218\164\238\191\004\006person\218\244\134\182\012\193\001\138\233\142\251\014\186\001\210\203\242$'\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006number\210\171\158\194\006\006string\210\203\242$p\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004type\210\171\158\194\006\017person-phone-type\226\128\157\190\n\nphone_type\138\140\251\240\r*\218\148\211\024\002\b\001\210\171\158\194\006\029addressbook/person-phone-type\218\164\238\191\004\019person-phone-number\218\244\134\182\012\148\001\138\176\205\197\001\141\001\218\164\238\191\004\017person-phone-type\170\183\218\222\005$\232\146\150q\000\234\188\204\215\002\rperson_mobile\218\164\238\191\004\006mobile\170\183\218\222\005 \232\146\150q\002\234\188\204\215\002\011person_home\218\164\238\191\004\004home\170\183\218\222\005 \232\146\150q\004\234\188\204\215\002\011person_work\218\164\238\191\004\004work\218\244\134\182\012D\138\233\142\251\014>\210\203\242$'\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006person\210\171\158\194\006\006person\218\164\238\191\004\012address-book"
include Addressbook_piqi
