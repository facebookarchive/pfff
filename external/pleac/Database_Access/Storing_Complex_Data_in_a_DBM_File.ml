(* ********************************************************************** *)
(* Storing Complex Data in a DBM File *)
(* ********************************************************************** *)
let pleac_Storing_Complex_Data_in_a_DBM_File () = 
  (* OCaml includes a Marshal module which does binary serialization and
     deserialization of arbitrary data structures. However, it is not
     type-safe, so coding errors can result in segmentation faults.
  
     One way to eliminate this risk is to use functors. The following
     example builds a functor called "MakeSerializedDbm" which extends
     the Dbm module to provide type-safe serialization of values using
     a user-defined method such as (but not limited to) Marshal. *)
  
  #load "dbm.cma";;
  
  (* This module type defines a serialization method. It contains a type
     and functions to convert values of that type to and from strings. *)
  module type SerializedDbmMethod =
  sig
    type value
    val serialize : value -> string
    val deserialize : string -> value
  end
  
  (* This module type defines an enhanced Dbm interface that includes a
     type for values to be used instead of strings. *)
  module type SerializedDbm =
  sig
    type t
    type value
    val opendbm : string -> Dbm.open_flag list -> int -> t
    val close : t -> unit
    val find : t -> string -> value
    val add : t -> string -> value -> unit
    val replace : t -> string -> value -> unit
    val remove : t -> string -> unit
    val firstkey : t -> string
    val nextkey : t -> string
    val iter : (string -> value -> 'a) -> t -> unit
  end
  
  (* Here is the functor itself. It takes a SerializedDbmMethod as an
     argument and returns a SerializedDbm module instance as a result.
     It is defined mainly in terms of Dbm, with a few overridden
     definitions where the value type is needed. *)
  module MakeSerializedDbm (Method : SerializedDbmMethod)
    : SerializedDbm with type value = Method.value =
  struct
    include Dbm
    type value = Method.value
    let find db key = Method.deserialize (find db key)
    let add db key value = add db key (Method.serialize value) 
    let replace db key value = replace db key (Method.serialize value)
    let iter f db = iter (fun key value -> f key (Method.deserialize value)) db
  end
  
  (* Now, we can easily build typed Dbm interfaces by providing the type
     and conversion functions. In this case, we use Marshal, but we could
     also use other string-based serialization formats like JSON or XML. *)
  module StringListDbm =
    MakeSerializedDbm(struct
                        type value = string list
                        let serialize x = Marshal.to_string x []
                        let deserialize x = Marshal.from_string x 0
                      end)
  
  let db = StringListDbm.opendbm "data.db" [Dbm.Dbm_rdwr; Dbm.Dbm_create] 0o666
  let () =
    StringListDbm.replace db "Tom Christiansen"
      [ "book author"; "tchrist@perl.com" ];
    StringListDbm.replace db "Tom Boutell"
      [ "shareware author"; "boutell@boutell.com" ];
  
    (* names to compare *)
    let name1 = "Tom Christiansen" in
    let name2 = "Tom Boutell" in
  
    let tom1 = StringListDbm.find db name1 in
    let tom2 = StringListDbm.find db name2 in
  
    let show strings =
      "[" ^ (String.concat "; "
               (List.map (fun s -> "\"" ^ s ^ "\"") strings)) ^ "]" in
    Printf.printf "Two Toming: %s %s\n" (show tom1) (show tom2)
  

