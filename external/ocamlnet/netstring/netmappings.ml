(* $Id: netmappings.ml 1219 2009-04-14 13:28:56Z ChriS $
 * ----------------------------------------------------------------------
 *
 *)

type from_uni_list =
    U_nil
  | U_single of (int*int)
  | U_double of (int*int * int*int)
  | U_array of int array
;;

let to_unicode = Hashtbl.create 50;;
let from_unicode = Hashtbl.create 50;;

let omtp = !Netsys_oothr.provider
let mutex = omtp # create_mutex()

let lock () = mutex#lock();;
let unlock () = mutex#unlock();;


let get_to_unicode enc_name : int array =
  lock();
  try
    let table =
      try
	Hashtbl.find to_unicode enc_name
      with
	  Not_found ->
	    let t = Netdb.read_db ("cmapf." ^ enc_name) in
	    Hashtbl.add to_unicode enc_name t;
	    t
    in
    unlock();
    table
  with
      error -> 
	unlock();
	raise error
;;


let get_from_unicode enc_name : from_uni_list array =
  lock();
  try
    let table =
      try
	Hashtbl.find from_unicode enc_name
      with
	  Not_found ->
	    let t = Netdb.read_db ("cmapr." ^ enc_name) in
	    Hashtbl.add from_unicode enc_name t;
	    t
    in
    unlock();
    table
  with
      error -> 
	unlock();
	raise error
;;
