(* $Id: unimap_to_ocaml.ml 1084 2007-02-20 12:36:17Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

open Printf;;

type format =
    Normal
  | Jis0208
  | Jis0212
  | Ks1001

let comment_re = Str.regexp "#.*$";;
let space_re = Str.regexp "[ \t\r\n]+";;

let read_unimap_format_a ?(fmt=Normal) fname f =
  (* Reads a Unicode mapping in format A from a "local" code to Unicode.
   * Returns a list of pairs (localcode, unicode).
   *)
  
  let read_unimap_line() =
    let s = input_line f in    (* may raise End_of_file *)
    let s' = Str.global_replace comment_re "" s in
    let words = Str.split space_re s' in
    match words with
	[] -> raise Not_found
      | [ localcode; unicode ] when fmt=Normal ->
	  int_of_string localcode, int_of_string unicode
      | [ localcode; unicode ] when fmt=Jis0212 ->
	  let local = int_of_string localcode in
	  let row = (local lsr 8) - 0x20 in
	  let col = (local land 255) - 0x20 in
	  assert (row >= 1 && row <= 94 && col >= 1 && col <= 94);
	  (row * 96 + col, int_of_string unicode)
      | [ _; localcode; unicode ] when fmt=Jis0208 ->
	  let local = int_of_string localcode in
	  let row = (local lsr 8) - 0x20 in
	  let col = (local land 255) - 0x20 in
	  assert (row >= 1 && row <= 94 && col >= 1 && col <= 94);
	  (row * 96 + col, int_of_string unicode)
      | [ localcode; unicode ] when fmt=Ks1001 ->
	  let local = int_of_string localcode in
	  let row = (local lsr 8) - 0x20 in
	  let col = (local land 255) - 0x20 in
	  assert (row >= 1 && row <= 94 && col >= 1 && col <= 94);
	  (row * 96 + col, int_of_string unicode)
      | _ ->
	  failwith ("File " ^ fname ^ ": Do not know what to do with:\n" ^ s')
  in

  let rec read_following_lines() =
    try
      let localcode, unicode = read_unimap_line() in 
                               (* may raise End_of_file, Not_found *)
      (localcode, unicode) :: read_following_lines()
    with
	Not_found -> read_following_lines()
      | End_of_file -> []
  in

  read_following_lines()
;;


type from_uni_list =
    U_nil
  | U_single of (int * int)
  | U_double of (int * int * int * int)
  | U_array of int array

type from_unicode =
    from_uni_list array;;
  (* A hashtable with fixed size (usually 256). A pair (unicode, localcode) is
   * stored at the position unicode mod 256 in the array.
   *)


let make_bijection unimap =
  (* unimap: a list of pairs (localcode, unicode)
   * returns a pair of arrays (m_to_unicode, m_from_unicode) with:
   *   - m_to_unicode.(localcode) = Some unicode, 
   *                                 if the pair (localcode, unicode) exists
   *     m_to_unicode.(x) = None otherwise
   *   - m_from_unicode.(unicode lsr 8) = [ ...; (unicode,localcode); ... ]
   *)

  let l = List.length unimap in

  let max_localcode = ref 255 in
  List.iter
    (fun (localcode, _) ->
       max_localcode := max !max_localcode localcode
    )
    unimap;

  let m_from_size = ref 256 in
  while !m_from_size < l do
    m_from_size := !m_from_size * 2
  done;

  let m_to_unicode   = Array.create (!max_localcode+1) None in
  let m_from_unicode = Array.create !m_from_size [] in

  List.iter
    (fun (localcode, unicode) ->
       assert(localcode <= !max_localcode);

       (* Update m_to_unicode: *)
       if m_to_unicode.(localcode) <> None then
	 failwith ("Local code point " ^ string_of_int localcode ^ 
		   " mapped twice");
       m_to_unicode.(localcode) <- Some unicode;

       (* Update m_from_unicode: *)
       let unilow = unicode land (!m_from_size - 1) in
       if List.mem_assoc unicode (m_from_unicode.(unilow)) then
	 failwith ("Unicode code point " ^ string_of_int unicode ^ 
		   " mapped twice");
       m_from_unicode.(unilow) <- 
         m_from_unicode.(unilow) @ [unicode,localcode];
    )
    unimap;

  m_to_unicode, m_from_unicode
;;


let to_unimap_as_string to_unimap =
  let make_repr x =
    match x with
	None -> -1
      | Some u -> u
  in
  Marshal.to_string (Array.map make_repr to_unimap) [ Marshal.No_sharing ]
;;


let from_unimap_as_string from_unimap =
  let make_repr l =
    match l with
	[]    -> U_nil
      | [u,l] -> U_single(u,l)
      | [u1,l1; u2,l2] -> U_double(u1,l1,u2,l2)
      | _     -> U_array (Array.of_list 
			    (List.flatten (List.map (fun (u,l) -> [u;l]) l)))
  in
  let m = Array.map make_repr from_unimap in
  Marshal.to_string m [ Marshal.No_sharing ]
;;


let count_heavy_conflicts from_unimap =
  Array.fold_left 
    (fun n x -> if List.length x > 2 then n+1 else n) 0 from_unimap
;;


let print_bijection f name m_to_unicode m_from_unicode =
  (* Prints on file f this O'Caml code:
   * let <name>_to_unicode = ...
   * let <name>_from_unicode = ...
   *)
  fprintf f "let %s_to_unicode = \"%s\";;\n" 
    name 
    (String.escaped (to_unimap_as_string m_to_unicode));

  fprintf f "let %s_from_unicode = \"%s\";;\n"
    name
    (String.escaped (from_unimap_as_string m_from_unicode));
;;


let print_ocaml_file out unimaps =
  (* Compute all bijections: *)
  let bijections =
    List.map
      (fun (mapname, unimap) ->
	 prerr_endline ("Processing " ^ mapname);
	 let to_unicode, from_unicode = make_bijection unimap in
	 prerr_endline ("   (Heavy conflicts: " ^ 
			string_of_int (count_heavy_conflicts from_unicode) ^ 
			" of " ^ 
			string_of_int (Array.length from_unicode) ^ ")");
	 mapname, to_unicode, from_unicode
      )
      unimaps
  in

  (* Output all results: *)
  output_string out "(* WARNING! This is a generated file! *)\n";

  List.iter
    (fun (mapname, to_unicode, from_unicode) ->
       print_bijection out mapname to_unicode from_unicode)
    bijections;
  List.iter
    (fun (mapname, _, _) ->
       fprintf out "Netdb.set_db \"cmapf.%s\" %s_to_unicode;\n" 
	           mapname mapname;
       fprintf out "Netdb.set_db \"cmapr.%s\" %s_from_unicode;\n" 
	           mapname mapname;
    )
    (List.rev bijections);
  fprintf out "Netdb.disable_file_db();;\n";
;;


let print_netdb_files unimaps =
  (* Compute all bijections: *)
  let bijections =
    List.map
      (fun (mapname, unimap) ->
	 prerr_endline ("Processing " ^ mapname);
	 let to_unicode, from_unicode = make_bijection unimap in
	 prerr_endline ("   (Heavy conflicts: " ^ 
			string_of_int (count_heavy_conflicts from_unicode) ^ 
			" of " ^ 
			string_of_int (Array.length from_unicode) ^ ")");
	 mapname, to_unicode, from_unicode
      )
      unimaps
  in

  List.iter
    (fun (mapname, to_unicode, from_unicode) ->
       let filename_to = "cmapf." ^ mapname ^ ".netdb" in
       let ch = open_out_bin filename_to in
       output_string ch (to_unimap_as_string to_unicode);
       close_out ch;
       let filename_from = "cmapr." ^ mapname ^ ".netdb" in
       let ch = open_out_bin filename_from in
       output_string ch (from_unimap_as_string from_unicode);
       close_out ch;
    )
    bijections;
;;


let write_portable_file out unimaps =
  List.iter
    (fun (name,unimap) ->
       output_string out (name ^ "\n");
       List.iter
	 (fun (localcode,unicode) ->
	    output_string out (string_of_int localcode ^ "\n");
	    output_string out (string_of_int unicode ^ "\n");
	 )
	 unimap;
       output_string out "\n";
    )
    unimaps
;;


let read_portable_file inch =
  let unimaps = ref [] in
  let rec read_section() =
    let lc_str = input_line inch in
    if lc_str = "" then
      []
    else
      let uc_str = input_line inch in
      (int_of_string lc_str, int_of_string uc_str) :: read_section()
  in
  try
    while true do
      let name = input_line inch in
      let map = read_section() in
      unimaps := (name,map) :: !unimaps
    done;
    assert false
  with
      End_of_file ->
	!unimaps
;;


let main() =
  let files = ref [] in
  let outch = ref (lazy stdout) in
  let pmap = ref false in
  let netdb = ref false in
  Arg.parse
      [ "-o", Arg.String (fun s -> outch := lazy (open_out s)),
           " <file>   Redirect stdout to this file";
	"-pmap", Arg.Set pmap,
	      "       Write in pmap format (portable maps)";
	"-netdb", Arg.Set netdb,
	       "      Write netdb files (non-portable maps)";
      ]
      (fun s -> files := !files @ [s])
      "usage: unimap_to_ocaml file.unimap ... file.pmap ...";
  
  (* First read in all unimaps: *)
  let unimaps =
    List.flatten
      (List.map
	 (fun filename ->
	    let mapname = 
	      Filename.chop_extension (Filename.basename filename) in
	    if Filename.check_suffix filename ".unimap" then begin
	      let f = open_in filename in
	      prerr_endline ("Reading " ^ filename);
	      let unimap = read_unimap_format_a filename f in
	      close_in f;
	      [ mapname, unimap ]
	    end
	    else 
	      if Filename.check_suffix filename ".pmap" then begin
		let f = open_in filename in
		prerr_endline ("Reading " ^ filename);
		let unimaps = read_portable_file f in
		close_in f;
		unimaps
	      end
	      else
		if Filename.check_suffix filename ".0208map" then begin
		  let f = open_in filename in
		  prerr_endline ("Reading " ^ filename);
		  let unimap = read_unimap_format_a ~fmt:Jis0208 filename f in
		  close_in f;
		  [ mapname, unimap ]
		end
		else
		  if Filename.check_suffix filename ".0212map" then begin
		    let f = open_in filename in
		    prerr_endline ("Reading " ^ filename);
		    let unimap = read_unimap_format_a ~fmt:Jis0212 filename f in
		    close_in f;
		    [ mapname, unimap ]
		  end
		  else
		    if Filename.check_suffix filename ".1001map" then begin
		      let f = open_in filename in
		      prerr_endline ("Reading " ^ filename);
		      let unimap = read_unimap_format_a ~fmt:Ks1001 filename f in
		      close_in f;
		      [ mapname, unimap ]
		    end
		    else
		      failwith ("Unknown filename suffix: " ^ filename)
	 )
	 !files
      )
  in

  let out = Lazy.force !outch in

  if !netdb then begin
    print_netdb_files unimaps
  end
  else if !pmap then begin
    write_portable_file out unimaps
  end
  else begin
    print_ocaml_file out unimaps
  end;

  close_out out
;;


main();;

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 2.4  2003/06/03 18:49:10  stolpmann
 * 	Support for netdb.
 * 	Support for Japanese encodings.
 *
 * Revision 2.3  2002/06/23 19:48:03  stolpmann
 * 	Improved representation of character mappings.
 *
 * Revision 2.2  2002/06/09 10:53:44  stolpmann
 * 	Introducing the pmap format (portable map) to avoid problems
 * with incompatible marshalling in the future
 *
 * Revision 2.1  2001/09/14 14:22:34  stolpmann
 * 	Initial revision (sourceforge)
 *
 *
 * ======================================================================
 * Revision 1.3  2000/08/29 00:48:52  gerd
 * 	Conversion tables are now stored in marshalled form.
 * 	New type for the conversion table Unicode to 8bit.
 *
 * Revision 1.2  2000/08/12 23:54:56  gerd
 * 	Initial revision.
 *
 * 
 *)
