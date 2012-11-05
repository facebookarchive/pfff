(*
 * This file is part of Javalib
 * Copyright (c)2010 Tiphaine Turpin (Université de Rennes 1)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)
{

}

(* Name-Value pairs and Sections *)

let cr = '\r'
let lf = '\n'
let newline = cr lf? | lf
let otherchar = [^ '\000' '\r' '\n']
let alphanum = ['a' - 'z' 'A' - 'Z' '0' - '9']
let headerchar = alphanum | '-' | '_'
let name = alphanum headerchar*

rule value = parse
  | (otherchar* as s) { s ^ continuation lexbuf }

and continuation = parse
  | newline ' ' { value lexbuf }
  | newline | eof { "" }

and section = parse
  | (name as n) ": " {
      let v = value lexbuf in
	(n, v) :: section lexbuf
    }
  | newline+ | eof { [] }

{

  let rec sections lexbuf =
    match section lexbuf with
      | [] -> []
      | s -> s :: sections lexbuf

  type main_section = {
    manifest_version : int list;
    main_attributes : (string * string) list
  }

  type section = {
    name : string;
    attributes : (string * string) list
  }

  type manifest = {
    main_section : main_section;
    individual_sections : section list
  }

  open ExtString

  (* TODO : accept missing manifest version (for midlets) *)
  let sections2manifest = function
      | ((mv, v) :: main) :: sections
	  when String.lowercase mv = "manifest-version" ->
	  {main_section =
	      {manifest_version = List.map int_of_string (String.nsplit v ".");
	       main_attributes = main};
	   individual_sections =
	      List.map
		(function
		   | (name, v) :: attributes
		       when String.lowercase name = "name" ->
		       {name = v ; attributes = attributes}
		   | _ -> failwith "incorrect manifest")
		sections}
      | _ -> failwith "incorrect manifest"

  let jar2manifest f =
    if not (Filename.check_suffix f ".jar") then
      invalid_arg "not a jar file";
    let c = Zip.open_in f in
    let e = Zip.find_entry c "META-INF/MANIFEST.MF" in
    let s = Zip.read_entry c e in
      Zip.close_in c;
      let lexbuf = Lexing.from_string s in
      let sections = sections lexbuf in
	sections2manifest sections

  let midlet_main_class m =
    let s = List.assoc "MIDlet-1" m.main_section.main_attributes in
      match List.map (String.strip) (String.nsplit s ",") with
	| [_name ; _icon ; main] -> main
	| _ -> failwith "incorrect MIDlet-1 attribute"

(* for testing:
  let _ =
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    let sections = sections lexbuf in
      List.iter
	(function s ->
	   print_endline "Section:";
	   List.iter
	     (function n, v -> print_string (n ^ "=" ^ v ^ "\n"))
	     s;
	   print_newline ())
	sections;
      flush stdout;
      prerr_endline "OK"

  let _ =
    let m = jar2manifest Sys.argv.(1) in
      print_endline (midlet_main_class m)

*)

}
