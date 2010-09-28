(**************************************************************************)
(*                Lablgtk                                                 *)
(*                                                                        *)
(*    This program is free software; you can redistribute it              *)
(*    and/or modify it under the terms of the GNU Library General         *)
(*    Public License as published by the Free Software Foundation         *)
(*    version 2, with the exception described in file COPYING which       *)
(*    comes with the library.                                             *)
(*                                                                        *)
(*    This program is distributed in the hope that it will be useful,     *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU Library General Public License for more details.                *)
(*                                                                        *)
(*    You should have received a copy of the GNU Library General          *)
(*    Public License along with this program; if not, write to the        *)
(*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         *)
(*    Boston, MA 02111-1307  USA                                          *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

let print_binary_string oc s =
  let len = String.length s in
  let off = ref 0 in
  while !off < len do
    if !off mod 20 = 0
    then output_string oc "\\\n" ;
    Printf.fprintf oc "\\%03d" (int_of_char s.[!off]) ;
    incr off
  done

let add_to_list v x =
  v := x :: !v

let rec zip2 = function
  | [] -> []
  | x :: y :: tl -> 
      (x, y) :: zip2 tl
  | _ ->
      invalid_arg "zip2"


let (rle, files) =
  let rle = ref true in
  let pairs = ref false in
  let anon_args = ref [] in
  let cli_args = [
    "--rle", Arg.Set rle, 
    "Enables run-length encoding for the generated pixel data (default)" ;

    "--raw", Arg.Clear rle,
    "Disables run-length encoding for the generated pixel data" ;

    "--build-list", Arg.Set pairs,
    "Enables (name, image) pair parsing mode";
  ] in
  let usg_msg = 
    let exe = Filename.basename Sys.executable_name in
    Printf.sprintf
      "\
usage: %s [options] [image]
       %s [options] --build-list [ [name] [image] ...]" 
      exe exe in

  Arg.parse cli_args (add_to_list anon_args) usg_msg ;
  anon_args := List.rev !anon_args ;

  let files =
    if !pairs
    then begin
      try zip2 !anon_args 
      with Invalid_argument _ ->
	Arg.usage cli_args usg_msg ;
	exit 1
    end
    else 
      match !anon_args with
      | x :: _ ->
	  [ "pixbuf", x ]
      | [] ->
	  Arg.usage cli_args usg_msg ;
	  exit 1 in

  (!rle, files)

let _ =
  Gobject.Type.init ();
  GdkPixbuf.set_marshal_use_rle rle ;
  let data =
    List.map
      (fun (name, fname) -> (name, Marshal.to_string (GdkPixbuf.from_file fname) []))
      files in
  List.iter
    (fun (name, pixdata) ->
      Printf.printf 
	"
let %s_data = \"%a\"

let %s () : GdkPixbuf.pixbuf = Marshal.from_string %s_data 0
" 
    name print_binary_string pixdata name name)
    data
