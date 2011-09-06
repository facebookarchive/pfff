open Printf
open ExtLib
open Genotype

type t = {
	genos : Genotype.t array;
	fields : string array;
	geno_size : int;
	size : int;
}

let check_fields_nb collec =
	let fields_nb = Array.length collec.fields in
	let wrong_fields_nb g = Array.length g.infos <> fields_nb in
	not (Array.exists wrong_fields_nb collec.genos)

let create genos fields =
	let size = Array.length genos in
	match Genotype.markers_nb genos with
		| None -> invalid_arg "Genotypes.create: diffent genotype sizes"
		| Some geno_size ->
			  let coll = { genos = genos;  fields = fields;
						   geno_size = geno_size; size = size } in
			  if not (check_fields_nb coll) then
				  invalid_arg "Genotypes.create: wrong fields number"
			  else
				  coll

let remove_duplicates coll =
	let set = Array.fold_left
		(fun s g -> GenoSet.add g s) GenoSet.empty coll.genos in
	{ coll with
	  genos = Array.of_list (GenoSet.elements set) }

(* READING A FILE *)

(* See an example in data/geno *)

let (|>) x f = f x

(* Splits a string s with sep, applies f to each *)
let split_and_map s sep f =
    String.nsplit s sep
	|> Array.of_list
	|> Array.map f

let read_markers s =
	try split_and_map s "," float_of_string
	with Failure "float_of_string" ->
		invalid_arg "Genotypes.read_markers"

let read_line line =
    let fields = String.nsplit line ";" in
	if List.length fields < 2 then
		invalid_arg "Genotypes.read_line"
	else
		try
			{ id = List.nth fields 0;
			  markers = read_markers (List.nth fields 1);
			  infos = Array.of_list (List.tl (List.tl fields)) }
		with Invalid_argument _ ->
			invalid_arg "Genotypes.read_line"

let is_comment = function
    | "" -> false
    | s -> s.[0] = '#'

let not_comment s = not (is_comment s)

let read_lines lines =
	match Enum.get lines with
		| None ->
			  invalid_arg "Genotypes.read_lines : no lines"
		| Some s ->
			  let fields = Array.of_list (String.nsplit s ";") in
			  let genotypes = lines
		        |> Enum.map read_line
				|> Array.of_enum in
			  create genotypes fields

let read_file file_name =
    let chan = open_in file_name in
    let lines = chan
	  |> Std.input_lines
	  |> Enum.filter not_comment in
	try let collec = read_lines lines in
		close_in chan;
		collec
	with exc ->
		close_in chan;
		raise exc
