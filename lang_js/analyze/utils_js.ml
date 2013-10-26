
open Common

let string_of_any ast =
	ast +> Meta_ast_js.vof_any +> Ocaml.string_of_v

let load db gen =
	try
		let in_channel = open_in db in
		let data = Marshal.from_channel in_channel in
		close_in in_channel;
		data
	with _ -> 
		let data = gen() in
		let out_channel = open_out db in
		Marshal.to_channel out_channel data [];
		close_out out_channel;
		data
