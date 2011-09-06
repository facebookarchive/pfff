open Phylomel

open Phylogram
open Vec2
open BarnesHut
open Phylogram

open Printf

type args = {
    mutable mode : string;
    mutable format : string;
    mutable input : string;
    mutable id_session : string;
    mutable table : string;
    mutable link_file : string;
    mutable field : string;
    mutable dir : string;
    mutable target : string;
}

let string_of_args args =
    sprintf "mode : %s\nformat : %s\ninput : %s\nid_session : %s\ntable : %s\nlink_file : %s\nfield : %s\n" args.mode args.format args.input args.id_session args.table args.link_file args.field
	
let args_create () = { mode=""; format=""; input=""; id_session=""; table=""; link_file=""; field=""; dir=""; target="" }

let read_args () =
    let args = args_create () in
    let set_mode s = args.mode <- s in
    let set_format s = args.format <- s in
    let set_input s = args.input <- s in
    let set_id_session s = args.id_session <- s in
    let set_table s = args.table <- s in
    let set_link_file s = args.link_file <- s in
    let set_field s = args.field <- s in
    let set_dir s = args.dir <- s in
    let set_target s = args.target <- s in
    let speclist =
	[("-m", Arg.String set_mode, "mode : upgma/mst");
	 ("-f", Arg.String set_format, "format : svg");
	 ("-i", Arg.String set_input, "input : input_file");
	 ("-id", Arg.String set_id_session, "id : session id");
	 ("-table", Arg.String set_table, "table : ");
	 ("-link_file", Arg.String set_link_file, "link_file : file name");
	 ("-field", Arg.String set_field, "field : field in the link");
	 ("-target", Arg.String set_target, "target : in the link")] in
    
    let usage_msg = "usage : genotree -m [upgma/mst] -f [svg/png] -i [input] -id [id_session] -table [table] -link_file [link_file] -field [link_field] -target = [target] [dir/]" in
    Arg.parse speclist set_dir usage_msg;
    args	

(*Modes :
	minimum spanning tree
	upgma
 
Syntax
	test -m [upgma/mst] -f [svg/png] -i [input] -id [id_session] -table [table] -link_file [file] -field [link_field]
	(7 parameters) *)

let update_state n fs fs' bs fig =
	let delta = 0.05 in

	for i=0 to n-1 do
		fs'.(i) <- fs.(i)
	done;

	(* Update forces *)
	ForceDirectedLayout.do_calc_forces fs bs fig;
	
	(* Euler integration on each body *)
	for i=0 to n - 1 do
		let b = bs.(i) in
		let f = fs.(i) in
		let f' = fs'.(i) in
		b.p.x <-
			b.p.x +. delta *. b.v.x +. 1./.2. *. delta *. delta *. f.x;
		b.p.y <-
			b.p.y +. delta *. b.v.y +. 1./.2. *. delta *. delta *. f.y;
		b.v.x <- b.v.x +. delta *. ((f.x +. f'.x) /. 2.);
		b.v.y <- b.v.y +. delta *. ((f.y +. f'.y) /. 2.);
		f.x <- 0.;
		f.y <- 0.
	done

let get_links args coll =
	let get_link genotype = 
		sprintf "%s?type=%s&amp;%s=%s"
		  args.link_file
		  args.table
		  args.field
		  genotype.Genotype.id
	in Array.map get_link coll.Genotypes.genos

let write_html svg_file file (width, height) =
    let channel = open_out file in
    let write = output_string channel in
    write "<HTML>\n\t<body>\n";
    write (sprintf "\t\t<object type=\"image/svg+xml\" data=\"%s\" width=\"%d\" height=\"%d\">\n\t\t</object >\n" svg_file width height);
    write "\t</body>\n</HTML>";
    close_out channel

let () =
    let args = read_args () in
    printf "ARGS :\n%s\n" (string_of_args args);
    	
    match args.mode with
	| "upgma-dendogram" ->
		  let coll = Genotypes.read_file args.input in
		  printf "%d genotypes read (size = %d)\n"
			  coll.Genotypes.size coll.Genotypes.geno_size;
		  let dmat = GenoMat.createF coll in
		  let dendogram = Clustering.upgma dmat coll in
		   
	      let links = get_links args coll in
	      let name = sprintf "%supgma-%s" args.dir args.id_session in
	      let svg_file = name ^ ".svg" in

		  let width, height =
			  Dendogram.write_svg_file
				  ~links_info:(Some (links, args.target))
				  svg_file
				  dendogram in
	      
		  write_html
			  ((sprintf "upgma-%s.svg" args.id_session))
			  (name ^ ".html")
			  (width, height)
		  
	| "mst-phylogram" ->
		  let coll = Genotypes.remove_duplicates
			  (Genotypes.read_file args.input) in
		  printf "%d genotypes read (size = %d)\n"
			  coll.Genotypes.size coll.Genotypes.geno_size;
		  let dmat = GenoMat.create coll in
		  let tree = Tree.prim_complete coll dmat in
		  let fig = Phylogram.radial_layout ~reframe:false 800. tree in
		   
	      (* Creates force array, bodies *)
		  let n = Phylogram.size fig in
		  let fs = Array.init n (fun _ -> Vec2.null ()) in
          let fs' = Array.init n (fun _ -> Vec2.null ()) in
		  let bs = Array.map ForceDirectedLayout.body_of_pos fig.ps in

		  for i=0 to 200 do
		      update_state n fs fs' bs fig
		  done;

		  let x0, y0 = (10.,10.) in
		  unsafe_reframe (10.,10.) fig.ps;
		  unsafe_crop_width (800.-.2.*.x0) fig.ps;
		  fig.h <- height fig.ps +. 2. *. y0;

	      let links = get_links args coll in
	      let name = sprintf "%smst-%s" args.dir args.id_session in
	      let svg_file = name ^ ".svg" in

		  let width, height = fig.Phylogram.w, fig.Phylogram.h in
		  Phylogram.write_svg_file
			  ~links_info:(Some (links, args.target))
			  coll
			  fig
			  svg_file;
	      
		  write_html
			  ((sprintf "mst-%s.svg" args.id_session))
			  (name ^ ".html")
			  (int_of_float width, int_of_float height)

	| _ ->
		  failwith "Mode invalide."
