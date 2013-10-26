
open Common
open Module_js

(********)
(* main *)
(********)

type special_paths = {
	modules_db: string;
	trees_db: string;
	ignore_list: string;
}

let paths = {
	modules_db = "/dev/shm/js_modules.db";
	trees_db = "/dev/shm/js_trees.db";
	ignore_list = "lang_js/analyze/stats_js.ignore";
}

let load_trees xs = 
	Utils_js.load paths.trees_db (fun () ->
		let files = Lib_parsing_js.find_source_files_of_dir_or_files xs in
    	let prefix = Sys.getenv "STATS_JS_PREFIX" in
    	let ignore_files =
    		cat paths.ignore_list +> 
    			List.map (fun s -> Filename.concat prefix s)
    	in
	    files 
    	  +> exclude (fun file -> 
        	    (List.mem file ignore_files) ||
            	(file =~ ".*tests__.*") ||
            	(file =~ ".*benchmarks__.*") ||
            	(file =~ ".*mocks__.*")
         	 )
      	  +> List.fold_left (fun trees -> fun file ->
      	  		try 
	        		let (parseinfo,_) = Parse_js.parse file in 
    	    		print_endline file;
        			trees +> SMap.add file parseinfo
        		with exc ->
            		prerr_endline (spf "\n###\n%s\n\n%s\n###\n" 
            			file 
            			(Printexc.to_string(exc))
            		);
            		trees
    	    ) SMap.empty
	)

let load_modules xs =
	Utils_js.load paths.modules_db (fun () ->
		let trees = load_trees xs in
		SMap.fold (fun file -> fun parseinfo -> fun modules ->
			try 
				let shape = Toplevel_js.analyze file parseinfo in
				modules +> Toplevel_js.export_module shape
			with exc -> 
            	prerr_endline (spf "\n###\n%s\n\n%s\n###\n" 
            		file 
            		(Printexc.to_string(exc))
            	);
            	Printexc.print_backtrace stderr;
            	raise Exit
		) trees SMap.empty
	)	 

let print_info m modules = 
	let local = SMap.find m modules in
	let shape = SMap.find "exports" local.local_bindings in
	print_endline (string_of_shape 1 shape);
	print_endline "REQUIRES";
	List.iter (fun s -> print_endline(spf "\t%s" s)) local.local_requires

let main xs =

  let modules = load_modules xs in

  Link_js.missing_imports modules;

  let (nclasses,nfunctions,nobjects) = (ref 0, ref 0, ref 0) in
  modules +> SMap.iter
    (fun m -> function local -> match SMap.find "exports" local.local_bindings with
      | ClassShape _ ->
          nclasses := !nclasses + 1
      
      | FunctionShape _ ->
          nfunctions := !nfunctions + 1;

      | ObjectShape _ ->
          nobjects := !nobjects + 1;
          ()     
      
      | UnknownShape s ->
          print_endline (spf "UNKNOWN %s: %s" m s);
          ()     
      
      | LiteralShape 
      | NewShape _ 
      | ApplyShape _ 
      | RequireShape _
      | PropertyShape _ 
      | ArrayShape -> ()

      | shape -> 
          failwith (string_of_shape 0 shape)
    );
  print_newline();
  print_endline (spf "Classes: %d; Functions: %d; Objects: %d" 
    !nclasses
    !nfunctions
    !nobjects
  );

  print_newline();
  Link_js.topsort modules;
  Link_js.validate_topsort modules;

  modules +> SMap.iter (fun m -> fun local ->
  	let shape = SMap.find "exports" local.local_bindings in 
  	try 
		Link_js.resolve modules shape
  	with exc -> 
  		prerr_endline (spf "MODULE %s\n\t%s" 
  			m 
  			(Printexc.to_string exc)
  		)
  );
  

  let rec loop () =
    print_endline "\nInspect module?";
    let m = read_line() in
    (try print_info m modules with _ -> ());
    loop () 
  in
  loop()

(********)
(* TODO *)
(********)

(* global *)





