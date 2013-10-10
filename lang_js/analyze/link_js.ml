
open Common
open Module_js

(*******************)
(* name resolution *)
(*******************)

let rec resolve_ stack modules shape = 
	match shape with

		| FunctionShape (_,shape) -> resolve_ stack modules shape

		| ObjectShape {contents = (id,map,maps)} -> 
			if (List.mem id stack) then () else
			map +> SMap.iter (fun _ -> resolve_ (id::stack) modules);
			maps +> List.iter (resolve_ (id::stack) modules)
	
		| ClassShape (instance, static) -> 
			resolve_ stack modules instance; resolve_ stack modules static		
	
		| RequireShape (m) -> 
			if SMap.mem m modules 
			then ()
			else failwith ("\n\nUNRESOLVED LINK " ^ m ^ "!")

		| PropertyShape (shape,_) -> resolve_ stack modules shape
	
		| NewShape _class -> resolve_ stack modules _class

		| MixinShape map -> resolve_ stack modules map

		| ClassWithMixinShape (_class, mixin) -> 
			resolve_ stack modules _class; resolve_ stack modules mixin	

		| ApplyShape _function -> resolve_ stack modules _function

		| LiteralShape
		| ArrayShape
		| UnknownShape _
		| ElementShape _ -> ()

let resolve modules shape = resolve_ [] modules shape

(*******************************************)
(* topological sort of module dependencies *)
(*******************************************)

let missing_imports modules =
	prerr_newline();
	let unbound_modules = ref SMap.empty in
	modules +> SMap.iter
		(fun _ -> fun local -> local.local_requires +> List.iter
			(fun require -> 
				if (SMap.mem require modules)
				then ()
				else unbound_modules := !unbound_modules +> SMap.add require ()
			)
		);
	!unbound_modules +> SMap.iter
		(fun m -> fun _ -> prerr_endline (spf "MISSING %s" m))	

type topsort_state = {
	mutable ordering: int smap;
	mutable visited: moduleinfo_map;
	mutable norder: int;
	mutable stack: string list;
}

let tsort = {
	ordering = SMap.empty;
	visited = SMap.empty;
	norder = 0;
	stack = [];
}

let rec topsort modules =
	tsort.visited <- modules;
	loop()

and loop () =
	try
		let (m,_) = SMap.choose tsort.visited in
		visit m;
		loop()
	with _ -> ()

and visit m =
	if (SMap.mem m tsort.visited) then (
		let local = SMap.find m tsort.visited in
		tsort.stack <- m :: tsort.stack;
		tsort.visited <- SMap.remove m tsort.visited;
		List.iter visit local.local_requires;
		tsort.ordering <- tsort.ordering +> 
			SMap.add m (tsort.norder <- tsort.norder + 1; tsort.norder);
		tsort.stack <- List.tl tsort.stack
	)
	else if (List.mem m tsort.stack) then (
		prerr_endline (spf "CYCLE %s" m);
		List.iter (fun m -> prerr_endline (spf "\t%s" m)) tsort.stack
	)
	else () 

let index m =
	try SMap.find m tsort.ordering
	with _ -> 0

let validate_dependency m_from m_to =
	try
		assert ((index m_from) >= (index m_to))
	with _ ->
		prerr_endline (spf "Inconsistent index: %s[%d] -> %s[%d]" 
			m_from
			(index m_from)
			m_to
			(index m_to)
		)


let validate_topsort modules =
	modules +> SMap.iter (
		fun m -> fun local -> 
			local.local_requires +> List.iter (validate_dependency m)
	)

	
