exception Out_of_range
exception Wrong_substraction

type 'a t = 'a array array
		
let mk_dist dist table =
	let len = Array.length table in
	let res = Array.create len [||] in
	for i=1 to len - 1 do
		res.(i) <- Array.create i (dist table.(i) table.(0));
		for j=0 to i-1 do
			res.(i).(j) <- dist table.(i) table.(j)
		done
	done;
	res
	
let create dim x =
	let res = Array.create dim [||] in
	for i=1 to dim - 1 do
		res.(i) <- Array.create i x
	done;
	res
		
let init dim f =
	let res = Array.create dim [||] in
	for i=1 to dim - 1 do
		res.(i) <- Array.create i (f i 0);
		for j=1 to i - 1 do
			res.(i).(j) <- f i j
		done;
	done;
	res
		
let copy = function
	| [|[||]|] -> [|[||]|]
	| a ->
		  let dim = Array.length a in
		  let res = Array.create dim [||] in
		  for i=1 to dim - 1 do
			  let first = a.(i).(0) in
			  res.(i) <- Array.create i first;
			  for j=1 to i - 1 do
				  res.(i).(j) <- a.(i).(j)
			  done
		  done;
		  res
			  
let iter (f : 'a -> unit) a =
	for i=1 to (Array.length a) - 1 do
		for j=0 to i - 1 do
			f a.(i).(j)
		done
	done
		
let iterij (f : int -> int -> 'a -> unit) a =
	for i=1 to (Array.length a) - 1 do
		for j=0 to i - 1 do
			f i j a.(i).(j)
		done
	done
		
let map (f : 'a -> 'b) = function
	| [|[||]|] -> [|[||]|]
	| a ->
		  let dim = Array.length a in
		  let res = Array.create dim [||] in
		  for i=1 to dim - 1 do
			  let first = f a.(i).(0) in
			  res.(i) <- Array.create i first;
			  for j=1 to i - 1 do
				  res.(i).(j) <- f a.(i).(j)
			  done
		  done;
		  res
			  
let modif (f : 'a -> 'b) matrix =
	for i=0 to (Array.length matrix) - 1 do
		for j=0 to i - 1 do
			matrix.(i).(j) <- f matrix.(i).(j)
		done;
	done
		
let mapij (f : int -> int -> 'a -> 'b) = function
	| [|[||]|] -> [|[||]|]
	| a ->
		  let dim = Array.length a in
		  let res = Array.create dim [||] in
		  for i=1 to dim - 1 do
			  let first = f i 0 a.(i).(0) in
			  res.(i) <- Array.create i first;
			  for j=1 to i - 1 do
				  res.(i).(j) <- f i j a.(i).(j)
			  done
		  done;
		  res
			  
let modifij (f : int -> int -> 'a -> 'a) matrix =
	for i=0 to (Array.length matrix) - 1 do
		for j=0 to i - 1 do
			matrix.(i).(j) <- f i j matrix.(i).(j)
		done;
	done
		
let fold_left (f : 'b -> 'a -> 'b) x a =
	if a = [|[||]|] then
		failwith "Empty_array"
	else (
		let res = ref x in
		let dim = Array.length a in
		for i=0 to dim - 1 do
			for j=0 to i - 1 do
				res := f !res a.(i).(j)
			done
		done;
		!res
	)
		
let fold_right (f : 'a -> 'b -> 'b) a x =
	if a = [|[||]|] then
		failwith "Empty _array"
	else (
		let res = ref x in
		let dim = Array.length a in
		for i = dim - 1 downto 0 do
			for j = i - 1 downto 0 do
				res := f a.(i).(j) !res
			done
		done;
		!res
	)
		
let get m i j =
	if i=j then invalid_arg "DistMat.get : out of range"
	else
		if i>j then m.(i).(j)
		else m.(j).(i)
			
let unsafe_get m i j =
	Array.unsafe_get (Array.unsafe_get m i) j

let set m i j x =
	if i=j then invalid_arg "DistMat.set : out of range"
	else
		if i>j then m.(i).(j) <- x
		else m.(j).(i) <- x

let unsafe_set m i j x =
	Array.unsafe_set (Array.unsafe_get m j) i x
		
(*Does the substraction of 2 matrices*)
let sub m1 m2 =
	let len1 = Array.length m1 and len2 = Array.length m2 in
	if len1 <> len2 then
		raise Wrong_substraction
	else
		init len1 (fun i j -> m1.(i).(j) - m2.(i).(j))

let max m =
	let dim = Array.length m in
	if dim = 0 || Array.length m.(0) == 0 then
		invalid_arg "DistMat.max"
	else
		let max = ref m.(1).(0) in
		for i=0 to dim - 1 do
			for j=0 to i-1 do
				if m.(i).(j) > !max then
					max := m.(i).(j)
			done
		done;
		!max
