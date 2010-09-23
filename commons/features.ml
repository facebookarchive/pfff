# 1 "commons/features.ml.in"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "commons/features.ml.in"
(* yes sometimes cpp is useful *)

(* old:
note: in addition to Makefile.config, globals/config.ml is also modified
by configure
features.ml: features.ml.cpp Makefile.config
	cpp -DFEATURE_GUI=$(0)             -DFEATURE_MPI=$(0) 	    -DFEATURE_PCRE=$(0) 	   features.ml.cpp > features.ml




clean::
	rm -f features.ml

beforedepend:: features.ml
*)

# 42 "commons/features.ml.in"

module Distribution = struct
  let map_reduce ?timeout ~fmap:map_ex ~freduce:reduce_ex acc xs = 
    let not_done = [] in
    List.fold_left reduce_ex acc (List.map map_ex xs), not_done

  let map_reduce_lazy ?timeout ~fmap:map_ex ~freduce:reduce_ex acc fxs = 
    let xs = fxs() in (* changed code *)
    let not_done = [] in
    List.fold_left reduce_ex acc (List.map map_ex xs), not_done


  let under_mpirun () = 
    false
      
  let set_debug_mpi () = 
    ()

end















module Backtrace = struct
 let print () = 
   print_string "no backtrace support, use configure --with-backtrace\n"
end

