open Common 

let server () = 
  let numworkers = Mpi.comm_size Mpi.comm_world - 1 in
  begin

    pr2 ("server");
  end

let worker rank = 
  begin
    pr2 (spf "worker: %d" rank);
  end

let main () = 
  match Array.to_list Sys.argv with
  | xs -> 
      let rank = Mpi.comm_rank Mpi.comm_world in
      if rank = 0
      then begin 
        xs +> List.iter pr2;
        server ()
      end
      else begin
        xs +> List.iter pr2;
        worker rank 
      end
      (* Mpi.barrier Mpi.comm_world *)

  | _ -> failwith "not enough arg"


let _ = 
  main ()
