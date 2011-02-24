(* $Id$ *)

open Netsys_oothr

exception Thread_val of Thread.t
exception Mutex_val of Mutex.t
exception Condition_val of Condition.t

let mtthread t : thread =
  ( object
      method id = Thread.id t
      method join() = Thread.join t
      method repr = Thread_val t
    end
  )

let mtmutex t : mutex =
  ( object(self)
      method lock() = 
	(* Printf.eprintf "LOCK oid=%d\n%!" (Oo.id self); *)
	Mutex.lock t
      method unlock() = 
	(* Printf.eprintf "UNLOCK oid=%d\n%!" (Oo.id self); *)
	Mutex.unlock t
      method try_lock() = Mutex.try_lock t
      method repr = Mutex_val t
    end
  )

let mtcondition c : condition =
  ( object
      method wait mobj =
	let m =
	  match mobj#repr with
	    | Mutex_val m -> m
	    | _ -> failwith "Netsys_oothr_mt: inconsistent use" in
	Condition.wait c m

      method signal() = Condition.signal c
      method broadcast() = Condition.broadcast c
      method repr = Condition_val c
    end
  )

let mtprovider() : mtprovider =
  ( object
      method single_threaded = false
      method create_thread : 's 't . ('s -> 't) -> 's -> thread =
	fun f arg ->
	  mtthread(Thread.create f arg)
      method self = mtthread(Thread.self())
      method yield() = Thread.yield()
      method create_mutex() = mtmutex(Mutex.create())
      method create_condition() = mtcondition(Condition.create())
    end
  )

let init() =
  Netsys_oothr.provider := mtprovider()
