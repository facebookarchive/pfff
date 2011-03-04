(** Database interface to the Ocsimore database. *)

module PGOCaml : PGOCaml_generic.PGOCAML_GENERIC with type 'a monad = 'a Lwt.t


type db_t = PGOCaml.pa_pg_data PGOCaml.t

(* val connect: unit -> db_t Lwt.t *)

(** Pool of SQL connections *)
val pool : db_t Lwt_pool.t

(** Perform an atomic transaction (using BEGIN and COMMIT/ROLLBACK *)
val transaction_block : db_t -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** Same as [transaction_block] but takes a db connection in the pool. *)
val full_transaction_block : (db_t -> 'a Lwt.t) -> 'a Lwt.t
