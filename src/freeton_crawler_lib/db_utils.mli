

module PGOCaml = PGOCaml_lwt

type database_handle = (string, bool) Hashtbl.t PGOCaml_lwt.t

val database : string ref
val create : unit -> unit

val ( let>>> ) :  'a -> (database_handle -> 'b Lwt.t) -> 'b Lwt.t

val transaction : (database_handle -> 'a Lwt.t) -> 'a Lwt.t

val ( let> ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
