
val database : unit -> string

val upgrades : (int * (unit PGOCaml.t -> int -> unit)) list
val downgrades : (int * string list) list
