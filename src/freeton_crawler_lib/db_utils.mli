(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module PGOCaml = PGOCaml_lwt

type database_handle = (string, bool) Hashtbl.t PGOCaml_lwt.t

val database : string ref
val create : unit -> unit

val ( let>>> ) :  'a -> (database_handle -> 'b Lwt.t) -> 'b Lwt.t

val transaction : (database_handle -> 'a Lwt.t) -> 'a Lwt.t

val ( let> ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
