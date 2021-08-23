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

val cmd : Ezcmd.V2.EZCMD.TYPES.sub

val list_contracts : unit -> unit
val known_contracts : unit -> string EzCompat.StringMap.t

val create_new_version : string -> int -> string
val get_current_version : string -> int option
val same_file : string -> string -> bool

val action :
  filename:string -> force:bool -> ?contract:string -> unit -> unit

val preprocess_solidity :
  ?old_file:string -> from_:string -> to_:string -> unit ->
  string list * bool
