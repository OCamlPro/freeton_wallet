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

val create_multisig :
  ?client:Ton_sdk.TYPES.client -> ?custodians:string list ->
  ?not_owner:bool -> ?req:int -> ?wc:int -> ?contract:string -> string -> unit

val check_key_contract : Types.key -> string
