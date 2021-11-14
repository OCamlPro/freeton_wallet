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

type t

val check_account :
  ?unknown:string list ref -> Types.config -> abis:t -> address:string -> unit
val replace_addr : abis:t -> address:string -> string
val create : Types.config -> abis:string list -> t

val parse_message_body :
  client:Ton_sdk.TYPES.client ->
  abis:t ->
  Ton_sdk.ENCODING.message ->
  string option
