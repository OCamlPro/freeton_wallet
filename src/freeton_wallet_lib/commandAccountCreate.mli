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

val genkey :
  ?name:string -> ?contract:string ->
  ?initial_data:string ->
  ?initial_pubkey:string ->
  Types.config -> force:bool -> unit

val gen_address :
  Types.config -> Ton_sdk.TYPES.keypair ->
  string ->
  ?initial_data:string ->
  ?initial_pubkey:string ->
  ?wc:int -> unit ->
  string

val change_account :
  Types.config ->
  name:string ->
  ?passphrase:string ->
  ?address:string ->
  ?contract:string ->
  ?initial_data:string ->
  ?initial_pubkey:string ->
  ?keyfile:string -> ?wc:int -> unit -> unit
