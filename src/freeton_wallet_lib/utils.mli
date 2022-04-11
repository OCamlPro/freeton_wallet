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

open Types

val call_contract :
  Types.config ->
  address:ADDRESS.t ->
  contract:string ->
  meth:string ->
  params:string ->
  ?client:Ton_sdk.TYPES.client ->
  ?src:Types.key ->
  ?local:bool ->
  ?subst:(msg:string -> Types.config -> string -> unit) ->
  ?wait:bool -> (* false by default *)
  ?accounts:(string * ADDRESS.t * string) list ->
  unit -> unit

val post :
  Types.config -> 'a Ton_sdk.REQUEST.t -> 'a

val post_lwt :
  Types.config -> 'a Ton_sdk.REQUEST.t -> 'a Lwt.t

val deploy_contract :
  Types.config ->
  key:Types.key ->
  ?sign:Types.key ->
  contract:string -> params:string -> wc:int option ->
  ?initial_data: string ->
  ?initial_pubkey: PUBKEY.t ->
  ?client:Ton_sdk.TYPES.client ->
  unit -> unit

val tonoscli : Types.config -> string list -> string list

val address_of_account : Types.network -> string -> Types.address
val abi_of_account : Types.config -> string -> string option (* content *)

(* returns Some (exists, balance) if exists, None otherwise *)
val get_account_info : Types.config -> ADDRESS.t -> ( bool * Z.t ) option

val show_abi : contract:string -> string

val call_run :
  Types.config ->
  ?client:Sdk_types.client ->
  wait:bool ->
  server_url:string ->
  address:ADDRESS.t ->
  abi:string ->
  meth:string ->
  params:string ->
  local:bool ->
  ?key_pair:key_pair ->
  ?accounts:(string * ADDRESS.t * string) list ->
  unit ->
  string
