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

val call_contract :
  Types.config ->
  address:string ->
  contract:string ->
  meth:string ->
  params:string ->
  ?client:Ton_sdk.TYPES.client ->
  ?src:Types.key ->
  ?local:bool ->
  ?subst:(msg:string -> Types.config -> string -> unit) ->
  ?wait:bool -> (* false by default *)
  unit -> unit

val post :
  Types.config -> 'a Ton_request.t -> 'a

val post_lwt :
  Types.config -> 'a Ton_request.t -> 'a Lwt.t

val deploy_contract :
  Types.config ->
  key:Types.key ->
  ?sign:Types.key ->
  contract:string -> params:string -> wc:int option ->
  ?initial_data: string ->
  ?initial_pubkey: string ->
  ?client:Ton_sdk.TYPES.client ->
  unit -> unit

val tonoscli : Types.config -> string list -> string list

val address_of_account : Types.config -> string -> Types.address
val abi_of_account : Types.config -> string -> string option (* content *)

(* returns Some (exists, balance) if exists, None otherwise *)
val get_account_info : Types.config -> string -> ( bool * Z.t ) option

val show_abi : contract:string -> string
