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
  ?client:Ton_types.client ->
  ?src:Types.key ->
  ?local:bool ->
  ?output:string ->
  unit -> unit

val post :
  Types.config -> 'a Ton_request.t -> 'a

val deploy_contract :
  Types.config ->
  key:Types.key ->
  contract:string -> params:string -> wc:int option ->
  ?client:Ton_types.client ->
  unit -> unit


val tonoscli : Types.config -> string list -> string list

val address_of_account : Types.config -> string -> string
val abi_of_account : Types.config -> string -> string option (* content *)
