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

type t

val check_account :
  ?unknown:string list ref -> Types.config -> abis:t -> address:ADDRESS.t -> unit

(* a better version of Misc.string_of_address *)
val replace_addr : abis:t -> address:ADDRESS.t -> string
val create :
  ?abis:string list ->
  ?accounts:(string * ADDRESS.t * string) list -> (* name, address, contract *)
  Types.config -> t

type message_body = {
  m_address : ADDRESS.t ;
  m_contract_name : string ;
  m_body_type : string ;
  m_body_name : string ;
  m_body_args : string ;
}

val parse_message_body :
  client:Sdk_types.client ->
  abis:t ->
  Ton_sdk.ENCODING.message -> (string * message_body option) option

val string_of_message_body :
  (string * message_body option) option -> string
