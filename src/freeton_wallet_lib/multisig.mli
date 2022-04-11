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

module TYPES : sig
  type ctxt = {
    config : Types.config;
    net : Types.network;
    client : Sdk_types.client ;
    server_url : string ;
    multisig_address : ADDRESS.t ;
    multisig_contract : string ;
    multisig_contract_abi : string ;
  }
end

val get_context : Types.config -> string -> TYPES.ctxt

val get_custodians : TYPES.ctxt -> Types.MULTISIG.custodian list
val get_transactions : TYPES.ctxt -> Types.MULTISIG.transaction list
val get_updates : TYPES.ctxt -> Types.MULTISIG.update list
val get_parameters : TYPES.ctxt -> Types.MULTISIG.parameters

(* pubkeys start with 0x *)
val name_by_pubkey : Types.network -> JSON_PUBKEY.t -> string
val custodian_by_index :
  name_by_pubkey:( JSON_PUBKEY.t -> string ) ->
  Types.MULTISIG.custodian list -> string EzCompat.StringMap.t

val string_of_seconds : int -> string

val call :
  TYPES.ctxt ->
  ?params:string ->
  ?local:bool ->
  ?key_pair:key_pair ->
  ?subst:(msg:string -> Types.config -> string -> unit) ->
  string -> 'a Json_encoding.encoding -> 'a
