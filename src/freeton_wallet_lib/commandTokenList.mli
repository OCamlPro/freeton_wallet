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

module TYPES : sig
  type ctxt = {
    config : Types.config;
    net : Types.network;
    manifest : Types.MANIFEST.t;
    client : Sdk_types.client ;
    server_url : string ;
    root_contract_abi : string ;
    wallet_contract_abi : string ;
  }
end

val cmd : string list * Ezcmd.V2.EZCMD.TYPES.sub
val destruct : string -> 'a Json_encoding.encoding -> string -> 'a
val get_context : Types.config -> TYPES.ctxt
val get_token_wallet_address :
  TYPES.ctxt -> Types.MANIFEST.token -> string -> string
val get_token_by_symbol :
  TYPES.ctxt -> string -> Types.MANIFEST.token
val get_token_balance_gas :
  TYPES.ctxt -> string -> (int64 * int64) option
