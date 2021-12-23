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

val inspect_account_past :
  level:int ->
  ?limit:int ->
  abis:string list ->
  ?f:(n:int ref ->
      abis:AbiCache.t ->
      level:int ->
      Ton_sdk.ENCODING.transaction *
      (Ton_sdk.ENCODING.message * string option) option *
      (Ton_sdk.ENCODING.message * string option) list ->
      unit) ->
  ?queue:string list ref -> string -> unit

val simplify_message :
  abis:AbiCache.t ->
  level:int -> Ton_sdk.ENCODING.message -> Ton_sdk.ENCODING.message
