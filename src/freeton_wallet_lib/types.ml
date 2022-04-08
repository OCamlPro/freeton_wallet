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

module ADDRESS : sig

  type t
  val to_string : t -> string
  val of_string : string -> t
  val parse_string : string -> t
  val t_enc : t Json_encoding.encoding
  val not_empty : t -> bool

end = struct

  type t = string  [@@deriving json_encoding]

  let to_string t = t
  let of_string t = t
  let parse_string t = t (* TODO: check conformity *)
  let t_enc = enc
  let not_empty t = t <> ""
end

module JSON_PUBKEY : sig

  type t
  val to_string : t -> string
  val of_string : string -> t
  val parse_string : string -> t
  val t_enc : t Json_encoding.encoding

end = struct

  type t = string  [@@deriving json_encoding]

  let to_string t = t
  let of_string t = t
  let parse_string t = t (* TODO: check conformity *)
  let t_enc = enc
end

module PUBKEY : sig (* without 0x *)

  type t
  val to_string : t -> string
  val to_json_string : t -> string
  val of_string : string -> t
  val parse_string : string -> t
  val to_json : t -> JSON_PUBKEY.t
  val of_json : JSON_PUBKEY.t -> t
  val t_enc : t Json_encoding.encoding

end = struct

  type t = string  [@@deriving json_encoding]

  let to_string t = t
  let of_string t = t
  let parse_string t = t (* TODO: check conformity *)
  let to_json_string t = "0x" ^ t
  let to_json t = JSON_PUBKEY.of_string ( to_json_string t )
  let of_json t =
    let t = JSON_PUBKEY.to_string t in
    assert ( String.length t = 66 );
    String.sub t 0 64
  let t_enc = enc
end

type key_pair = { (* typed version of Ton_sdk.TYPES.keypair *)
  public : PUBKEY.t ;
  mutable secret : string option ;
} [@@deriving json_encoding]

let keypair_of_key_pair {public; secret } =
  Ton_sdk.TYPES.{ public = PUBKEY.to_string public; secret }

let key_pair_of_keypair Ton_sdk.TYPES.{public; secret } =
  { public = PUBKEY.of_string public; secret }

type account = {
  acc_address : ADDRESS.t ;                 [@key "address"]
  mutable acc_contract : string option ; [@key "contract"]
  mutable acc_workchain : int option ;   [@key "workchain"]
  mutable acc_static_vars : string option ; [@key "static_vars"]
} [@@deriving json_encoding]

type key = {
  mutable key_name : string ;                   [@key "name"]
  mutable key_passphrase : string option ;      [@key "passphrase"]
  mutable key_pair : key_pair option ; [@key "pair"]
  mutable key_account : account option ;        [@key "account"]
} [@@deriving json_encoding]

type local_node = {
  local_port : int ;
} [@@deriving json_encoding]

type node = {
  node_name : string ; [@key "name"]
  mutable node_url : string ;  [@key "url" ]
  node_local : local_node option ;
} [@@deriving json_encoding]

type network = {
  net_name : string ;                       [@key "name"]
  mutable current_node : string ;           [@key "node"]
  mutable current_account : string option ; [@key "account"]
  mutable net_nodes : node list ;           [@key "nodes"]
  mutable net_keys : key list ;   [@dft []] [@key "keys"]
  mutable net_deployer : string ; [@dft "deployer"] [@key "deployer"]
  mutable net_toolchain : string ; [@dft ""]
} [@@deriving json_encoding]

type repos = {
  mutable repo_toolchain : string ; [@dft ""]
  mutable repo_tonos_cli : string ; [@dft ""]
  mutable repo_solc : string ; [@dft ""]
  mutable repo_tvm_linker : string ; [@dft ""]
} [@@deriving json_encoding]

type config = {
  mutable version : int ;             [@dft 0]
  mutable modified : bool ;           [@dft true]
  mutable current_network : string ;  [@key "network"]
  mutable networks : network list ;
  mutable repos : repos option ;
  mutable toolchains : repos list ;   [@dft []]
  mutable multisigs : string list ;   [@dft []]
} [@@deriving json_encoding]

type address =
  | RawAddress of ADDRESS.t
  | Account of account


module MULTISIG = struct

  type transaction = {
    id : string ;
    confirmationsMask : string ;
    signsRequired : string ;
    signsReceived : string ;
    creator : JSON_PUBKEY.t ; (* 0x pubkey *)
    index : string ;  (* index of custodian creator *)
    dest : ADDRESS.t ;
    value : string;
    sendFlags : string;
    payload : string ;
    bounce : bool;
  } [@@deriving json_encoding]

  type transactions = {
    transactions : transaction list ;
  }
  [@@deriving json_encoding]

  type custodian = {
    index : string ;
    pubkey : JSON_PUBKEY.t ;
  }
  [@@deriving json_encoding]

  type custodians = {
    custodians : custodian list ;
  }
  [@@deriving json_encoding]

  type update = {
    update_id : string ;
    update_index : string ; (* uint8 *)
    update_signs : string ; (* uint8 *)
    update_confirmationsMask : string ; (* uint32 *)
    update_creator : JSON_PUBKEY.t ; (* pubkey *)
    update_codeHash : string ;
    update_custodians : JSON_PUBKEY.t list ;
    update_reqConfirms : string ; (* uint8 *)
  }
  [@@deriving json_encoding]

  type updates = {
    updates : update list ;
  }
  [@@deriving json_encoding]

  type parameters = {
    maxQueuedTransactions : string ; (* uint8 *)
    maxCustodianCount : string ; (* uint8 *)
    expirationTime : string; (* uint64 *)
    minValue : string ; (* uint128 *)
    requiredTxnConfirms : string ; (* uint8 *)
    requiredUpdConfirms : string ; [@dft "0" ] (* uint8 *)
  }
  [@@deriving json_encoding]

  type submitUpdate_reply = {
    updateId : string ;
  }
  [@@deriving json_encoding]

end

module MANIFEST = struct

  type token = {
    token_name : string ;
    token_chainId : int ;
    token_symbol : string ;
    token_decimals : int ;
    token_address : ADDRESS.t ;
    token_logoURI : string option ;
    token_version : int ;
  }
  [@@deriving json_encoding]

  type version = {
    major : int ;
    minor : int ;
    patch : int ;
  }
  [@@deriving json_encoding]

  type t = {
    schema : string ; [@key "$schema"]
    name : string ;
    version : version ;
    keywords : string list ;
    timestamp : string ;
    tokens : token list ;
  }
  [@@deriving json_encoding]

end
