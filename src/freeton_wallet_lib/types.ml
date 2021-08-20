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

type account = {
  acc_address : string ;                 [@key "address"]
  mutable acc_contract : string option ; [@key "contract"]
  mutable acc_workchain : int option ;   [@key "workchain"]
  mutable acc_static_vars : string option ; [@key "static_vars"]
} [@@deriving json_encoding]

type key = {
  mutable key_name : string ;                   [@key "name"]
  mutable key_passphrase : string option ;      [@key "passphrase"]
  mutable key_pair : Ton_sdk.TYPES.keypair option ; [@key "pair"]
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
  | RawAddress of string
  | Account of account


module MULTISIG = struct

  type transaction = {
    id : string ;
    confirmationsMask : string ;
    signsRequired : string ;
    signsReceived : string ;
    creator : string ; (* 0x pubkey *)
    index : string ;  (* index of custodian creator *)
    dest : string ;
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
    pubkey : string ;
  }
  [@@deriving json_encoding]

  type custodians = {
    custodians : custodian list ;
  }
  [@@deriving json_encoding]

end
