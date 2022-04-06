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

open Ez_file.V1
open EzCompat (* for StringMap *)

open Types.MULTISIG


module TYPES = struct
  type ctxt = {
    config : Types.config;
    net : Types.network;
    client : Sdk_types.client ;
    server_url : string ;
    multisig_address : string ;
    multisig_contract : string ;
    multisig_contract_abi : string ;
  }
end
open TYPES


let get_context config account =
  let net = Config.current_network config in
  let client, server_url =
    let node = Config.current_node config in
    let client = Ton_sdk.CLIENT.create node.node_url in
    let server_url = node.node_url in
    client, server_url
  in
  let multisig_address, multisig_contract =
    match Misc.is_address account with
    | Some address -> address, "SafeMultisigWallet"
    | None ->
        let account_key = Misc.find_key_exn net account in
        let account_addr = Misc.get_key_address_exn account_key in
        let account_contract =
          CommandMultisigCreate.check_key_contract account_key in
        ( account_addr, account_contract )
  in

  let multisig_contract_abi =

    let abifile = Misc.get_contract_abifile multisig_contract in
    EzFile.read_file abifile
  in
  {
    config;
    net;
    client;
    server_url;
    multisig_contract;
    multisig_contract_abi;
    multisig_address ;
  }

let call ctxt ?(params="{}") ?(local=true) ?keypair ?subst meth encoding =
  Printf.eprintf "Multisig: calling %s %s\n%!" ctxt.multisig_address meth;
  let reply =
    Utils.call_run
      ctxt.config
      ~client:ctxt.client
      ~wait:false
      ~server_url:ctxt.server_url
      ~address:ctxt.multisig_address
      ~abi:ctxt.multisig_contract_abi
      ~meth
      ~params
      ~local
      ?keypair
      ()
  in
  begin
    match subst with
    | Some subst -> subst ~msg:"call result" ctxt.config reply
    | None ->
        if Misc.verbose 2 then
          Printf.printf "call result:\n%s\n%!" reply
  end;
  CommandTokenList.destruct meth encoding reply

let get_custodians ctxt =
  let r = call ctxt "getCustodians" Types.MULTISIG.custodians_enc in
  r.custodians

let get_transactions ctxt =
  let r = call ctxt "getTransactions" Types.MULTISIG.transactions_enc in
  r.transactions

let get_parameters ctxt =
  call ctxt "getParameters" Types.MULTISIG.parameters_enc

let name_by_pubkey net =
  let map = ref StringMap.empty in
  List.iter (fun key ->
      match key.Types.key_pair with
      | None -> ()
      | Some pair ->
          map := StringMap.add ( "0x" ^ pair.public ) key.key_name !map
    ) net.Types.net_keys;
  !map


let custodian_by_index ~name_by_pubkey custodians =

  let map = ref StringMap.empty in
  List.iter (fun c ->
      let name =
        match StringMap.find c.pubkey name_by_pubkey with
        | exception Not_found -> c.pubkey
        | name ->
            Printf.sprintf "%s (%s)" name c.pubkey
      in
      map := StringMap.add c.index name !map
    ) custodians ;
  !map


let get_updates ctxt =
  let r = call ctxt "getUpdateRequests" Types.MULTISIG.updates_enc in
  r.updates

let string_of_seconds delay =
  let hours = delay / 3600 in
  let secs = delay - hours * 3600 in
  let mins = secs / 60 in
  let secs = secs - mins * 60 in
  Printf.sprintf "%dh%dm%ds" hours mins secs
