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

open Ezcmd.V2
open EZCMD.TYPES

module TYPES = struct
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
open TYPES

module GETWALLETADDRESS = struct
  type reply = {
    value0 : string
  } [@@deriving json_encoding]
end

let destruct name enc s =
  match EzEncoding.destruct enc s with
  | exception exn ->
      Printf.eprintf "Cannot parse JSON:\n%s\n%!" s;
      Printf.eprintf "Error: %s\n%!" (Printexc.to_string exn);
      Error.raise "Cannot parse %s" name
    | t -> t

let manifest =
  lazy (
    let manifest = match Files.read "manifest.json" with
      | None -> assert false
      | Some manifest -> manifest
    in
    destruct "manifest file" Types.MANIFEST.enc manifest
  )

let get_context config =
  let net = Config.current_network config in
  let client, server_url =
    let node = Config.current_node config in
    let client = Ton_sdk.CLIENT.create node.node_url in
    let server_url = node.node_url in
    client, server_url
  in
  let root_contract_abi =
    Misc.get_contract_abifile "Broxus_RootTokenContract" in
  let root_contract_abi = EzFile.read_file root_contract_abi in
  let wallet_contract_abi =
    Misc.get_contract_abifile "Broxus_TONTokenWallet" in
  let wallet_contract_abi = EzFile.read_file wallet_contract_abi in
  let manifest = Lazy.force manifest in
  {
    config;
    net;
    manifest;
    client;
    server_url;
    root_contract_abi;
    wallet_contract_abi;
  }

let get_token_wallet_address ctxt token address =
  let root_address = token.Types.MANIFEST.token_address in
  let params = Printf.sprintf {|{ "_answer_id": 1,
                                      "wallet_public_key_": 0,
                                      "owner_address_": "%s"}|}
      address
  in
  let reply =
    Utils.call_run
      ctxt.config
      ~client:ctxt.client
      ~wait:false
      ~server_url:ctxt.server_url
      ~address:root_address
      ~abi:ctxt.root_contract_abi
      ~meth:"getWalletAddress"
      ~params
      ~local:true
      ()
  in
  let address = (
    destruct "getWalletAddress reply" GETWALLETADDRESS.reply_enc reply
  ).value0
  in
  address

let get_token_by_symbol ctxt symbol =
  let rec iter = function
      [] ->
        Error.raise "token with symbol %S not found\n%!" symbol
    | token :: tokens ->

        if token.Types.MANIFEST.token_symbol = symbol then
          token
        else iter tokens
  in
  iter ctxt.manifest.Types.MANIFEST.tokens

let get_token_balance_gas ctxt wallet_address =
  let info = CommandAccountState.get_address_info ctxt.config
      (RawAddress wallet_address) in
  match info with
  | None -> None
  | Some acc ->
      let gas =
        match acc.acc_balance with
        | None -> 0L
        | Some n -> Z.to_int64 n
      in
      let params = Printf.sprintf {|{ "_answer_id": 1 }|} in
      let reply =
        Utils.call_run ctxt.config
          ~client:ctxt.client
          ~wait:false
          ~server_url:ctxt.server_url
          ~address:wallet_address
          ~abi:ctxt.wallet_contract_abi
          ~meth:"balance"
          ~params
          ~local:true
          ()
      in
      let balance = (
        destruct "balance reply" GETWALLETADDRESS.reply_enc reply
      ).value0
      in
      let balance = Int64.of_string balance in
      Some ( balance, gas )

let print_wallet config account =
  let ctxt = get_context config in
  let key = Misc.find_key_exn ctxt.net account in
  let contract = CommandMultisigCreate.check_key_contract key in
  let address = Misc.get_key_address_exn key in
  Printf.printf "%s (at %s, %s):\n%!" account address contract;
  let manifest = Lazy.force manifest in

  List.iter (fun token ->

      let wallet_address = get_token_wallet_address ctxt token address in

      match get_token_balance_gas ctxt wallet_address with
      | None -> ()
      | Some ( balance, gas ) ->
          Printf.printf "  %s\n%!" token.Types.MANIFEST.token_name ;
          Printf.printf "    address: %s\n%!" wallet_address;
          Printf.printf "    balance %s %s (gas %s TON)\n%!"
            ( Misc.string_of_nanoton balance )
            token.Types.MANIFEST.token_symbol
            ( Misc.string_of_nanoton gas )
    ) manifest.Types.MANIFEST.tokens

let cmd =
  let args = ref [] in
  Misc.cmd
    ["token"; "list"]
    (fun () ->
       let config = Config.config () in
       List.iter ( print_wallet config ) !args;
       ()
    )
    ~args:
      [
        [], Arg.Anons ( fun list -> args := list),
        EZCMD.info ~docv:"ACCOUNT" "The associated multisig wallet";
      ]
    ~doc: "Display token wallet info"
    ~man:[
    ]
