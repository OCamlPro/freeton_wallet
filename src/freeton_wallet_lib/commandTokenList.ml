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
open EzFile.OP
open Ezcmd.V2
open EZCMD.TYPES

(*
module Int64 = struct
  include Int64
  let of_string s = try
      of_string s
    with exn ->
      Printf.eprintf "Failure on %S\n%!" s ;
      raise exn
end
*)

module TYPES = struct
  type ctxt = {
    config : Types.config;
    net : Types.network;
    manifest : Types.MANIFEST.t;
    client : Sdk_types.client ;
    server_url : string ;
    root_contract_abi : string ;
    wallet_contract_abi : string ;
    vault_address : string ;
    dexroot_address : string ;
    dexroot_contract_abi : string ;
    dexpair_contract_abi : string ;
  }
end
open TYPES

module GETWALLETADDRESS = struct
  type reply = {
    value0 : string
  } [@@deriving json_encoding]
end

module EXPECTEDEXCHANGE = struct
  type reply = {
    expected_amount : string; [@key "expected_amount" ]
    expected_fee : string; [@key "expected_fee" ]
  } [@@deriving json_encoding]
end

module GETPAIRTWALLETS = struct
  type reply = {
    left : string ;
    right : string ;
    lp : string option ;
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
    let manifest_file = Globals.ft_dir // "tokens.json" in
    let manifest =
      if Sys.file_exists manifest_file then
        EzFile.read_file manifest_file
      else
        match Files.read "manifest.json" with
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

  let dexroot_address = Utils.address_of_account net "broxus-dex-root"
                        |> Misc.raw_address in
  let vault_address = Utils.address_of_account net "broxus-wton-vault"
                      |> Misc.raw_address in

  let dexroot_contract_abi =
    Misc.get_contract_abifile "Broxus_DexRoot" in
  let dexroot_contract_abi = EzFile.read_file dexroot_contract_abi in
  let dexpair_contract_abi =
    Misc.get_contract_abifile "Broxus_DexPairV4" in
  let dexpair_contract_abi = EzFile.read_file dexpair_contract_abi in
  let manifest = Lazy.force manifest in
  {
    config;
    net;
    manifest;
    client;
    server_url;
    root_contract_abi;
    vault_address ;
    wallet_contract_abi;
    dexroot_address ;
    dexroot_contract_abi ;
    dexpair_contract_abi ;
  }

let address_of_reply ~query ~reply =
  let address = (
    destruct (Printf.sprintf "%s reply" query) GETWALLETADDRESS.reply_enc reply
  ).value0
  in
  address

let string_of_amount_token amount token =
  if amount = "0" then
    Printf.sprintf "0 %s" token.Types.MANIFEST.token_symbol
  else
    let len = String.length amount in
    let ndecimals = token.Types.MANIFEST.token_decimals in
    let units, decimals =
      if len > ndecimals then
        String.sub amount 0 ( len - ndecimals ),
        String.sub amount ( len - ndecimals ) ndecimals
      else
        "0",
        String.make ( ndecimals - len ) '0' ^
        amount
    in
    Printf.sprintf "%s.%s %s"
      units decimals
      token.Types.MANIFEST.token_symbol


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
  address_of_reply ~query:"getWalletAddress" ~reply

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

let contract_exists ctxt address =
  match CommandAccountState.get_address_info ctxt.config (RawAddress address)
  with
  | None -> false
  | Some _ -> true

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
      Some ( balance, gas )

let print_wallet ctxt ~address ~wallet_address ~token =
  Printf.printf "wallet address: %s (for contract %s)\n%!"
    wallet_address address;
  match get_token_balance_gas ctxt wallet_address with
  | None ->
      Printf.printf "  Broxus_TONTokenWallet contract not yet deployed\n%!";
  | Some ( balance, gas ) ->
      Printf.printf "  balance %s (gas %s TON)\n%!"
        ( string_of_amount_token balance token )
        ( Misc.string_of_nanoton gas )

let print_wallets config account =
  let ctxt = get_context config in
  let key = Misc.find_key_exn ctxt.net account in
  let contract = CommandMultisigCreate.check_key_contract key in
  let address = Misc.get_key_address_exn key in
  Printf.printf "%s (at %s, %s):\n%!" account address contract;
  let manifest = Lazy.force manifest in

  List.iter (fun token ->

      let wallet_address = get_token_wallet_address ctxt token address in

      print_wallet ctxt ~address ~wallet_address ~token
    ) manifest.Types.MANIFEST.tokens


let get_dexpair_address ctxt token1 token2 =
  let from_token_root_address = token1.Types.MANIFEST.token_address in
  let to_token_root_address = token2.Types.MANIFEST.token_address in
  let params =
    Printf.sprintf {|{ "answerId": 1,
                       "left_root": "%s",
                       "right_root": "%s"}|}
      from_token_root_address
      to_token_root_address
  in
  let reply =
    Utils.call_run
      ctxt.config
      ~client:ctxt.client
      ~wait:false
      ~server_url:ctxt.server_url
      ~address:ctxt.dexroot_address
      ~abi:ctxt.dexroot_contract_abi
      ~meth:"getExpectedPairAddress"
      ~params
      ~local:true
      ()
  in
  address_of_reply ~query:"getExpectedPairAddress" ~reply

let get_dexpair_exchange_rate ctxt dexpair_address from_token =
  let from_token_root_address = from_token.Types.MANIFEST.token_address in
  let params =
    Printf.sprintf {|{ "answerId": 1,
                       "amount": "1000000000",
                       "spent_token_root": "%s"}|}
      from_token_root_address
  in
  let reply =
    Utils.call_run
      ctxt.config
      ~client:ctxt.client
      ~wait:false
      ~server_url:ctxt.server_url
      ~address:dexpair_address
      ~abi:ctxt.dexpair_contract_abi
      ~meth:"expectedExchange"
      ~params
      ~local:true
      ()
  in
  (* Printf.eprintf "reply: %s\n%!" reply; *)
  let reply = (
    destruct "expectedExchange reply"
      EXPECTEDEXCHANGE.reply_enc reply
  )
  in
  reply.EXPECTEDEXCHANGE.expected_amount,
  reply.expected_fee


let get_dexpair_vaults ctxt dexpair_address =
  let params =
    Printf.sprintf {|{ "answerId": 1 }|}
  in
  let reply =
    Utils.call_run
      ctxt.config
      ~client:ctxt.client
      ~wait:false
      ~server_url:ctxt.server_url
      ~address:dexpair_address
      ~abi:ctxt.dexpair_contract_abi
      ~meth:"getVaultWallets"
      ~params
      ~local:true
      ()
  in
  (* Printf.eprintf "reply: %s\n%!" reply; *)
  let reply = (
    destruct "getVaultWallets reply"
      GETPAIRTWALLETS.reply_enc reply
  )
  in
  reply.left, reply.right

let get_dexpair_wallets ctxt dexpair_address =
  let params =
    Printf.sprintf {|{ "answerId": 1 }|}
  in
  let reply =
    Utils.call_run
      ctxt.config
      ~client:ctxt.client
      ~wait:false
      ~server_url:ctxt.server_url
      ~address:dexpair_address
      ~abi:ctxt.dexpair_contract_abi
      ~meth:"getTokenWallets"
      ~params
      ~local:true
      ()
  in
  (* Printf.eprintf "reply: %s\n%!" reply; *)
  let reply = (
    destruct "getTokenWallets reply"
      GETPAIRTWALLETS.reply_enc reply
  )
  in
  reply.left, reply.right, reply.lp

let print_pairs config =
  let ctxt = get_context config in

  let token1 = get_token_by_symbol ctxt "WTON" in
  List.iter (fun token2 ->
      if token1 <> token2 then
        let f token1 token2 =
          let dexpair_address = get_dexpair_address ctxt token1 token2 in
          Printf.printf "Pair %s/%s ( address %s )\n%!"
            token1.token_symbol token2.token_symbol dexpair_address ;
          if not ( contract_exists ctxt dexpair_address ) then
            Printf.printf "   DexPair contract does not exist\n%!"
          else
            let token1_wallet_address =
              get_token_wallet_address ctxt token1 dexpair_address in
            let token2_wallet_address =
              get_token_wallet_address ctxt token2 dexpair_address in
(*
          print_wallet ctxt
            ~address:dexpair_address
            ~wallet_address:token1_wallet_address
            ~token:token1 ;
          print_wallet ctxt
            ~address:dexpair_address
            ~wallet_address:token2_wallet_address
            ~token:token2 ;
*)
            let g token1 token2 =
              let expected_amount, expected_fee =
                get_dexpair_exchange_rate ctxt dexpair_address token1 in
              Printf.printf "   For %s => %s (fee %s)\n%!"
                ( string_of_amount_token "1000000000" token1 )
                ( string_of_amount_token expected_amount token2 )
                expected_fee ;
            in
            g token1 token2 ;
            g token2 token1 ;
            let left_vault_address, right_vault_address =
              get_dexpair_vaults ctxt dexpair_address in
            let _left_wallet_address, right_wallet_address, _lp =
              get_dexpair_wallets ctxt dexpair_address in

            print_wallet ctxt
              ~address:( Printf.sprintf "liquidity %s" token1.token_symbol )
              ~wallet_address:
                (if right_wallet_address = token1_wallet_address then
                   right_vault_address
                 else
                   left_vault_address )
              ~token:token1 ;
            print_wallet ctxt
              ~address:( Printf.sprintf "liquidity %s" token2.token_symbol )
              ~wallet_address:
                (if right_wallet_address = token2_wallet_address then
                   right_vault_address
                 else
                   left_vault_address )
              ~token:token2 ;
        in
        f token1 token2 ;
        (*        f token2 token1 ;*)
        Printf.printf "----------------------------------------------\n%!"
    ) ctxt.manifest.Types.MANIFEST.tokens


let cmd =
  let args = ref [] in
  Misc.cmd
    ["token"; "list"]
    (fun () ->
       let config = Config.config () in
       match !args with
       | [] -> print_pairs config
       | args ->
           List.iter ( print_wallets config ) args
    )
    ~args:
      [
        [], Arg.Anons ( fun list -> args := list),
        EZCMD.info ~docv:"ACCOUNT" "The associated multisig wallet";
      ]
    ~doc: "Display token wallet info"
    ~man:[
    ]
