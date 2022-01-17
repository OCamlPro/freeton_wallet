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
open CommandTokenList.TYPES


let action config ~account ~amount () =
  let ctxt = CommandTokenList.get_context config in
  let account_key = Misc.find_key_exn ctxt.net account in
  let account_contract = CommandMultisigCreate.check_key_contract account_key in
  let account_address = Misc.get_key_address_exn account_key in
  Printf.printf "%s (at %s, %s):\n%!" account account_address account_contract;

  let amount = Misc.nanotokens_of_string amount  in
  let token = CommandTokenList.get_token_by_symbol ctxt "WTON" in

  let vault_address = Utils.address_of_account ctxt.net "wton-vault" in
  let vault_address = Misc.raw_address vault_address in
  let wallet_address =
    CommandTokenList.get_token_wallet_address ctxt token account_address in

  let payload =
    let abi = ctxt.wallet_contract_abi in
    let meth = "transferToRecipient" in
    let params =
      Printf.sprintf {|{
        "recipient_public_key": 0,
        "recipient_address": "%s",
        "tokens": "%s",
        "deploy_grams": 0,
        "transfer_grams": 0,
        "send_gas_to": "%s",
        "notify_receiver": true,
        "payload": ""
       }|}
        vault_address
        ( Int64.to_string amount )
        account_address
    in
    Ton_sdk.ABI.encode_body ~abi ~meth ~params
  in
  let params = Printf.sprintf
      {|{"dest":"%s","value":%Ld,"bounce":%b,"flags":%d,"payload":"%s"}|}
      wallet_address
      2_000_000_000L
      true
      0
      payload
  in

  Utils.call_contract config
    ~contract:account_contract
    ~address:account_address
    ~meth:"sendTransaction"
    ~params
    ~local:false
    ~src:account_key
    ~wait:true
    ();

  match CommandTokenList.get_token_balance_gas ctxt wallet_address with
  | None ->
      Printf.printf "  %s\n%!" token.Types.MANIFEST.token_name ;
      Printf.printf "    address: %s\n%!" wallet_address;
      Printf.printf "    Broxus_TONTokenWallet contract not yet deployed\n%!";
  | Some ( balance, gas ) ->
      Printf.printf "  %s\n%!" token.Types.MANIFEST.token_name ;
      Printf.printf "    address: %s\n%!" wallet_address;
      Printf.printf "    balance %s %s (gas %s TON)\n%!"
        ( Misc.string_of_nanoton balance )
        token.Types.MANIFEST.token_symbol
        ( Misc.string_of_nanoton gas )

(*
        let root_address = token.Types.MANIFEST.token_address in
        let params = Printf.sprintf {|{ "_answer_id": 1,
                                      "wallet_public_key_": 0,
                                      "owner_address_": "%s"}|}
            address
        in
        let reply =
          Utils.call_run config ~client ~wait ~server_url
            ~address:root_address
            ~abi:root_contract_abi
            ~meth:"getWalletAddress"
            ~params
            ~local:true
            ()
        in
        let address = (
          destruct "getWalletAddress reply" GETWALLETADDRESS.reply_enc reply
        ).value0
        in

        let info = CommandAccountState.get_address_info config
            (RawAddress address) in
        match info with
        | None ->
            Printf.printf "  %s\n%!" token.Types.MANIFEST.token_name ;
            Printf.printf "    address: %s\n%!" address;
            Printf.printf "    Broxus_TONTokenWallet contract not yet deployed\n%!";
        | Some acc ->
            let gas =
              match acc.acc_balance with
              | None -> "0"
              | Some n -> Misc.string_of_nanoton ( Z.to_int64 n )
            in
            Printf.printf "  %s\n%!" token.Types.MANIFEST.token_name ;
            Printf.printf "    address: %s\n%!" address;

            let params = Printf.sprintf {|{ "_answer_id": 1 }|} in
            let reply =
              Utils.call_run config ~client ~wait ~server_url
                ~address
                ~abi:wallet_contract_abi
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
            Printf.printf "    balance %s %s (gas %s TON)\n%!"
              ( Misc.string_of_nanoton balance )
              token.Types.MANIFEST.token_symbol
              gas
    ) manifest.Types.MANIFEST.tokens
*)

let cmd =
  let args = ref [] in
  Misc.cmd
    ["token"; "wton"; "withdraw"]
    (fun () ->
       let config = Config.config () in
       match !args with
       | [ account ; amount ] ->
           action config ~account ~amount ()
       | _ ->
           Error.raise "Usage: ft token wton withdraw ACCOUNT AMOUNT"
    )
    ~args:
      [
        [], Arg.Anons ( fun list -> args := list),
        EZCMD.info ~docv:"ACCOUNT AMOUNT" "The associated multisig wallet";
      ]
    ~doc: "Display token wallet info"
    ~man:[
    ]
