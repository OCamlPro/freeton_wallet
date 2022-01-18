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


let action config ~amount ~token ~from_ ~to_ () =

  let ctxt = CommandTokenList.get_context config in

  let from_key = Misc.find_key_exn ctxt.net from_ in
  let from_contract = CommandMultisigCreate.check_key_contract from_key in
  let from_address = Misc.get_key_address_exn from_key in

  let to_key = Misc.find_key_exn ctxt.net to_ in
  let to_address = Misc.get_key_address_exn to_key in

  let amount = Misc.nanotokens_of_string amount  in
  let token = CommandTokenList.get_token_by_symbol ctxt token in

  let from_wallet_address =
    CommandTokenList.get_token_wallet_address ctxt token from_address in
  let to_wallet_address =
    CommandTokenList.get_token_wallet_address ctxt token to_address in

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
        to_address
        ( Int64.to_string amount )
        from_address
    in
    Ton_sdk.ABI.encode_body ~abi ~meth ~params
  in
  let params = Printf.sprintf
      {|{"dest":"%s","value":%Ld,"bounce":%b,"flags":%d,"payload":"%s"}|}
      from_wallet_address
      2_000_000_000L
      true
      0
      payload
  in

  Printf.printf "Source:";
  CommandTokenList.print_wallet ctxt
    ~wallet_address:from_wallet_address ~address:from_address ~token;
  Printf.printf "Destination:";
  CommandTokenList.print_wallet ctxt
    ~wallet_address:to_wallet_address ~address:to_address ~token;

  Utils.call_contract config
    ~contract:from_contract
    ~address:from_address
    ~meth:"sendTransaction"
    ~params
    ~local:false
    ~src:from_key
    ~wait:true
    ();

  Printf.printf "AFTER TRANSFER:\n%!";
  Printf.printf "Source:";
  CommandTokenList.print_wallet ctxt
    ~wallet_address:from_wallet_address ~address:from_address ~token;
  Printf.printf "Destination:";
  CommandTokenList.print_wallet ctxt
    ~wallet_address:to_wallet_address ~address:to_address ~token;

  ()

let cmd =
  let args = ref [] in
  let arg_to = ref None in
  let arg_from = ref None in
  Misc.cmd
    ["token"; "transfer"]
    (fun () ->
       let config = Config.config () in
       match !args, !arg_to, !arg_from with
       | [ amount ; token ], Some to_, Some from_ ->
           action config ~token ~amount ~to_ ~from_ ()
       | _ ->
           Error.raise "Usage: ft token transfer AMOUNT TOKEN --from OWNER --to OWNER"
    )
    ~args:
      [
        [], Arg.Anons ( fun list -> args := list),
        EZCMD.info ~docv:"AMOUNT TOKEN" "Amount and token symbol";

        [ "--to" ], Arg.String (fun s -> arg_to := Some s),
        EZCMD.info ~docv:"OWNER" "Destination account owner";

        [ "--from" ], Arg.String (fun s -> arg_from := Some s),
        EZCMD.info ~docv:"OWNER" "Source account owner";

      ]
    ~doc: "Transfer tokens between two token wallets"
    ~man:[
    ]
