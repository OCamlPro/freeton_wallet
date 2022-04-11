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

(*
DexRoot 0:943bad2e74894aa28ae8ddbe673be09a0f3818fd170d12b4ea8ef1ea8051e940

To make an exchange on tonswap you should

1) request dex pair address
https://github.com/broxus/ton-dex/blob/master/build/DexRoot.abi.json#L189

Parameters are two token addresses in any position

{
			"name": "getExpectedPairAddress",
			"inputs": [
				{"name":"answerId","type":"uint32"},
				{"name":"left_root","type":"address"},
				{"name":"right_root","type":"address"}
			],
			"outputs": [
				{"name":"value0","type":"address"}
			]
		},

2) Get your token wallets address for both tokens

wallet_public_key_: 0,
owner_address_ - main wallet

call  https://github.com/broxus/ton-eth-bridge-token-contracts/blob/master/free-ton/contracts/RootTokenContract.sol#L112

Also check that they are deployed, by asking state or balance of evers

3) Construct payload

call https://github.com/broxus/ton-dex/blob/master/build/DexPairV4.abi.json#L118
               {
                        "name": "buildExchangePayload",
                        "inputs": [
                                {"name":"id","type":"uint64"},
                                {"name":"deploy_wallet_grams","type":"uint128"},
                                {"name":"expected_amount","type":"uint128"}
                        ],
                        "outputs": [
                                {"name":"value0","type":"cell"}
                        ]
                },

id - any number
deploy_wallet_grams - here 100000000 (0.1 ever) if you don't have deployed wallet or 0 if you have wallet deployed
expectedAmount - here you input how much tokens minimum you want to recieve. if 0 than it will be executed by market rate

Or you can input in expectedamount results of execution decreased by slippage amount
https://github.com/broxus/ton-dex/blob/master/build/DexPairV4.abi.json#L229

       {
                        "name": "expectedExchange",
                        "inputs": [
                                {"name":"answerId","type":"uint32"},
                                {"name":"amount","type":"uint128"},
                                {"name":"spent_token_root","type":"address"}
                        ],
                        "outputs": [
                                {"name":"expected_amount","type":"uint128"},
                                {"name":"expected_fee","type":"uint128"}
                        ]
                },


4) Than on wallet of token that is being sold
https://github.com/broxus/ton-eth-bridge-token-contracts/blob/master/free-ton/build/TONTokenWallet.abi.json#L84

recipient_public_key = 0
recipient_address - pair address (см. п 1)
tokens = how much tokens we swap
deploy_grams = 0
transfer_grams = 0
send_gas_to - adress you send transaction from
notify_receiver = true
payload = results of 3rd step

5) than you catch event
https://github.com/broxus/ton-dex/blob/master/contracts/interfaces/IDexPairOperationCallback.sol

dexPairExchangeSuccess or dexPairOperationCancelled

*)

open Ezcmd.V2
open EZCMD.TYPES
open CommandTokenList.TYPES


let action config ~amount ~token ~from_ ~to_ () =

  let ctxt = CommandTokenList.get_context config in

  let from_key = Misc.find_key_exn ctxt.net from_ in
  let from_contract = CommandMultisigCreate.check_key_contract from_key in
  let from_address = Misc.get_key_address_exn from_key in

  let amount = Misc.nanotokens_of_string amount  in
  let from_token = CommandTokenList.get_token_by_symbol ctxt token in
  let to_token = CommandTokenList.get_token_by_symbol ctxt to_ in

  let from_wallet_address =
    CommandTokenList.get_token_wallet_address ctxt from_token from_address in
  let to_wallet_address =
    CommandTokenList.get_token_wallet_address ctxt to_token from_address in

  let dexpair_address =
    CommandTokenList.get_dexpair_address ctxt from_token to_token in
  let _from_address_status =
    CommandTokenList.get_token_balance_gas ctxt
      ~wallet_address:from_wallet_address in
  let to_address_status =
    CommandTokenList.get_token_balance_gas ctxt
      ~wallet_address:to_wallet_address in


  let payload =

    let params =
      Printf.sprintf {|{ "id": 1,
                         "deploy_wallet_grams": "%Ld",
                         "expected_amount": "%Ld" }|}
        ( match to_address_status with
          | None -> 100_000_000L
          | Some _ -> 0L )
        0L
    in
    let reply =
      Utils.call_run
        ctxt.config
        ~client:ctxt.client
        ~wait:false
        ~server_url:ctxt.server_url
        ~address:dexpair_address
        ~abi:ctxt.dexpair_contract_abi
        ~meth:"buildExchangePayload"
        ~params
        ~local:true
        ()
    in
    CommandTokenList.string_of_reply ~query:"buildExchangePayload" ~reply
  in

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
        "payload": "%s"
       }|}
        ( ADDRESS.to_string dexpair_address )
        ( Int64.to_string amount )
        ( ADDRESS.to_string from_address )
        payload
    in
    Ton_sdk.ABI.encode_body ~abi ~meth ~params
  in
  let params = Printf.sprintf
      {|{"dest":"%s","value":%Ld,"bounce":%b,"flags":%d,"payload":"%s"}|}
      ( ADDRESS.to_string from_wallet_address )
      ( Int64.add amount 2_000_000_000L )
      true
      0
      payload
  in

  let accounts = [
    "dexroot", ctxt.dexroot_address, "Broxus_DexRoot" ;
    "dexpair", dexpair_address, "Broxus_DexPairV4" ;
    "from_wallet", from_wallet_address, "Broxus_TONTokenWallet" ;
    "to_wallet", to_wallet_address, "Broxus_TONTokenWallet" ;
  ] in

  Printf.printf "Source:";
  CommandTokenList.print_wallet ctxt
    ~wallet_address:from_wallet_address
    ~owner:( ADDRESS.to_string from_address ) ~token:from_token;
  Printf.printf "Destination:";
  CommandTokenList.print_wallet ctxt
    ~wallet_address:to_wallet_address
    ~owner:( ADDRESS.to_string from_address ) ~token:to_token;

  Utils.call_contract config
    ~contract:from_contract
    ~address:from_address
    ~meth:"sendTransaction"
    ~params
    ~local:false
    ~src:from_key
    ~wait:true
    ~accounts
    ();

  Printf.printf "AFTER TRANSFER:\n%!";
  Printf.printf "Source:";
  CommandTokenList.print_wallet ctxt
    ~wallet_address:from_wallet_address
    ~owner:( ADDRESS.to_string from_address ) ~token:from_token;
  Printf.printf "Destination:";
  CommandTokenList.print_wallet ctxt
    ~wallet_address:to_wallet_address
    ~owner:( ADDRESS.to_string from_address ) ~token:to_token;

  ()

let cmd =
  let args = ref [] in
  let arg_to = ref None in
  let arg_for = ref None in
  Misc.cmd
    ["token"; "swap"]
    (fun () ->
       let config = Config.config () in
       match !args, !arg_to, !arg_for with
       | [ amount ; token ], Some to_, Some from_ ->
           action config ~token ~amount ~to_ ~from_ ()
       | _ ->
           Error.raise "Usage: ft token swap AMOUNT TOKEN --for OWNER --to TOKEN"
    )
    ~args:
      [
        [], Arg.Anons ( fun list -> args := list),
        EZCMD.info ~docv:"AMOUNT TOKEN" "Amount and token symbol";

        [ "--for" ], Arg.String (fun s -> arg_for := Some s),
        EZCMD.info ~docv:"OWNER" "Token account owner";

        [ "--to" ], Arg.String (fun s -> arg_to := Some s),
        EZCMD.info ~docv:"TOKEN" "Destination token";

      ]
    ~doc: "Swap tokens in DEX"
    ~man:[
    ]
