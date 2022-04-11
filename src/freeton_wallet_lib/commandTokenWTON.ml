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

open Types

let print_wallet config account =
  let ctxt = CommandTokenList.get_context config in
  let key = Misc.find_key_exn ctxt.net account in
  let contract = CommandMultisigCreate.check_key_contract key in
  let address = Misc.get_key_address_exn key in
  Printf.printf "%s (at %s, %s):\n%!" account
    ( ADDRESS.to_string address ) contract;

  let token = CommandTokenList.get_token_by_symbol ctxt "WTON" in
  if token.Types.MANIFEST.token_symbol = "WTON" then

    let wallet_address =
      CommandTokenList.get_token_wallet_address ctxt token address in

    match CommandTokenList.get_token_balance_gas ctxt ~wallet_address with
    | None ->
        Printf.printf "  %s\n%!" token.Types.MANIFEST.token_name ;
        Printf.printf "    address: %s\n%!" ( ADDRESS.to_string address );
        Printf.printf "    Broxus_TONTokenWallet contract not yet deployed\n%!";
    | Some ( balance, gas ) ->
        Printf.printf "  %s\n%!" token.Types.MANIFEST.token_name ;
        Printf.printf "    address: %s\n%!" ( ADDRESS.to_string wallet_address );
        Printf.printf "    balance %s (gas %s TON)\n%!"
          ( CommandTokenList.string_of_amount_token balance token )
          ( Misc.string_of_nanoton gas )


let cmd =
  let args = ref [] in
  Misc.cmd
    ["token"; "wton"]
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
