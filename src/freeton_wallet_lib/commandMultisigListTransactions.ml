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

open Types


let is_multisig_contract = function
  | "SafeMultisigWallet"
  | "SetcodeMultisigWallet"
  | "SetcodeMultisigWallet2"
    -> true
  | _ -> false

let check_key_contract key =
  match key.key_account with
  | Some { acc_contract = Some acc_contract ; _ } ->
      if is_multisig_contract acc_contract then
        acc_contract
      else
        Error.raise "Account's contract %S is not multisig" acc_contract;

      (* Account's contract is not set, let's use the minimal common ABI *)
  | _ ->
      "SafeMultisigWallet"

let get_waiting account =
  let config = Config.config () in
  let net = Config.current_network config in
  let key = Misc.find_key_exn net account in
  let contract = check_key_contract key in
  let address = Misc.get_key_address_exn key in

  (*
  let subst ~msg _config res =
    match EzEncoding.destruct Types.multisig_transactions res with
    | exception exn ->
        Printf.eprintf "Can't parse: %s\n%!" ( Printexc.to_string exn );
        Printf.printf "call result:\n%s\n%!" res
    | trs ->
        Printf.printf "%d transactions waiting\n%!" (List.length trs);
        List.iter (fun tr ->
            Printf.printf "Transaction id: %d\n%!" tr.id
          ) trs

  in
*)

  Utils.call_contract config
    ~contract
    ~address
    ~meth:"getTransactions"
    ~params:"{}"
    ~local:true
    (*    ~subst *)
    ()

let action account =

  let account = match account with
    | None ->
        Error.raise "You must provide the account"
    | Some account -> account
  in
  get_waiting account ;
  ()

let cmd =
  let account = ref None in
  EZCMD.sub
    "multisig list transactions"
    (fun () ->
       action !account
    )
    ~args:
      [
        [], Arg.Anon (0, fun s -> account := Some s),
        EZCMD.info ~docv:"ACCOUNT" "The multisig account";
      ]
    ~doc: "Display waiting transactions in a multisig wallet"
    ~man:[
      `S "DESCRIPTION";
      `P "This command can be used to display the currently waiting transactions in a multisig wallet" ;
      `S "LIST WAITING TRANSACTIONS";
      `P "Display transactions waiting for confirmations:";
      `Pre {|# ft multisig list transactions MY-ACCOUNT|};
    ]
