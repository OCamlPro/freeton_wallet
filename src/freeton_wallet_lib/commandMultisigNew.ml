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

let action ?wc account custodians ?req ~contract
    ?credit ?creditor () =

  let config = Config.config () in
  let net = Config.current_network config in

  if Misc.key_exists net account then
    Error.raise "Account %s already exists (use 'multisig create')" account;

  CommandAccountCreate.genkey ~name:account ~contract config ~force:false ;

  let custodians =
    Subst.with_substituted_list ~config custodians (fun c -> c)
  in

  let credit = match credit with
    | None -> "1"
    | Some s -> s
  in
  let creditor = match creditor, custodians with
    | None, [] -> Error.raise "You must provide custodians"
    | None, creditor :: _ -> creditor
    | Some creditor, _ -> creditor
  in
  Printf.eprintf "Sending %s TON from %S\n%!" credit creditor ;

  CommandMultisigTransfer.send_transfer
    ~account:creditor
    ~dst:account
    ~amount:credit ();

  CommandMultisigCreate.create_multisig account ~custodians
    ~not_owner:true ?req ?wc ~contract

let cmd =
  let wc = ref None in
  let owners = ref [] in
  let req = ref None in
  let contract = ref "SetcodeMultisigWallet2" in
  let credit = ref None in
  let creditor = ref None in

  Misc.cmd
    [ "multisig" ;  "new" ]
    (fun () ->
       match !owners with
       | [] -> Error.raise "You must provide the account name"
       | account :: custodians ->
           action
             account custodians
             ?req:!req
             ?wc:!wc
             ~contract:!contract
             ?credit:!credit
             ?creditor:!creditor
             ()
    )
    ~args:
      [
        [], Arg.Anons (fun list -> owners := list),
        EZCMD.info ~docv:"ACCOUNT" "Account name, and other custodians" ;

        [ "wc" ], Arg.Int (fun s -> wc := Some s),
        EZCMD.info ~docv:"WORKCHAIN" "The workchain (default is 0)";

        [ "contract" ], Arg.String (fun s -> contract := s),
        EZCMD.info ~docv:"CONTRACT" "Use this contract";

        [ "surf" ], Arg.Unit (fun () ->
            contract :=  "SetcodeMultisigWallet2"),
        EZCMD.info "Use Surf contract (default)";

        [ "req" ], Arg.Int (fun s -> req := Some s),
        EZCMD.info ~docv:"REQ" "Number of confirmations required (majority by default)";

        [ "credit" ], Arg.String (fun s -> credit := Some s),
        EZCMD.info ~docv:"TONS" "Initial credit for multisig (1 TON by default)";

        [ "creditor" ], Arg.String (fun s -> creditor := Some s),
        EZCMD.info ~docv:"ACCOUNT"
          "Send credit from this account (default is to use the first custodian)";

      ]
    ~doc: "Create an independant multisig-wallet, similarly to 'multisig create', but not owner and perform crediting"
    ~man:[
      `S "DESCRIPTION";
      `P "This command deploys a multisig contract on a new account." ;

    ]
