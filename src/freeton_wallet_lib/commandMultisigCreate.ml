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

open EzCompat
open Ezcmd.V2
open EZCMD.TYPES

open Types

let is_multisig_contract s =
  let config = Config.config () in
  let s,_ = EzString.cut_at s '/' in
  List.mem s config.multisigs

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

(* without 0x *)
let pubkey_of_custodian net name =
  match Misc.find_key net name with
  | None ->
      begin match Misc.is_uint256 name with
        | None ->
            Error.raise "Key %S does not exist" name
        | Some name -> name
      end
  | Some key ->
      match key.key_pair with
      | None -> Error.raise "Key %S has no key pair" name
      | Some pair ->
          match pair.secret with
          | None ->
              (* We should add an option to allow this *)
              Error.raise "Key %S has no secret" name
          | Some _ -> pair.public


let create_multisig
    ?client
    ?(custodians=[])
    ?(not_owner=false)
    ?req
    ?wc
    ?contract
    account
  =
  let config = Config.config () in
  let net = Config.current_network config in
  let owners = StringSet.of_list custodians in
  let owners =
    if not_owner then owners else
      StringSet.add account owners in

  let owners = StringSet.to_list owners in

  let owners = List.map (pubkey_of_custodian net) owners in

  let nowners = List.length owners in
  if nowners = 0 then
    Error.raise "Multisig requires at least one custodian";
  let req = match req with
    | Some req ->
        if req < 1 || req > nowners then
          Error.raise "Invalid --req %d, should be 0 < REQ <= %d (nbr owners)"
            req nowners;
        req
    | None ->
        nowners / 2 + 1
  in

  let params =
    Printf.sprintf "{\"owners\":[ \"0x%s\" ],\"reqConfirms\":%d}"
      ( String.concat "\", \"0x" owners )
      req
  in
  let key = Misc.find_key_exn net account in

  let contract =
    match key.key_account with
    | None ->
        begin
          match contract with
          | None -> "SafeMultisigWallet"
          | Some contract -> contract
        end
    | Some acc ->
        begin
          match wc with
          | None -> ()
          | Some _ ->
              if Misc.string_of_workchain wc <>
                 Misc.string_of_workchain  acc.acc_workchain then
                Error.raise {|Account address uses a different workchain. Clear it with  'ft account %s --contract ""|} key.key_name
        end ;
        match acc.acc_contract with
        | None ->
            begin
              match contract with
              | None -> "SafeMultisigWallet"
              | Some contract -> contract
            end
        | Some acc_contract ->
            match contract with
            | None -> acc_contract
            | Some contract ->
                if contract <> acc_contract then
                  Error.raise {|Account %s uses a different contract %S. Clear it with 'ft account %s --contract ""|} key.key_name acc_contract key.key_name ;
                contract
  in
  if not (is_multisig_contract contract) then
    Error.raise {|Contract %s is not a multisig contract|} contract;

  let wc = match wc with
    | Some _ -> wc
    | None ->
        match key.key_account with
        | None -> None
        | Some acc ->
            acc.acc_workchain
  in

  Utils.deploy_contract config ~key ~contract ~params ~wc ?client ()

let action ?wc account custodians ~req ~not_owner ~contract =

  let config = Config.config () in

  Subst.with_substituted_list ~config custodians (fun custodians ->
      create_multisig account ~custodians ~not_owner ~req ?wc ?contract
    )

let cmd =
  let wc = ref None in
  let owners = ref [] in
  let req = ref 1 in
  let not_owner = ref false in
  let contract = ref None in

  EZCMD.sub
    "multisig create"
    (fun () ->
       match !owners with
       | [] -> Error.raise "You must provide the account name"
       | account :: custodians ->
           action
             account custodians
             ~req:!req
             ~not_owner:!not_owner
             ?wc:!wc
             ~contract:!contract
    )
    ~args:
      [
        [], Arg.Anons (fun list -> owners := list),
        EZCMD.info ~docv:"ACCOUNT" "Account name, and other custodians" ;

        [ "wc" ], Arg.Int (fun s -> wc := Some s),
        EZCMD.info ~docv:"WORKCHAIN" "The workchain (default is 0)";

        [ "not-owner" ], Arg.Set not_owner,
        EZCMD.info " Initial account should not be an owner";

        [ "contract" ], Arg.String (fun s -> contract := Some s),
        EZCMD.info ~docv:"CONTRACT" "Use this contract";

        [ "surf" ], Arg.Unit (fun () ->
            contract := Some "SetcodeMultisigWallet2"),
        EZCMD.info "Use Surf contract";

        [ "req" ], Arg.Int (fun s -> req := s),
        EZCMD.info ~docv:"REQ" "Number of confirmations required";

      ]
    ~doc: "Manage a multisig-wallet (create, confirm, send)"
    ~man:[
      `S "DESCRIPTION";
      `P "This command deploys a multisig contract on a credited account." ;

      `P "Create an account and get its address:";
      `Pre {|# ft account create MY-ACCOUNT|} ;
      `P "Backup the account info off-computer.";
      `P "The second command will give you an address in 0:XXX format. Send some tokens on the address to be able to deploy the multisig.";
      `P "Check its balance with:";
      `Pre {|# ft account info MY-ACCOUNT|};
      `P "Then, to create a single-owner multisig:";
      `Pre {|# ft multisig create MY-ACCOUNT|} ;
      `P "To create a multi-owners multisig:";
      `Pre {|# ft multisig create MY-ACCOUNT owner2 owner3 owner4|} ;
      `P "To create a multi-owners multisig with 2 signs required:";
      `Pre {|# ft multisig create MY-ACCOUNT owner2 owner3 --req 2|} ;
      `P "To create a multi-owners multisig not self-owning:";
      `Pre {|# ft multisig create MY-ACCOUNT owner1 owner2 owner3 --not-owner|} ;
      `P "Verify that it worked:";
      `Pre {|# ft account info MY-ACCOUNT -v|};
    ]
