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
open Ezcmd.V2
open EZCMD.TYPES
open Types
(* open EzFile.OP *)

let for_all_users config f =
  let net = Config.current_network config in
  List.iter (fun key ->
      match EzString.chop_prefix ~prefix:"user" key.key_name with
      | None -> ()
      | Some _ ->
          f key
    ) net.net_keys

(* Currently, the giver creates an account with Contract:
"code_hash":
     "207dc560c5956de1a2c1479356f8f3ee70a59767db2bf4788b1d61ad42cdad82",
for SetcodeMultisigWallet2
whereas
"code_hash":
     "80d6c47c4a25543c9b397b71716f3fae1e2c5d247174c52e2c19bd896442b105",
for SafeMultisigWallet
*)

let action ~amount ~accounts =
  let config = Config.config () in
  let node = Config.current_node config in
  match node.node_local with
  | None ->
      Error.raise "cannot manage remote node %S" node.node_name
  | Some _local_node ->

      let amount = amount * 1_000_000_000 in
      let to_give = ref [] in
      let to_deploy = ref [] in
      let check_key ?(force=false) key =
        Printf.eprintf "For key %s\n%!" key.key_name;
        match key.key_account with
        | None ->
            if force then
              Error.raise "Key %S has no address\n%!" key.key_name
        | Some { acc_address ; acc_contract ; _ } ->
            let exists, balance =
              match Utils.get_account_info config acc_address with
              | None -> false, Z.zero
              | Some ( exists, balance ) -> exists, balance
            in
            if balance < Z.of_int amount then
              to_give := (acc_address, key) :: !to_give ;
            if not exists then
              to_deploy := (acc_contract, key) :: !to_deploy
      in
      let config = Config.config () in
      let net = Config.current_network config in
      begin match accounts with
        | [] ->
            for_all_users config check_key
        | _ ->
            List.iter (fun account ->
                let key = Misc.find_key_exn net account in
                check_key ~force:true key
              ) accounts
      end;
      let amount = string_of_int amount in
      let client = Ton_sdk.CLIENT.create node.node_url in
      List.iter (fun (address, key) ->
          let meth = "sendGrams" in
          let params =
            Printf.sprintf
              {|{ "dest": "%s", "amount": "%s" }|} address amount
          in

          if Globals.use_ton_sdk then
            let node = Config.current_node config in
            let abi_file = Misc.get_contract_abifile "Giver" in
            let abi = EzFile.read_file abi_file in
            let giver = Misc.find_key_exn net "giver" in
            let giver = match giver.key_account with
              | None -> assert false
              | Some { acc_address ; _ } -> acc_address
            in
            let res =
              Ton_sdk.CALL.call
                ~client
                ~server_url:node.node_url
                ~address:giver ~abi
                ~meth ~params
                ~local:false
                ()
            in
            Printf.eprintf "call returned %s\n%!" res
          else
            CommandClient.action
              ~exec:false
              [
                "call" ; "%{account:addr:giver}"; meth ; params ;
                "--abi"; "%{abi:Giver}" ;
                "--sign" ; Printf.sprintf "%%{account:keyfile:%s}"
                  key.key_name;
              ]
        ) !to_give;

      List.iter (fun (contract, key) ->
          CommandMultisigCreate.create_multisig ~client key.key_name ?contract
        ) !to_deploy;

      ()

let cmd =
  let amount = ref 1000 in
  let accounts = ref [] in
  EZCMD.sub
    "node give"
    (fun () ->
       action ~accounts:!accounts ~amount:!amount
    )
    ~args: [

      [ "amount" ], Arg.Int (fun n -> amount := n),
      EZCMD.info ~docv:"AMOUNT" "Number of TONs to give";

      [], Arg.Anons (fun list -> accounts := list ),
      EZCMD.info
        ~docv:"ACCOUNT"
        "Give TONs from giver to ACCOUNT (all if none specified). By \
         default, transfer 1000 TONS to the account if its \
         balance is smaller, and deploy a contract if it is a multisig \
         smart contract.";
    ]
    ~doc: "Give TONs to accounts on sandbox networks"
    ~man:[
      `S "DESCRIPTION";
      `P "This command can be used to create initial accounts with \
          tokens on sandbox networks running TONOS SE (created using \
          'ft switch create sandboxNN').";

    ]
