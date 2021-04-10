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
(* open EzFile.OP *)

type todo =
    NodeStart
  | NodeStop
  | NodeWeb
  | NodeGive of string

let container_of_node local_node =
  Printf.sprintf "local-node-%d" local_node.local_port

let for_all_users config f =
  let net = Config.current_network config in
  List.iter (fun key ->
      match EzString.chop_prefix ~prefix:"user" key.key_name with
      | None -> ()
      | Some _ ->
          f key
    ) net.net_keys

let z1000 = Z.of_string "1_000_000_000_000"

(* Currently, the giver creates an account with Contract:
"code_hash":
     "207dc560c5956de1a2c1479356f8f3ee70a59767db2bf4788b1d61ad42cdad82",
for SetcodeMultisigWallet2
whereas
"code_hash":
     "80d6c47c4a25543c9b397b71716f3fae1e2c5d247174c52e2c19bd896442b105",
for SafeMultisigWallet
*)

let action ~todo =
  let config = Config.config () in
  let node = Config.current_node config in
  match node.node_local with
  | None ->
      Error.raise "cannot manage remote node %S" node.node_name
  | Some local_node ->
      match todo with
      | NodeStart ->
          Misc.call [ "docker"; "start" ; container_of_node local_node ]
      | NodeStop ->
          Misc.call [ "docker"; "stop" ; container_of_node local_node ]
      | NodeWeb ->
          Misc.call [ "xdg-open";
                      Printf.sprintf "http://0.0.0.0:%d/graphql"
                        local_node.local_port ]

      | NodeGive account ->

          let account, amount = EzString.cut_at account ':' in
          let account = if account = "all" then None else Some account in
          let amount = if amount = "" then 1000 else int_of_string amount in
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
                let give, deploy =
                  match
                    Utils.post config
                      (Ton_sdk.REQUEST.account acc_address)
                  with
                    [] -> true, true
                  | [ acc ] ->
                      let give =
                        match acc.acc_balance with
                        | None -> true
                        | Some z ->
                            z < Z.of_int amount
                      in
                      let deploy =
                        match acc.acc_type_name with
                        | Some "Uninit" (* 0 *) -> true
                        | _ -> false
                      in
                      give, deploy
                  | _ -> assert false
                in
                if give then
                  to_give := (acc_address, key) :: !to_give ;
                if deploy then
                  to_deploy := (acc_contract, key) :: !to_deploy
          in
          let config = Config.config () in
          let net = Config.current_network config in
          begin match account with
            | None ->
                for_all_users config check_key
            | Some account ->
                let key = Misc.find_key_exn net account in
                check_key ~force:true key
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
                  Ton_sdk.ACTION.call_run
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
              CommandMultisig.create_multisig ~client key.key_name ?contract
            ) !to_deploy;

          ()

let cmd =
  let set_todo, with_todo = Misc.todo_arg () in
  EZCMD.sub
    "node"
    (fun () ->
       with_todo (fun todo ->
           action ~todo
         )
    )
    ~args: [

      [ "start" ], Arg.Unit (fun () -> set_todo "--start" NodeStart ),
      EZCMD.info "Start network node";

      [ "stop" ], Arg.Unit (fun () -> set_todo "--stop" NodeStop ),
      EZCMD.info "Stop network node";

      [ "web" ], Arg.Unit (fun () -> set_todo "--web" NodeWeb ),
      EZCMD.info "Open Node GraphQL webpage";

      [ "give" ], Arg.String (fun s -> set_todo "--give" (NodeGive s) ),
      EZCMD.info "ACCOUNT Give 1000 TON from giver to ACCOUNT ('all' for user*)";



    ]
    ~doc: "Manage local nodes"
