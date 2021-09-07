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

open Ton_sdk.TYPES
open Types

let get_key_info key ~json ~secrets =
  if json then
    let json = EzEncoding.construct ~compact:false Encoding.key key in
    Printf.printf "%s\n%!" json
  else begin
    Printf.printf "Name: %s\n" key.key_name ;
    Option.iter (fun acc ->
        Printf.printf "  %s\n%!" acc.acc_address;
        Option.iter (fun s ->
            Printf.printf "  Contract: %s\n" s
          ) acc.acc_contract
      ) key.key_account;
    Option.iter (fun pair ->
        Printf.printf "  Public: 0x%s\n" pair.public ;
        if secrets then
          Option.iter (fun s ->
              Printf.printf "  Secret: %s\n" s ;
            ) pair.secret
      ) key.key_pair ;
    if secrets then
      Option.iter (fun s ->
          Printf.printf "  Passphrase:\n   %s\n" s) key.key_passphrase;
    Printf.printf "%!";
  end

let get_account_info accounts ~json ~secrets =

  let config = Config.config () in
  let net = Config.current_network config in
  match accounts with
  | [] -> List.iter (fun key ->
      match key.key_account with
      | None -> ()
      | Some _ ->
          get_key_info key ~json ~secrets
    ) net.net_keys
  | names ->
      List.iter (fun name ->
          match Misc.find_key net name with
          | None ->
              Error.raise "No key %S in network %S" name net.net_name
          | Some key ->
              get_key_info key ~json ~secrets
        ) names

let action accounts ~json ~secrets =
  get_account_info accounts ~json ~secrets

let cmd =
  let accounts = ref [] in
  let secrets = ref false in
  let json = ref false in
  EZCMD.sub
    "account info"
    (fun () -> action
        !accounts ~json:!json ~secrets:!secrets
    )
    ~args:
      [ [],
        Arg.Anons (fun args -> accounts := args),
        EZCMD.info "Name of account" ;

        [ "json" ], Arg.Set json,
        EZCMD.info "Print in json format";

        [ "S"; "secrets" ], Arg.Set secrets,
        EZCMD.info "Print passphrase and secret key (default with --json)";
      ]
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command displays information on given accounts, either locally or from the blockchain";
        `P "Examples:";
        `Pre {|ft account info MY-ACCOUNT|};
      ];

    ]
    ~doc:
      "Get account info (local or from blockchain)."
