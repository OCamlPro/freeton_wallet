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

let get_address_info config address =
  let addr = Misc.raw_address address in
  let open Ton_sdk in
  let level = if !Globals.verbosity > 1 then 3 else 1 in
  match
    Utils.post config
      ( REQUEST.account ~level ( ADDRESS.to_string addr ))
  with
  | [] -> None
  |  [ account ] ->
      if !Globals.verbosity > 1 then
        Format.printf "%s@."
          (EzEncoding.construct ~compact:false ENCODING.accounts_enc [account]
          );
      begin
        match account.acc_code_hash with
        | None -> ()
        | Some code_hash ->
            match address with
            | RawAddress _ -> ()
            | Account acc ->
                match acc.acc_contract with
                | Some _ -> ()
                | None ->
                    match Misc.contract_of_code_hash ~code_hash with
                    | None -> ()
                    | Some contract ->
                        Printf.eprintf "Setting contract %S for %s\n%!"
                          contract ( ADDRESS.to_string acc.acc_address );
                        acc.acc_contract <- Some contract;
                        config.modified <- true
      end;
      Some account
  | _ -> assert false


let get_account_info config ~name ~address =
  match get_address_info config address with
  | None ->
      Printf.printf "Account %S: not yet created (empty balance)\n%!" name
  | Some account ->
      Printf.printf "Account %S: %s\n%!" name
        (match account.acc_balance with
         | None -> "no balance"
         | Some n ->
             Printf.sprintf "%s TONs (%s)"
               (Misc.string_of_nanoton (Z.to_int64 n))
               (match account.acc_type_name with
                | None -> "Non Exists"
                | Some s ->
                    match address with
                    | Account { acc_contract = Some contract; _ } ->
                        Printf.sprintf "%s: %s" contract s
                    | _ -> s
               ))

let get_account_info accounts =

  let config = Config.config () in
  let net = Config.current_network config in
  match accounts with
  | [] ->
      List.iter (fun key ->
          match key.key_account with
          | None -> ()
          | Some account ->
              get_account_info config
                ~address:( Account account ) ~name:key.key_name
        ) net.net_keys
  | _ ->
      List.iter (fun account ->
          let address = Utils.address_of_account net account in
          get_account_info config ~address ~name:account
        ) accounts

let get_live accounts =
  let config = Config.config () in
  let net = Config.current_network config in
  let host = match net.net_name with
    | "mainnet" -> "ton.live"
    | "testnet" -> "net.ton.live"
    | _ -> assert false
  in
  List.iter (fun account ->
      let address = Utils.address_of_account net account in
      let addr = Misc.raw_address address in
      let url = Printf.sprintf
          "https://%s/accounts/accountDetails?id=%s" host
          ( ADDRESS.to_string addr ) in
      Misc.call [ "xdg-open" ; url ]
    ) accounts


let action accounts ~live =
  if live then
    get_live accounts
  else
    get_account_info accounts

let cmd =
  let accounts = ref [] in
  let live = ref false in
  Misc.cmd
    ["account" ; "state"]
    (fun () -> action !accounts ~live:!live
    )
    ~args:
      [ [],
        Arg.Anons (fun args -> accounts := args),
        EZCMD.info "Name of account" ;

        [ "live" ],
        Arg.Set live,
        EZCMD.info "Open block explorer on address";

      ]
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command displays the current states of the given accounts from the blockchain";
        `P "Examples:";
        `Pre {|ft account state MY-ACCOUNT|};
        `Pre {|ft account state MY-ACCOUNT -v|}
      ];

    ]
    ~doc:
      "Get account info (local or from blockchain)."
