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

(*
Known code hashes:
* DePool:
14e20e304f53e6da152eb95fffc993dbd28245a775d847eed043f7c78a503885
* SetcodeMultisigWallet2:
207dc560c5956de1a2c1479356f8f3ee70a59767db2bf4788b1d61ad42cdad82
* Giver:
fdfab26e1359ddd0c247b0fb334c2cc3943256a263e75d33e5473567cbe2c124

*)

let get_account_info config address =
  let addr = Misc.raw_address address in
  let open Ton_sdk in
  let level = if !Globals.verbosity > 1 then 3 else 1 in
  match
    Utils.post config
      ( REQUEST.account ~level addr )
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
                          contract acc.acc_address;
                        acc.acc_contract <- Some contract;
                        config.modified <- true
      end;
      Some account
  | _ -> assert false


let cut v =
  let rem = Int64.rem v 1_000L in
  let v = Int64.div v 1_000L in
  v, rem

let string_of_nanoton v =
  let v, nanotons = cut v in
  let v, mutons = cut v in
  let v, millitons = cut v in
  let v, tons = cut v in
  let v, thousandtons = cut v in
  let v, milliontons = cut v in
  let v, billiontons = cut v in
  assert (v = 0L);
  let tons =
    match billiontons, milliontons, thousandtons with
    | 0L, 0L, 0L -> Int64.to_string tons
    | 0L, 0L, _ -> Printf.sprintf "%Ld_%03Ld" thousandtons tons
    | 0L, _, _ -> Printf.sprintf "%Ld_%03Ld_%03Ld" milliontons thousandtons tons
    | _, _, _ -> Printf.sprintf "%Ld_%03Ld_%03Ld_%03Ld"
                   billiontons milliontons thousandtons tons
  in
  let nanotons = match nanotons, mutons, millitons with
    | 0L, 0L, 0L -> ""
    | 0L, 0L, _ -> Printf.sprintf "%03Ld" millitons
    | 0L, _, _ -> Printf.sprintf "%03Ld_%03Ld" millitons mutons
    | _, _, _ -> Printf.sprintf "%03Ld_%03Ld_%03Ld" millitons mutons nanotons
  in
  let s = Printf.sprintf "%s.%s" tons nanotons in
  s

let get_account_info config ~name ~address =
  match get_account_info config address with
  | None ->
      Printf.printf "Account %S: not yet created (empty balance)\n%!" name
  | Some account ->
      Printf.printf "Account %S: %s\n%!" name
        (match account.acc_balance with
         | None -> "no balance"
         | Some n ->
             Printf.sprintf "%s TONs (%s)"
               (string_of_nanoton (Z.to_int64 n))
               (match account.acc_type_name with
                | None -> "Non Exists"
                | Some s ->
                    match address with
                    | Account { acc_contract = Some contract; _ } ->
                        Printf.sprintf "%s: %s" contract s
                    | _ -> s
               ))

let get_key_info config key ~info =
  if info then
    let json = EzEncoding.construct ~compact:false Encoding.key key in
    Printf.printf "%s\n%!" json
  else
    let address = match key.key_account with
      | None ->
          Error.raise "Address %s has no address (use genaddr before)"
            key.key_name
      | Some account -> Account account
    in
    get_account_info config ~address ~name:key.key_name

let get_account_info accounts ~info =

  let config = Config.config () in
  let net = Config.current_network config in
  if info then
    match accounts with
    | [] -> List.iter (fun key ->
        match key.key_account with
        | None -> ()
        | Some _ ->
            get_key_info config key ~info
      ) net.net_keys
    | names ->
        List.iter (fun name ->
            match Misc.find_key net name with
            | None ->
                Error.raise "No key %S in network %S" name net.net_name
            | Some key ->
                get_key_info config key ~info
          ) names
  else
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
            let address = Utils.address_of_account config account in
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
      let address = Utils.address_of_account config account in
      let addr = Misc.raw_address address in
      let url = Printf.sprintf
          "https://%s/accounts/accountDetails?id=%s" host addr in
      Misc.call [ "xdg-open" ; url ]
    ) accounts


let action accounts ~info ~live =
  if live then
    get_live accounts
  else
    get_account_info accounts ~info

let cmd =
  let accounts = ref [] in
  let info = ref false in
  let live = ref false in
  EZCMD.sub
    "account info"
    (fun () -> action
        !accounts
        ~info:!info
        ~live:!live
    )
    ~args:
      [ [],
        Arg.Anons (fun args -> accounts := args),
        EZCMD.info "Name of account" ;

        [ "all" ] ,
        Arg.Set info,
        EZCMD.info "Display all account parameters" ;

        [ "live" ],
        Arg.Set live,
        EZCMD.info "Open block explorer on address";

      ]
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command displays information on given accounts, either locally or from the blockchain";
        `P "Examples:";
        `Pre {|ft account info my-account|};
        `Pre {|ft account info my-account --all|}
      ];

    ]
    ~doc:
      "Get account info (local or from blockchain)."
