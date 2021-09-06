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

let copy_key key = { key with
                     key_account = match key.key_account with
                       | None -> None
                       | Some account ->
                           Some { account
                                  with acc_address = account.acc_address }
                   }

let action ~switch ~accounts ~rename ~prefix =
  let config = Config.config () in
  let target_switch = Config.current_network config in
  if target_switch.net_name = switch then
    Error.raise "Cannot copy from current switch";
  let found = ref false in
  List.iter (fun net ->
      if net.net_name = switch then begin
        found := true ;
        Config.load_wallet config net ;
        let keys =
          match accounts with
          | [] ->
              net.net_keys
          | _ ->
              let keys = ref [] in
              List.iter (fun account ->
                    keys := Misc.find_key_exn net account :: !keys
                ) accounts ;
              !keys
        in
        let keys = List.map copy_key keys in
        let keys = match rename, keys with
          | None, _ -> keys
          | Some key_name, [ key ] ->
              [ { key with key_name } ]
          | _ -> Error.raise "--rename can only be used with one account"
        in
        let keys = match prefix with
          | None -> keys
          | Some prefix ->
              List.map (fun key ->
                  { key with key_name = prefix ^ key.key_name } ) keys
        in
        let existing_keys =
          let set = ref StringSet.empty in
          List.iter (fun key -> set := StringSet.add key.key_name !set)
            target_switch.net_keys ;
          !set
        in
        let keys =
          let r = ref [] in
          List.iter (fun key ->
              if not ( StringSet.mem key.key_name existing_keys ) then
                r := key :: !r
            ) keys ;
          !r
        in
        target_switch.net_keys <- target_switch.net_keys @
                                  keys;
        config.modified <- true;
        Printf.printf "%d keys copied\n%!" ( List.length keys )
      end;
    ) config.networks;
  if not !found then
    Error.raise "Switch %S does not exist" switch


let cmd =
  let accounts = ref [] in
  let rename = ref None in
  let prefix = ref None in
  EZCMD.sub
    "account copy from"
    (fun () ->
       match !accounts with
       | [] -> Error.raise "You must specify a SWITCH name first"
       | switch :: accounts ->
           action ~switch ~accounts ~rename:!rename ~prefix:!prefix
    )
    ~args:
      [

        [ "rename" ], Arg.String (fun s -> rename := Some s),
        EZCMD.info ~docv:"ACCOUNT" "New name of account";

        [ "prefix" ], Arg.String (fun s -> prefix := Some s),
        EZCMD.info ~docv:"PREFIX" "Prefix created accounts by PREFIX";

        [], Arg.Anons (fun args -> accounts := args),
        EZCMD.info ~docv:"SWITCH ACCOUNTS" "Switch name and accounts" ;

      ]
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command copies accounts from another switch to the \
            current one";
        `P "Examples:";
        `Pre {|ft account copy mainnet my-account|};
        `Pre {|ft --switch testnet account copy my-account|}
      ];

    ]
    ~doc:
      "Copy accounts from another switch to the current one"
