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

let action ~accounts ~prefix =
  let config = Config.config () in
  let net = Config.current_network config in

  match prefix with

  | Some prefix ->

      List.iter (fun account ->
          let key = Misc.find_key_exn net account in
          key.key_name <- prefix ^ key.key_name ;
          config.modified <- true ;
          Printf.eprintf "Account %S renamed to %S\n%!" account
            key.key_name ;
        ) accounts ;

  | None ->
      match accounts with
      | [] | [ _ ] | _ :: _ :: _ :: _ ->
          Error.raise "Takes 2 arguments: SRC_ACCOUNT and DST_ACCOUNT"
      | [ src ; dst ] ->
          let key = Misc.find_key_exn net src in
          key.key_name <- dst ;
          config.modified <- true ;
          Printf.eprintf "Account %S renamed to %S\n%!" src dst

let cmd =
  let accounts = ref [] in
  let prefix = ref None in
  EZCMD.sub
    "account rename"
    (fun () ->
       action ~accounts:!accounts ~prefix:!prefix
    )
    ~args:
      [

        [ "prefix" ], Arg.String (fun s -> prefix := Some s),
        EZCMD.info ~docv:"PREFIX" "Prefix provided accounts by PREFIX";

        [], Arg.Anons (fun args -> accounts := args),
        EZCMD.info ~docv:"ACCOUNTS" "Source and Destination accounts" ;

      ]
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command displays information on given accounts, either locally or from the blockchain";
        `P "Examples:";
        `Pre {|ft account copy mainnet my-account|};
        `Pre {|ft --switch testnet account copy my-account|}
      ];

    ]
    ~doc:
      "Get account info (local or from blockchain)."
