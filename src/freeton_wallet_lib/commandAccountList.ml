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
open Types

let shorten_key s =
  Printf.sprintf "%s.." (String.sub s 0 6)

let shorten_addr s =
  Printf.sprintf "%s.." (String.sub s 0 8)

let action () =
  let config = Config.config () in
  let net = Config.current_network config in

  List.iter (fun key ->
      Printf.printf "* %S%s%s%s\n"
        key.key_name
        (match key.key_passphrase with
         | Some _ -> " pass"
         | None ->
             match key.key_pair with
             | Some { secret = Some _ ; _ }  -> " secr"
             | _ -> "")
        (match key.key_pair with
         | None -> ""
         | Some pair ->
             Printf.sprintf " pk:%s"
               (shorten_key pair.public))
        (match key.key_account with
         | None -> ""
         | Some acc ->
             Printf.sprintf " ad: %s%s" (shorten_addr acc.acc_address)
               (match acc.acc_contract with
                | None -> ""
                | Some s -> Printf.sprintf " (%s)" s)
        )
    ) net.net_keys

let cmd =
  EZCMD.sub
    "account list"
    ( fun () -> action () )
    ~args:
      [
      ]
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command lists all known accounts in the wallet." ;
        `P "Examples:";
        `Pre {|ft account list|};
        `P "pass: passphrase known";
        `P "secr: secret key known";
        `P "pk: public key";
        `P "ad: address known";
      ];
    ]
    ~doc:
      "List all known accounts in the current switch"
