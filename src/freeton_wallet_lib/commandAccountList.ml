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
  let len = String.length s in
  Printf.sprintf "%s/%s" (String.sub s 0 4) (String.sub s (len-4) 4)

let shorten_addr s =
  let len = String.length s in
  Printf.sprintf "%s/%s" (String.sub s 0 6) (String.sub s (len-4) 4)

let action () =
  let config = Config.config () in
  let net = Config.current_network config in

  List.iter (fun key ->
      Printf.printf "* %S%s%s%s\n"
        key.key_name
        (match key.key_passphrase with
         | Some _ -> " P"
         | None -> "")
        (match key.key_pair with
         | None -> ""
         | Some pair ->
             Printf.sprintf " %s%s"
               (shorten_key pair.public)
               (match pair.secret with
                | None -> ""
                | Some _ -> " P"))
        (match key.key_account with
         | None -> ""
         | Some acc ->
             Printf.sprintf " %s%s" (shorten_addr acc.acc_address)
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
        `P "This command can perform the following actions:";
        `I ("1.", "Display information on given accounts, either locally or from the blockchain");
        `I ("2.", "Create new accounts");
        `I ("3.", "Add information to existing accounts");
        `I ("4.", "Delete existing accounts");
      ];
      `S "DISPLAY LOCAL INFORMATION";
      `Blocks [
        `P "Examples:";
        `Pre {|ft account --list|};
        `Pre {|ft account my-account --info|}
      ];
      `S "DISPLAY BLOCKCHAIN INFORMATION";
      `Blocks [
        `P "Accounts must have an address on the blockchain.";
        `P "Examples:";
        `Pre {|ft account my-account|};
        `Pre {|ft account|};
      ];
      `S "CREATE NEW ACCOUNTS";
      `Blocks [
        `P "Examples:";
        `Pre {|ft account --create account1 account2 account3|};
        `Pre {|ft account --create new-account --passphrase "some known passphrase"|};
        `Pre {|ft account --create new-account --contract SafeMultisigWallet|};
        `Pre {|ft account --create new-address --address 0:1234...|};
        `P "Only the last one will compute an address on the blockchain, since the contract must be known.";
      ];
      `S "COMPLETE EXISTING ACCOUNTS";
      `Blocks [
        `P "Examples:";
        `Pre {|ft account old-account --contract SafeMultisigWallet|};
      ];
      `S "DELETE EXISTING ACCOUNTS";
      `Blocks [
        `P "Examples:";
        `Pre {|ft account --delete account1 account2|};
      ];

    ]
    ~doc:
      "Get account info (local or from blockchain), or create/modify/delete accounts."
