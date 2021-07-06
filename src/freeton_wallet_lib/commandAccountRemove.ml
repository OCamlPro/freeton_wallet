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

let action accounts =
  let config = Config.config () in
  let net = Config.current_network config in
  List.iter (fun name ->
      Misc.delete_account config net name ;
      Printf.eprintf "Account %S deleted\n%!" name
    ) accounts

let cmd =
  let accounts = ref [] in
  EZCMD.sub
    "account remove"
    ( fun () -> action !accounts )
    ~args:
      [
        [], Arg.Anons (fun args -> accounts := args),
        EZCMD.info "Name of account" ;

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
