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

let action ~account ~passphrase ~address ~contract ~keyfile ~wc ~initial_data =
  let config = Config.config () in
  match account with
  | None -> Error.raise "An account name must be provided"
  | Some name ->
      CommandAccountCreate.change_account config
        ~name ?passphrase ?address ?contract ?keyfile ?wc
        ~initial_data ()

let cmd =
  let account = ref None in
  let passphrase = ref None in
  let address = ref None in
  let contract = ref None in
  let keyfile = ref None in
  let wc = ref None in
  let static_vars = ref None in
  EZCMD.sub
    "account set"
    (fun () -> action
        ~account:!account
        ~passphrase:!passphrase
        ~address:!address
        ~contract:!contract
        ~keyfile:!keyfile
        ~wc:!wc
        ~initial_data:!static_vars
    )
    ~args:
      [ [],
        Arg.Anon (0, fun s -> account := Some s ),
        EZCMD.info "Name of account" ;

        [ "passphrase"],
        Arg.String (fun s -> passphrase := Some s),
        EZCMD.info ~docv:"PASSPHRASE" "BIP39 Passphrase for account";

        [ "address"],
        Arg.String (fun s ->
            let s = match EzString.chop_prefix s ~prefix:"0-1:" with
                None -> s
              | Some s -> "-1:" ^ s
            in
            address := Some s),
        EZCMD.info ~docv:"ADDRESS" "Address for account";

        [ "contract"],
        Arg.String (fun s -> contract := Some s),
        EZCMD.info ~docv:"CONTRACT" "Contract for account";

        [ "static-vars"],
        Arg.String (fun s -> static_vars := Some s),
        EZCMD.info ~docv:"JSON" "Set static vars for account";

        [ "surf" ],
        Arg.Unit (fun () -> contract := Some "SetcodeMultisigWallet2"),
        EZCMD.info "Contract should be TON Surf contract" ;

        [ "multisig" ],
        Arg.Unit (fun () -> contract := Some "SafeMultisigWallet"),
        EZCMD.info "Contract should be multisig" ;

        [ "keyfile"],
        Arg.String (fun s -> keyfile := Some s),
        EZCMD.info ~docv:"KEYFILE" "Key file for account";

        [ "wc" ], Arg.Int (fun s -> wc := Some s),
        EZCMD.info ~docv:"WORKCHAIN" "The workchain (default is 0)";

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
