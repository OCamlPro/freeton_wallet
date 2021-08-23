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

  let subst, _ = Subst.subst_string config in
  let passphrase = Option.map subst passphrase in
  let initial_data = Option.map subst initial_data in

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
        `P "This command adds information to existing accounts in the wallet";
        `P "Examples:";
        `Pre {|ft account set old-account --contract SafeMultisigWallet|};
      ];
    ]
    ~doc:
      "Modify the information associated with accounts in the wallet"
