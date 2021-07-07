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

let is_multisig_contract = function
  | "SafeMultisigWallet"
  | "SetcodeMultisigWallet"
  | "SetcodeMultisigWallet2"
    -> true
  | _ -> false

let check_key_contract key =
  match key.key_account with
  | Some { acc_contract = Some acc_contract ; _ } ->
      if is_multisig_contract acc_contract then
        acc_contract
      else
        Error.raise "Account's contract %S is not multisig" acc_contract;

      (* Account's contract is not set, let's use the minimal common ABI *)
  | _ ->
      "SafeMultisigWallet"

let get_custodians account =
  let config = Config.config () in
  let net = Config.current_network config in
  let key = Misc.find_key_exn net account in
  let contract = check_key_contract key in
  let address = Misc.get_key_address_exn key in

  Utils.call_contract config
    ~contract
    ~address
    ~meth:"getCustodians"
    ~params:"{}"
    ~local:true
    ()

let action account =

  let account = match account with
    | None ->
        Error.raise "You must provide the account"
    | Some account -> account
  in
  get_custodians account ;
  ()

let cmd =
  let account = ref None in
  EZCMD.sub
    "multisig list custodians"
    (fun () ->
       action !account
    )
    ~args:
      [
        [], Arg.Anon (0, fun s -> account := Some s),
        EZCMD.info ~docv:"ACCOUNT" "The multisig account";
      ]
    ~doc: "Manage a multisig-wallet (create, confirm, send)"
    ~man:[
      `S "DESCRIPTION";
      `P "This command is used to manage a multisig wallet, i.e. create the wallet, send tokens and confirm transactions.";

      `S "CREATE MULTISIG";
      `P "Create an account and get its address:";
      `Pre {|# ft account --create my-account
# ft genaddr my-account|};
      `P "Backup the account info off-computer.";
      `P "The second command will give you an address in 0:XXX format. Send some tokens on the address to be able to deploy the multisig.";
      `P "Check its balance with:";
      `Pre {|# ft account my-account|};
      `P "Then, to create a single-owner multisig:";
      `Pre {|# ft multisig -a my-account --create|} ;
      `P "To create a multi-owners multisig:";
      `Pre {|# ft multisig -a my-account --create owner2 owner3 owner4|} ;
      `P "To create a multi-owners multisig with 2 signs required:";
      `Pre {|# ft multisig -a my-account --create owner2 owner3 --req 2|} ;
      `P "To create a multi-owners multisig not self-owning:";
      `Pre {|# ft multisig -a my-account --create owner1 owner2 owner3 --not-owner|} ;

      `P "Verify that it worked:";
      `Pre {|# ft account my-account -v|};

      `S "GET CUSTODIANS";
      `P "To get the list of signers:";
      `Pre {|# ft multisig -a my-account --custodians"|};

      `S "SEND TOKENS";
      `P "Should be like that:";
      `Pre {|# ft multisig -a my-account --transfer 100.000 --to other-account|};
      `P "If the target is not an active account:";
      `Pre {|# ft multisig -a my-account --transfer 100.000 --to other-account --parrain|};
      `P "To send all the balance:";
      `Pre {|# ft multisig -a my-account --transfer all --to other-account|};

      `S "CALL WITH TOKENS";
      `P "Should be like that:";
      `Pre {|# ft multisig -a my-account --transfer 100 --to contract set '{ "x": "100" }|};

      `S "LIST WAITING TRANSACTIONS";
      `P "Display transactions waiting for confirmations:";
      `Pre {|# ft multisig -a my-account --waiting|};

      `S "CONFIRM TRANSACTION";
      `P "Get the transaction ID from above, and use:";
      `Pre {|# ft multisig -a my-account --confirm TX_ID|};
    ]
