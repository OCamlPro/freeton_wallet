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

let send_confirm ~account ?src ~tx_id () =
  let config = Config.config () in
  let net = Config.current_network config in
  let address = Utils.address_of_account config account in
  let address = Misc.raw_address address in
  let src = match src with
      None -> account
    | Some src -> src
  in
  let src = Misc.find_key_exn net src in
  let contract = "SafeMultisigWallet" in

  let meth = "confirmTransaction" in
  let params =
    Printf.sprintf
      {|{"transactionId":"%s"}|} tx_id
  in

  Utils.call_contract config ~contract
    ~address
    ~meth ~params
    ~local:false
    ~src
    ()

let action ~account ~transactions ~src =
  List.iter (fun tx_id ->
      send_confirm ~account ~tx_id ?src ()
    ) transactions

let cmd =
  let args = ref [] in
  let src = ref None in
  EZCMD.sub
    "multisig confirm"
    (fun () ->
       match !args with
       | [] | [ _ ] ->
           Error.raise "You must at least provide the ACCOUNT and the TX_ID"
       | account :: transactions ->
           action ~account ~transactions ~src:!src
    )
    ~args:
      [
        [], Arg.Anons ( fun list -> args := list),
        EZCMD.info ~docv:"ACCOUNT TX_ID" "The multisig account and the TX_ID";

        [ "src" ], Arg.String (fun s -> src := Some s),
        EZCMD.info ~docv:"ACCOUNT" "The multisig account";

      ]
    ~doc: "Confirm transactions on a multisig-wallet"
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
