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
  let address = Utils.address_of_account net account in
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
      `P "This command is used to confirm transactions on a multisig wallet." ;

      `S "LIST WAITING TRANSACTIONS";
      `P "Display transactions waiting for confirmations:";
      `Pre {|# ft multisig list transactions MY-ACCOUNT|};

      `S "CONFIRM TRANSACTION";
      `P "Get the transaction ID from above, and use:";
      `Pre {|# ft multisig confirm MY-ACCOUNT TX_ID|};
    ]
