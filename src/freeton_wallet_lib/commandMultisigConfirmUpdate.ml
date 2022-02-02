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
  let contract = "SetcodeMultisigWallet2" in

  let meth = "confirmUpdate" in
  let params =
    Printf.sprintf
      {|{"updateId":"%s"}|} tx_id
  in

  Utils.call_contract config ~contract
    ~address
    ~meth ~params
    ~local:false
    ~src
    ()

let action ~account ~updates ?src () =
  List.iter (fun tx_id ->
      send_confirm ~account ~tx_id ?src ()
    ) updates

let cmd =
  let args = ref [] in
  let src = ref None in
  let all = ref false in
  Misc.cmd
    [ "multisig" ; "confirm" ; "update" ]
    (fun () ->
       match !args with
       | [] ->
           Error.raise "You must provide the MULTISIG address first"
       | account :: updates ->
           match updates with
           | [] ->
               if !all then
                 CommandMultisigListUpdates.get_waiting
                   ~f:(fun tr ->
                       send_confirm ~account ~tx_id:tr.update_id ?src:!src ()
                     )
                   account
               else
                 Error.raise "You must provide the update ID or --all"
           | _ ->
               action ~account ~updates ?src:!src ()
    )
    ~args:
      [
        [], Arg.Anons ( fun list -> args := list),
        EZCMD.info ~docv:"ACCOUNT TX_ID" "The multisig account and the TX_ID";

        [ "src" ], Arg.String (fun s -> src := Some s),
        EZCMD.info ~docv:"ACCOUNT" "The signing custodian";

        [ "all" ], Arg.Set all,
        EZCMD.info "Sign all pending updates";

      ]
    ~doc: "Confirm updates on a multisig-wallet"
    ~man:[
      `S "DESCRIPTION";
      `P "This command is used to confirm updates on a multisig wallet." ;

      `S "LIST WAITING UPDATES";
      `P "Display updates waiting for confirmations:";
      `Pre {|# ft multisig list updates MY-ACCOUNT|};

      `S "CONFIRM UPDATE";
      `P "Get the update ID from above, and use:";
      `Pre {|# ft multisig confirm MY-ACCOUNT TX_ID|};
    ]
