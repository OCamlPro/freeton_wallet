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

let get_custodians account =
  let config = Config.config () in
  let net = Config.current_network config in
  let key = Misc.find_key_exn net account in
  let contract = CommandMultisigCreate.check_key_contract key in
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
    ~doc: "List owners/custodians of a multisig wallet"
    ~man:[
      `S "DESCRIPTION";
      `P "This command can be used to display the pubkeys of the owners/custodians of a multisig wallet";
      `P "To get the list of signers:";
      `Pre {|# ft multisig list custodians MY-ACCOUNT"|};
    ]
