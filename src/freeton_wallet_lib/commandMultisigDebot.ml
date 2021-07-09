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

let action account =

  let config = Config.config () in
  let account = match account with
    | None -> "debot-multisig"
    | Some account -> account in
  let address = Utils.address_of_account config account in
  let address = Misc.raw_address address in
  CommandClient.action ~exec:false [ "debot" ; "fetch" ; address ] ;
  ()

let cmd =
  let account = ref None in
  EZCMD.sub
    "multisig debot"
    (fun () ->
       action !account
    )
    ~args:
      [
        [], Arg.Anon (0, fun s -> account := Some s),
        EZCMD.info ~docv:"ACCOUNT" "The debot account";

      ]
    ~doc: "Executes the multisig debot"
    ~man:[
      `S "DESCRIPTION";
      `P "This command executes the multisig debot, whose address is in the 'debot-multisig' account. ";

    ]
