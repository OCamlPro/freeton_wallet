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
        `P "This command deletes known accounts from the wallet." ;
        `P "Examples:";
        `Pre {|ft account remove account1 account2|};
      ];

    ]
    ~doc:
      "Delete accounts from the wallet"
