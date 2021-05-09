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

let action ~deployer =
  let config = Config.config () in
  let net = Config.current_network config in
  begin
    match deployer with
    | None -> ()
    | Some deployer ->
        net.net_deployer <- deployer ;
        Printf.eprintf "Deployer set to %S\n5!" deployer;
        config.modified <- true
  end;
  ()

let cmd =
  let deployer = ref None in
  EZCMD.sub
    "config"
    (fun () ->
       action
         ~deployer:!deployer
    )
    ~args:
      [
        [ "deployer" ], Arg.String ( fun s -> deployer := Some s ),
        EZCMD.info ~docv:"ACCOUNT"
          "Set deployer to account ACCOUNT. The deployer is the \
           account used to credit the initial balance of an address \
           before deploying a contract on it." ;
      ]
    ~doc: "Modify configuration"
    ~man:[
      `S "DESCRIPTION";
      `P "Change the global configuration or the network configuration.";
    ]
