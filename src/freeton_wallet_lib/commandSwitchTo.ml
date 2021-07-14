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

let action ~switch =
    match switch with
    | None ->
        Error.raise "You must provide the new switch"
    | Some net_name ->
        let config = Config.config () in
        if List.for_all (fun net -> net.net_name <> net_name )
            config.networks then
          Error.raise "Network %S does not exist" net_name ;
        config.current_network <- net_name ;
        config.modified <- true;
        Printf.eprintf "Switched to network %S\n%!" net_name

let cmd =
  let switch = ref None in
  EZCMD.sub
    "switch to"
    (fun () ->
       action
         ~switch:!switch
    )
    ~args: (
      [ ( [],
          Arg.Anon (0, fun s -> switch := Some s),
          EZCMD.info ~docv:"NETWORK" "Name of network switch" );
      ] )
    ~doc: "Switch to another network/switch"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command is used to switch the current network \
            configuration to the provided switch. To only switch to a \
            new network for a particular command, it is possible to \
            use the --switch argument instead."
      ];

      `S "EXAMPLE";
      `Blocks [
        `P "Display current network and other existing networks:";
        `Pre {|$ ft switch list|};
        `P "Change current network to an existing network NETWORK:";
        `Pre {|$ ft switch to NETWORK|};
        `P "Call a command for another switch:";
        `Pre {|$ ft --switch NETWORK call contract get '{}'|};
      ];
    ]
