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

open Ez_file.V1
open Ezcmd.V2
open EZCMD.TYPES
open Types
open EzFile.OP

let action ~switches ~force =
  match switches with
  | [] ->
      Error.raise
        "You must specify the name of the switch to remove"
  | _ ->
      List.iter (fun net_name ->
          let config = Config.config () in
          if not force then
            List.iter (fun net ->
                if net_name = net.net_name then
                  Error.raise
                    "Cannot remove predefined switch %S (use -f to force)" net_name
              ) Config.known_networks ;
          if List.for_all (fun net -> net.net_name <> net_name )
              config.networks then
            Error.raise "Network %S does not exist" net_name ;
          if config.current_network = net_name then
            Error.raise "Cannot remove current switch %S" net_name;
          List.iter (fun net ->
              if net.net_name = net_name then
                let node = Misc.current_network_node net in
                match node.node_local with
                | None -> ()
                | Some local_node ->
                    let container =
                      CommandNodeStart.container_of_node local_node in
                    try
                      Misc.call [ "docker" ; "rm" ; container ];
                    with exn ->
                      if force then begin
                        Printf.eprintf "Exception: %s\n%!"
                          ( Printexc.to_string exn )
                      end else raise exn
            ) config.networks ;
          config.networks <-
            List.filter (fun net -> net.net_name <> net_name ) config.networks;
          Misc.call [ "rm" ; "-rf" ; Globals.ft_dir // net_name ];
          config.modified <- true
        ) switches

let cmd =
  let switches = ref [] in
  let force = ref false in
  EZCMD.sub
    "switch remove"
    (fun () ->
       action ~switches:!switches ~force:!force
    )
    ~args: [
      [], Arg.Anons (fun list -> switches := list),
      EZCMD.info ~docv:"NETWORK" "Name of network switch to remove" ;

      [ "f" ; "force" ], Arg.Set force,
      EZCMD.info "Remove network even in case of failure" ;
    ]
    ~doc: "Remove a network configurations/switches"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "Remove network configurations";
      ];
      `S "EXAMPLES";
      `Blocks [
        `P "Removing a created network:";
        `Pre {|$ ft switch remove NETWORK|};
      ];
    ]
