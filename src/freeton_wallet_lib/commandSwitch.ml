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
open EzFile.OP

let action ~switch ~create ~url ~remove =
  begin
    match switch with
    | None ->
        if create || remove then
          Error.raise
            "You must specify the name of the switch to perform an action of it"
        else
          Config.print ()
    | Some net_name ->
        if create then begin
          let config = Config.config () in
          if List.exists (fun net -> net.net_name = net_name )
              config.networks then
            Error.raise "Network %S already exists" net_name ;
          let add_network ?(net_keys=[]) ?(net_deployer="deployer") node =
            let net = {
              net_name ;
              net_nodes = [ node ] ;
              current_node = node.node_name ;
              current_account = None ;
              net_keys ;
              net_deployer ;
            } in
            config.networks <- config.networks @ [ net ];
            config.current_network <- net_name ;
            config.modified <- true
          in
          match url, EzString.chop_prefix ~prefix:"sandbox" net_name with
          | Some node_url, None ->
              add_network {
                node_name = "node" ;
                node_url ;
                node_local = None ;
              }
          | Some _node_url, Some _ ->
              Error.raise "sandbox network %S URL cannot be specified" net_name
          | None, None ->
              Error.raise
                "New network %S must either be sandboxed 'sandboxN' or remote (--url)"
                net_name

          | None, Some n ->
              let n = int_of_string n in
              let local_port = 7080+n in
              let node_local = { local_port } in
              Misc.call [
                "docker"; "create";
                "--name"; CommandNode.container_of_node node_local ;
                "-e" ; "USER_AGREEMENT=yes" ;
                Printf.sprintf "-p%d:80" local_port ;
                "tonlabs/local-node"
              ];
              add_network
                ~net_keys:Config.sandbox_keys
                ~net_deployer:"user1"
                {
                  node_name = "node" ;
                  node_url = Printf.sprintf "http://0.0.0.0:%d"  local_port ;
                  node_local = Some node_local;
                }
        end else
        if remove then begin
          let config = Config.config () in
          begin
            match net_name with
            | "mainnet" | "testnet" ->
                Error.raise "Cannot remove predefined switch %S" net_name
            | _ -> ()
          end;
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
                    let container = CommandNode.container_of_node local_node in
                    Misc.call [ "docker" ; "rm" ; container ];
            ) config.networks ;
          config.networks <-
            List.filter (fun net -> net.net_name <> net_name ) config.networks;
          Misc.call [ "rm" ; "-rf" ; Globals.ft_dir // net_name ];
          config.modified <- true
        end else begin
          let config = Config.config () in
          if List.for_all (fun net -> net.net_name <> net_name )
              config.networks then
            Error.raise "Network %S does not exist" net_name ;
          config.current_network <- net_name ;
          config.modified <- true;
          Printf.eprintf "Switched to network %S\n%!" net_name
        end
  end

let cmd =
  let switch = ref None in
  let url = ref None in
  let create = ref false in
  let remove = ref false in
  EZCMD.sub
    "switch"
    (fun () ->
       action
         ~switch:!switch
         ~create:!create
         ~url:!url
         ~remove:!remove)
    ~args: (
      [ ( [],
          Arg.Anon (0, fun s -> switch := Some s),
          EZCMD.info ~docv:"NETWORK" "Name of network switch" );

        ( [ "create" ], Arg.Set create,
          EZCMD.info "Create switch for a new network" );

        ( [ "delete" ; "remove" ], Arg.Set remove,
          EZCMD.info "Remove switch of a network" );

        ( [ "url" ], Arg.String ( fun s -> url := Some s ),
          EZCMD.info ~docv:"URL" "URL of the default node in this network" );
      ] )
    ~doc: "Display or change current network"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "Manage the different networks. Each switch includes a set \
            of accounts and nodes. TONOS SE local networks can be \
            created with this command (see the SANDBOXING section \
            below).";
      ];

      `S "EXAMPLES";
      `Blocks [
        `P "Display current network and other existing networks:";
        `Pre {|$ ft switch|};
        `P "Change current network to an existing network NETWORK:";
        `Pre {|$ ft switch NETWORK|};
        `P "Create a new network with name NETWORK and url URL, and switch to that network:";
        `Pre {|$ ft switch --create NETWORK --url URL|};
        `P "Removing a created network:";
        `Pre {|$ ft switch --remove NETWORK|};
      ];

      `S "SANDBOXING";
      `Blocks [
        `P "As a specific feature, ft can create networks based on TONOS SE to run on the local computer. Such networks are automatically created by naming the network 'sandboxN` where N is a number. The corresponding node will run on port 7080+N.";
        `P "Example of session (create network, start node, give user1 1000 TONs):";
        `Pre {|$ ft switch --create sandbox1|};
        `Pre {|$ ft node --start|};
        `Pre {|$ ft node --give user1:1000|};
        `P "When a local network is created, it is initialized with:";
        `I ("1.", "An account 'giver' corresponding to the Giver contract holding 5 billion TONS");
        `I ("2.", "A set of 10 accounts 'user0' to 'user9'. These accounts always have the same secret keys, so it is possible to define test scripts that will work on different instances of local networks.");
        `P "The 10 accounts are not deployed, but it is possible to use 'ft node --give ACCOUNT' to automatically deploy the account."
      ];

    ]
