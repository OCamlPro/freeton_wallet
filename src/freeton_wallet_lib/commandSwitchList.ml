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

let action () =
  Config.print ()

let cmd =
  EZCMD.sub
    "switch list"
    (fun () ->
       action ()
    )
    ~args: []
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
        `Pre {|$ ft switch create NETWORK --url URL|};
        `P "Removing a created network:";
        `Pre {|$ ft switch remove NETWORK|};
      ];

      `S "SANDBOXING";
      `Blocks [
        `P "As a specific feature, ft can create networks based on TONOS SE to run on the local computer. Such networks are automatically created by naming the network 'sandboxN` where N is a number. The corresponding node will run on port 7080+N.";
        `P "Example of session (create network, start node, give user1 1000 TONs):";
        `Pre {|$ ft switch create sandbox1|};
        `Pre {|$ ft node start|};
        `Pre {|$ ft node give user1:1000|};
        `P "When a local network is created, it is initialized with:";
        `I ("1.", "An account 'giver' corresponding to the Giver contract holding 5 billion TONS");
        `I ("2.", "A set of 10 accounts 'user0' to 'user9'. These accounts always have the same secret keys, so it is possible to define test scripts that will work on different instances of local networks.");
        `P "The 10 accounts are not deployed, but it is possible to use 'ft node --give ACCOUNT' to automatically deploy the account."
      ];

    ]
