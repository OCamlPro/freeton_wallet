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
open Types
(* open EzFile.OP *)

let container_of_node local_node =
  Printf.sprintf "local-node-%d" local_node.local_port

(* Currently, the giver creates an account with Contract:
"code_hash":
     "207dc560c5956de1a2c1479356f8f3ee70a59767db2bf4788b1d61ad42cdad82",
for SetcodeMultisigWallet2
whereas
"code_hash":
     "80d6c47c4a25543c9b397b71716f3fae1e2c5d247174c52e2c19bd896442b105",
for SafeMultisigWallet
*)

let action () =
  let config = Config.config () in
  let node = Config.current_node config in
  match node.node_local with
  | None ->
      Error.raise "cannot manage remote node %S" node.node_name
  | Some local_node ->
          Misc.call [ "docker"; "start" ; container_of_node local_node ]

let cmd =
  EZCMD.sub
    "node start"
    (fun () ->
       action ()
    )
    ~args: [
    ]
    ~doc: "Manage local nodes"
    ~man:[
      `S "DESCRIPTION";
      `P "This command performs operations on nodes running TONOS SE \
          in sandbox networks. It can start and stop nodes, and send \
          tokens to accounts.";
    ]
