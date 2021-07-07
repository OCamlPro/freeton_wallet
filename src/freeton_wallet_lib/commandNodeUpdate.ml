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

let action () =
  let config = Config.config () in
  let node = Config.current_node config in
  match node.node_local with
  | None ->
      Error.raise "cannot manage remote node %S" node.node_name
  | Some _local_node ->
      Misc.call [ "docker"; "pull" ; "tonlabs/local-node" ]

let cmd =
  EZCMD.sub
    "node update"
    (fun () ->
       action ()
    )
    ~args: [
    ]
    ~doc:  "Update Docker image of TONOS SE for new \
            features. You must recreate sandbox switches to \
            benefit from the new image."
    ~man:[
      `S "DESCRIPTION";
      `P "This command performs operations on nodes running TONOS SE \
          in sandbox networks. It can start and stop nodes, and send \
          tokens to accounts.";
    ]
