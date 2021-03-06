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
  | Some local_node ->
      Misc.call [ "xdg-open";
                  Printf.sprintf "http://0.0.0.0:%d/graphql"
                    local_node.local_port ]


let cmd =
  EZCMD.sub
    "node web"
    (fun () ->
       action ()
    )
    ~args: [
    ]
    ~doc:  "Open Node GraphQL webpage"
    ~man:[
      `S "DESCRIPTION";
      `P "This command can be used to open the GraphQL webpage \
          associated with the current local network"
    ]
