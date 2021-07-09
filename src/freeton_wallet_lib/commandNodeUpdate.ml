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
(* open EzFile.OP *)

let action ~image =
  let config = Config.config () in
  let node = Config.current_node config in
  match node.node_local with
  | None ->
      Error.raise "cannot manage remote node %S" node.node_name
  | Some _local_node ->
      Misc.call [ "docker"; "pull" ; image ]

let cmd =
  let image = ref "tonlabs/local-node" in
  EZCMD.sub
    "node update"
    (fun () ->
       action ~image:!image
    )
    ~args: [

      [ "image" ], Arg.String ( fun s -> image := s ),
      EZCMD.info ~docv:"DOCKER" "Docker image to use for sandboxes" ;

    ]
    ~doc:  "Update Docker image of TONOS SE for new \
            features. You must recreate sandbox switches to \
            benefit from the new image."
    ~man:[
      `S "DESCRIPTION";
      `P "This command can be used to update the docker image that \
          will be used to create new sandbox networks \
          (tonlabs/local-node, or the one provide with --image)";
    ]
