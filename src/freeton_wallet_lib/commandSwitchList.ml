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
    ~doc: "List the current switches/networks"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command is used to list the known networks (switches) and the current configuration."
      ];
    ]
