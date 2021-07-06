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

let action ~contract () =
  let s = Utils.show_abi ~contract in
  Printf.printf "%s\n%!" s

let cmd =
  let contract = ref None in
  EZCMD.sub
    "contract abi"
    (fun () ->
       match !contract with
       | None -> Error.raise "You must provide the contract name"
       | Some contract ->
             action
               ~contract
               ()
    )
    ~args:
      [

        [], Arg.Anon (0, fun s -> contract := Some s),
        EZCMD.info ~docv:"CONTRACT" "Name of contract to build";

      ]
    ~doc: "Print contract ABI"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command shows a human readable version of contract ABI";
      ];
    ]
