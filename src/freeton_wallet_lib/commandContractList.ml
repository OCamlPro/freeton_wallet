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
(* open EZCMD.TYPES *)

let cmd =
  EZCMD.sub
    "contract list"
    (fun () ->
       CommandContractBuild.list_contracts ()
    )
    ~args:[]
    ~doc: "List known contracts"
    ~man:[]
