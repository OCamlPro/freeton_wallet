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

let cmd =
  EZCMD.sub
    "crawler"
    (fun () ->
       Printf.eprintf
         "Unavailable. You must install ez_pgocaml and recompile.\n%!";
       exit 2
    )
    ~doc: "Unavailable. You must install ez_pgocaml and recompile"
