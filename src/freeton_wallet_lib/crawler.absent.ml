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

type action =
  | Crawler
  | Start
  | Status
  | Stop

let action ~dropdb:_ ~account:_ ~action:_ =
  Printf.eprintf
    "This sub-command is currently not built-in. You must install \
     ez_pgocaml and pgocaml_ppx.\n%!";
  exit 2
