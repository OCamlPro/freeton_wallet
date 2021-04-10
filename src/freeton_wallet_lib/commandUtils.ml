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

let of_base64 s =
  let s = Base64.decode_exn s in
  Printf.printf "string = %S\n" s;
  ()

let of_boc s =
  (*  let s = Base64.decode_exn s in *)
  let s = Ton_sdk.ACTION.parse_message s in
  Printf.printf "boc = %S\n" s

let cmd =
   EZCMD.sub
    "utils"
    (fun () -> ())
    ~args:
      [
        [ "of-base64" ], Arg.String of_base64,
        EZCMD.info "STR Translates from base64";

        [ "of-boc" ], Arg.String of_boc,
        EZCMD.info "STR Parse boc in base64 format";
      ]
    ~doc: "Some useful tools"
