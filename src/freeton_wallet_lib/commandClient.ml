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

(* open Types *)

(* Use %{account}, %{account:addr}, %{account:keyfile}, %{acccount:pubkey}
   %{contract:tvc}, %{contract:abi}
 *)

let action ?(exec=false) ?stdout args =
  let config = Config.config () in
  let (subst, files) = Subst.subst_string config in
  let args = List.map (fun arg ->
      subst arg
    ) args in
  let clean () =
    List.iter Sys.remove !files
  in
  let cmd = if exec then args else Utils.tonoscli config args in
  match
    match stdout with
    | None -> Misc.call cmd
    | Some file ->
        let ( _ : string ) = Misc.call_stdout_file ~file cmd in
        ()
  with
  | exception exn -> clean () ; raise exn
  | v -> v

let cmd =
  let exec = ref false in
  let args = ref [] in
  let stdout = ref None in
  EZCMD.sub
    "client"
    (fun () ->
       action !args ~exec:!exec ?stdout:!stdout
    )
    ~args:
      [ [],
        Arg.Anons (fun list -> args := list),
        EZCMD.info "Arguments to tonos-cli" ;

        [ "exec" ], Arg.Set exec,
        EZCMD.info "Do not call tonos-cli, the command is in the arguments";

        [ "stdout" ], Arg.String (fun s -> stdout := Some s),
        EZCMD.info "FILE Save command stdout to file";

      ]
    ~doc: "Call tonos-cli, use -- to separate arguments"
