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


let action ~account ~meth ~params ~local ~sign ~output =

  let config = Config.config () in
  let net = Config.current_network config in
  let key = Misc.find_key_exn net account in
  let account = Misc.get_key_account_exn key in
  let address = account.acc_address in
  let contract = account.acc_contract in
  match contract with
  | None -> Error.raise "You should set account's contract first"
  | Some contract ->
      let src = match sign with
        | Some sign -> Misc.find_key_exn net sign
        | None -> key
      in
      CommandOutput.with_substituted config params (fun params ->
          Utils.call_contract config
            ~address ~contract ~meth ~params ~local ~src ?output () )

let cmd =
  let args = ref [] in
  let local = ref false in
  let output = ref None in
  let sign = ref None in
   EZCMD.sub
    "call"
    (fun () ->
       let account, meth, params =
         match !args with
         | [ account ; meth ; params ] ->
             ( account, meth, params )
         | [ account ; meth ] ->
             ( account, meth, "{}" )
         | _ ->
             Error.raise "Requires at least ACCOUNT METHOD [PARAMS]"
       in
       action ~account ~meth ~params
         ~local:!local
         ~sign:!sign
         ~output:!output
    )
    ~args:
      [

        [], Arg.Anons (fun l -> args := l),
        EZCMD.info "ACCOUNT METH [PARAMS] arguments" ;

        [ "run" ], Arg.Set local,
        EZCMD.info "Run locally";

        [ "sign"], Arg.String (fun s -> sign := Some s),
        EZCMD.info "ACCOUNT Sign message with account";

        [ "o" ; "output"], Arg.String (fun s -> output := Some s),
        EZCMD.info "FILE Save result to FILE (use - for stdout)";
      ]
    ~doc: "Manage contracts"
