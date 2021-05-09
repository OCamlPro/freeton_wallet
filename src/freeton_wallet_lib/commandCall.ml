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


let action ~account ~meth ~params ~local ~sign ~subst =

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
        | Some sign -> Some (Misc.find_key_exn net sign )
        | None ->
            match key.key_pair with
            | None -> None
            | Some _ -> Some key
      in
      Subst.with_substituted config params (fun params ->
          Utils.call_contract config
            ~address ~contract ~meth ~params ~local ?src ~subst () )

let cmd =
  let args = ref [] in
  let local = ref false in
  let sign = ref None in
  let subst_args, subst = Subst.make_args () in
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
         ~subst
    )
    ~args: (
      subst_args
        [
          [], Arg.Anons (fun l -> args := l),
          EZCMD.info ~docv:"ACCOUNT METH [JSON_PARAMS]" "arguments" ;

          [ "run"; "local" ], Arg.Set local, EZCMD.info "Run locally";

          [ "sign"], Arg.String (fun s -> sign := Some s),
          EZCMD.info ~docv:"ACCOUNT" "Sign message with account";
        ] )
    ~doc: "Call contracts"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "Call a method of a deployed contract. Use --local or --run \
            to run the contract locally (only for get methods). If the \
            params are not specified, {} is used instead. The message \
            is signed if the --sign SIGNER argument is provided, or if \
            the secret key of the account is known.";
        `P "Examples:";
        `Pre {|$ ft call giver sendGrams
        '{ "dest":"%{account:address:user1}", "amount":"1000000000000"}'|};
        `Pre {|$ ft --switch mainnet call msig confirmUpdate
        '{  "updateId": "0x6092b3ee656aaa81" }' --sign mywallet|};
      ];

    ]
