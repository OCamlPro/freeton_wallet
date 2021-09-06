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

let send_transfer ~account ?src ~dst ~amount ?(bounce=false) ?(args=[])
    ?(wait=false) ?(send=false) () =
  let config = Config.config () in
  let net = Config.current_network config in

  let src = match src with
      None -> account
    | Some src -> src
  in
  let src_key = Misc.find_key_exn net src in

  let account_addr, account_contract =
    match Misc.is_address account with
    | Some address -> address, "SafeMultisigWallet"
    | None ->
        let account_key = Misc.find_key_exn net account in
        let account_addr = Misc.get_key_address_exn account_key in
        let account_contract =
          CommandMultisigCreate.check_key_contract account_key in
        ( account_addr, account_contract )
  in
  let net = Config.current_network config in
  let dst_addr = Utils.address_of_account net dst in
  let dst_addr = Misc.raw_address dst_addr in

  let nanotokens, allBalance =
    if amount = "all" then
      2_000_000L, (* MIN_VALUE is 1e06 in SetcodeMultisigWallet2 *)
      true
    else
      Misc.nanotokens_of_string amount, false
  in

  begin match Utils.get_account_info config account_addr with
    | Some (account_exists, account_balance) ->
        if not account_exists then
          Error.raise "Account %s does not exist yet." account ;
        if ( not allBalance ) && Z.of_int64 nanotokens >= account_balance then
          Error.raise
            "Balance %s nanotons of account %s is smaller than transferred amount %s"
            (Z.to_string account_balance) account amount
    | None ->
        Error.raise "Account %s does not exist yet." account
  end;

  let dst_exists = match Utils.get_account_info config dst_addr with
      Some (dst_exists, _ ) -> dst_exists
    | None -> false
  in
  if bounce && not dst_exists then
    Error.raise "Destination does not exist. Use --sponsor option";

  let args = match args with
    | [ meth ; params ] -> Some ( meth, params )
    | [ meth ] -> Some ( meth, "{}" )
    | [] -> None
    | _ ->
        Error.raise "Too many params arguments"
  in
  let payload = match args with
    | Some ( meth , params ) ->
        let dst_key = Misc.find_key_exn net dst in
        let dst_contract = Misc.get_key_contract_exn dst_key in
        let abi_file = Misc.get_contract_abifile dst_contract in
        let abi = EzFile.read_file abi_file in
        Ton_sdk.ABI.encode_body ~abi ~meth ~params
    | None -> ""
  in

  let meth, params =
    if send || allBalance then
      let meth = "sendTransaction" in
      let params = Printf.sprintf
          {|{"dest":"%s","value":%s,"bounce":%b,"flags":%d,"payload":"%s"}|}
          dst_addr
          (if allBalance then "0" else Int64.to_string nanotokens)
          bounce
          (if allBalance then 128 else 0)
          payload
      in
      Printf.eprintf "Warning: 'all' balance only works with one-custodian multisigs\n%!";
      meth, params
    else
      let meth = "submitTransaction" in
      let params = Printf.sprintf
          {|{"dest":"%s","value":%Ld,"bounce":%b,"allBalance":%b,"payload":"%s"}|}
          dst_addr
          nanotokens
          bounce
          allBalance
          payload
      in
      meth, params
  in
  Utils.call_contract config ~contract:account_contract
    ~address:account_addr
    ~meth ~params
    ~local:false
    ~src:src_key
    ~wait
    ()

let action account args ~amount ~dst ~bounce ~src ~wait ~send =

  let config = Config.config () in

  let account = match account with
    | None ->
        Error.raise "The argument --from ACCOUNT is mandatory"
    | Some account -> account
  in

  Subst.with_subst ~config (fun subst ->
      let account = subst account in
      let args = List.map subst args in
      match dst with
      | Some dst ->
          let dst = subst dst in
          let amount = subst amount in
          let src = Option.map subst src in
          send_transfer ~account ?src ~dst ~bounce ~amount ~args ~wait ~send ()
      | _ ->
          Error.raise "The argument --to ACCOUNT is mandatory"
    )

let cmd =
  let account = ref None in
  let args = ref [] in
  let dst = ref None in
  let bounce = ref true in
  let src = ref None in
  let wait = ref false in
  let send = ref false in
  EZCMD.sub
    "multisig transfer"
    (fun () ->
       match !args with
       | [] -> Error.raise "You must provide the amount to transfer"
       | amount :: args ->
           action !account
             args
             ~amount
             ~dst:!dst
             ~bounce:!bounce
             ~src:!src
             ~wait:!wait
             ~send:!send
    )
    ~args:
      [
        [], Arg.Anons (fun list -> args := list),
        EZCMD.info "Generic arguments" ;

        [ "from" ], Arg.String (fun s -> account := Some s),
        EZCMD.info ~docv:"ACCOUNT" "The source of the transfer";

        [ "src" ], Arg.String (fun s -> src := Some s),
        EZCMD.info ~docv:"ACCOUNT"
          "The custodian signing the multisig transfer";

        [ "wait" ], Arg.Set wait,
        EZCMD.info "Wait for all transactions to finish";

        [ "send" ], Arg.Set send,
        EZCMD.info "Force sendTransaction() instead of submitTransaction()";

        [ "parrain"; "sponsor" ], Arg.Clear bounce,
        EZCMD.info " Transfer to inactive account";

        [ "bounce" ], Arg.Bool (fun b -> bounce := b),
        EZCMD.info "BOOL Transfer to inactive account";

        [ "to" ], Arg.String (fun s -> dst := Some s),
        EZCMD.info ~docv:"ACCOUNT" "Target of a transfer";

      ]
    ~doc: "Transfer TONs from a multisig wallet to another account"
    ~man:[
      `S "DESCRIPTION";
      `P "This command is used to send tokens from a multisig wallet \
          to another account (or to submit a transaction if multiple \
          confirmations are required).";

      `S "SIMPLE TRANSFER";
      `P "Should be like that:";
      `Pre {|# ft multisig transfer 100.000 -from MY-ACCOUNT --to OTHER-ACCOUNT|};
      `P "If the target is not an active account:";
      `Pre {|# ft multisig transfer 100.000 --from MY-ACCOUNT --to OTHER-ACCOUNT --sponsor|};
      `P "To send all the balance:";
      `Pre {|# ft multisig transfer all --from MY-ACCOUNT --to OTHER-ACCOUNT|};

      `S "CALL WITH PARAMS";
      `P "Should be like that:";
      `Pre {|# ft multisig transfer 100 --from MY-ACCOUNT --to CONTRACT set '{ "x": "100" }|};
    ]
