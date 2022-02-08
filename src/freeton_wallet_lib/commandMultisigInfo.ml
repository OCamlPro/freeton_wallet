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

open EzCompat (* for StringMap *)
open Ezcmd.V2
open EZCMD.TYPES

let get_custodians account =
  let config = Config.config () in
  let ctxt = Multisig.get_context config account in
  let custodians = Multisig.get_custodians ctxt in
  let ncustodians = List.length custodians in

  let info = CommandAccountState.get_address_info config
      ( RawAddress ctxt.multisig_address ) in

  let parameters = Multisig.get_parameters ctxt in
  let name_by_pubkey = Multisig.name_by_pubkey ctxt.net in
  let name_by_pubkey s =
    match StringMap.find s name_by_pubkey with
    | exception Not_found -> s
    | name -> Printf.sprintf "%s (%s)" name s
  in

  let open Ton_sdk.ENCODING in
  let code_hash =
    match info with
    | Some { acc_code_hash = Some code_hash ; _ } ->
        begin
          match Misc.contract_of_code_hash ~code_hash with
          | None -> code_hash
          | Some contract ->
              Printf.sprintf "%s ( %s )" contract code_hash
        end
    | _ -> "unknown"
  in
  let balance =
    match info with
    | Some { acc_balance = Some balance ; _ } ->
        Z.to_int64 balance
    | _ -> 0L
  in

  Printf.printf "Multisig at %s\n%!" ctxt.multisig_address;
  Printf.printf "  Balance: %s\n%!"
    ( Misc.string_of_nanoton balance ) ;
  Printf.printf "  Code hash: %s\n%!" code_hash ;
  Printf.printf "  Expiration time: %s\n%!"
    ( int_of_string parameters.expirationTime |>
      Multisig.string_of_seconds );
  Printf.printf "  Required transaction confirmations: %s/%d\n%!"
    parameters.requiredTxnConfirms ncustodians ;
  if parameters.requiredUpdConfirms <> "0" then
    Printf.printf "  Required update confirmations: %s/%d\n%!"
      parameters.requiredUpdConfirms ncustodians ;
  Printf.printf "     Custodians: %d\n%!" ncustodians;
  List.iter (fun c ->
      Printf.printf "       %s: %s\n%!"
        c.Types.MULTISIG.index (name_by_pubkey c.pubkey)
    ) custodians;
  ()

let action account =

  let account = match account with
    | None ->
        Error.raise "You must provide the account"
    | Some account -> account
  in
  get_custodians account ;
  ()

let cmd =
  let account = ref None in
  Misc.cmd
    ["multisig"; "info"]
    (fun () ->
       action !account
    )
    ~args:
      [
        [], Arg.Anon (0, fun s -> account := Some s),
        EZCMD.info ~docv:"ACCOUNT" "The multisig account";
      ]
    ~doc: "List owners/custodians of a multisig wallet"
    ~man:[
      `S "DESCRIPTION";
      `P "This command can be used to display the pubkeys of the owners/custodians of a multisig wallet";
      `P "To get the list of signers:";
      `Pre {|# ft multisig list custodians MY-ACCOUNT"|};
    ]
