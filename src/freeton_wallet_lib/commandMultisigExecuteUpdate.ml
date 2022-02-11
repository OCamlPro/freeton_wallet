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
open Types.MULTISIG

type code =
  | NoCode
  | CodeFromString of string (* base64 *)
  | CodeFromFile of string (* filename *)

let action ~account ~updateId ~code ~src =
  let config = Config.config () in
  let ctxt = Multisig.get_context config account in
  let parameters = Multisig.get_parameters ctxt in
  let custodians = Multisig.get_custodians ctxt in
  let custodians = List.map (fun c -> c.Types.MULTISIG.pubkey) custodians in
  let requiredUpdConfirms =
    match parameters.requiredUpdConfirms with
    | "0" -> Error.raise "Contract is not updatable"
    | s -> int_of_string s
  in

  let src = match src with
      None -> account
    | Some src -> src
  in
  let src_key = Misc.find_key_exn ctxt.net src in

  let keypair =
    match src_key.key_pair with
    | None ->
        Error.raise "No private key associated with signer %S" src
    | Some key_pair ->
        let public = "0x" ^ key_pair.public in
        if not (List.exists (fun c -> c = public) custodians ) then
          Error.raise
            "Key %S is not among old custodians (use 'ft multisig info %s')"
            src account;
        key_pair
  in

  let updates = Multisig.get_updates ctxt in
  let updateId, update =
    match updateId, updates with
    | None, [ up ] ->
        up.update_id, up
    | None, _ ->
        Error.raise "Multiple updates available, you must specify the updateId"
    | _, [] -> Error.raise "No update to execute"
    | Some updateId, updates ->
        let rec iter updates =
          match updates with
          | [] ->
              Error.raise "No update with updateId %s" updateId
          | up :: updates ->
              if up.update_id = updateId then
                updateId, up
              else
                iter updates
        in
        iter updates
  in
  if int_of_string update.update_signs < requiredUpdConfirms then
    Error.raise "Not enough confirmations (%s received < %d required)"
      update.update_signs requiredUpdConfirms ;
  let update_codeHash = Misc.unjson_uin256 update.update_codeHash in
  let contract = Misc.contract_of_code_hash ~code_hash:update_codeHash in
  let contract_code = match contract with
    | Some contract ->
        Printf.eprintf "Extracting code from contract %S\n%!" contract;
        let filename = Misc.get_contract_tvcfile contract in
        let code = Subst.get_code filename in
        let code_hash = Subst.get_code_hash filename in
        if code_hash <> update_codeHash then begin
          Printf.eprintf "Warning: extracted code does not match update code_hash\n%!";
          Printf.eprintf "  Update   codeHash: %s\n%!" update_codeHash;
          Printf.eprintf "  Contract codeHash: %s\n%!" code_hash;
          None
        end
        else
          Some code
    | None ->
        Printf.eprintf "Warning: unknown contract for codeHash %s\n%!"
          update_codeHash ;
        None
  in
  let code = match contract_code with
    | Some code -> code
    | None ->
        match code with
        | NoCode ->
            (* TODO: if we can find a contract onchain with the same codeHash,
               we could download it. *)
            Error.raise "Unknown codehash, you must provide the code"
        | CodeFromString s -> s
        | CodeFromFile filename ->
            Printf.eprintf "Extracting code from filename %S\n%!" filename;
            let code = Subst.get_code filename in
            let code_hash = Subst.get_code_hash filename in
            if code_hash <> update_codeHash then begin
              Printf.eprintf "Warning: extracted code does not match update code_hash\n%!";
              Printf.eprintf "  Update codeHash: %s\n%!" update_codeHash;
              Printf.eprintf "  File   codeHash: %s\n%!" code_hash;
              Error.raise "You must provide the correct code"
            end;
            code
  in
  let meth = "executeUpdate" in
  let params =
    Printf.sprintf
      {|{"updateId": "%s", "code": "%s" }|}
      updateId code
  in
  Multisig.call ctxt meth ~params ~local:false ~keypair
    Json_encoding.unit

let cmd =
  let account = ref None in
  let updateId = ref None in
  let src = ref None in
  let code = ref NoCode in
  Misc.cmd
    ["multisig"; "execute"; "update"]
    (fun () ->
       let account = match !account with
         | None -> Error.raise "You must provide the account"
         | Some account -> account
       in
       action ~account ~updateId:!updateId ~code:!code ~src:!src
    )
    ~args:
      [
        [ "code" ], Arg.String (fun s -> code := CodeFromString s),
        EZCMD.info ~docv:"CODE-BASE64" "New code from base64";

        [ "src" ], Arg.String (fun s -> src := Some s),
        EZCMD.info ~docv:"ACCOUNT" "The signing custodian";

       [ "tvm" ], Arg.String (fun s -> code := CodeFromFile s),
        EZCMD.info ~docv:"TVM-FILENAME" "New code from file";

        [], Arg.Anons (fun list ->
            match list with
            | [ s_account ; s_updateId ] ->
                account := Some s_account ;
                updateId := Some s_updateId ;
            | _ -> Error.raise "You must provide more arguments"
          ),
        EZCMD.info ~docv:"ACCOUNT UPDATE_ID CODE" "The multisig account";
      ]
    ~doc: "Update a multisig wallet with setcode"
    ~man:[
      `S "DESCRIPTION";
      `P "This command can be used to display the pubkeys of the owners/custodians of a multisig wallet";
      `P "To get the list of signers:";
      `Pre {|# ft multisig update MY-ACCOUNT --nreqs 3"|};
    ]
