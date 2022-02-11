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

type code_hash =
    NoCodeHash
  | CodeHash of string
  | CodeHashFromFile of string
  | CodeHashFromContract of string

let action ~account ~custodians ~nreqs ~code_hash ?src ?reset_custodians ~subst () =
  let config = Config.config () in
  let ctxt = Multisig.get_context config account in
  let old_custodians = Multisig.get_custodians ctxt in
  let updates = Multisig.get_updates ctxt in
  let old_custodians = List.map (fun c ->
      Misc.unjson_uin256 c.Types.MULTISIG.pubkey) old_custodians in
  let info = CommandAccountState.get_address_info config
      ( RawAddress ctxt.multisig_address ) in

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
        if not (List.exists (fun c -> c = key_pair.public) old_custodians ) then
          Error.raise
            "Key %S is not among old custodians (use 'ft multisig info %s')"
            src account;

        let public0x = "0x" ^ key_pair.public in
        List.iter (fun up ->
            if up.Types.MULTISIG.update_creator = public0x then
              Printf.eprintf "WARNING: An update by %s already exists (call with fail with error 113 if not expired)\n\n%!" src;
          ) updates ;

        key_pair
  in

  let parameters = Multisig.get_parameters ctxt in
  let requiredUpdConfirms =
    match parameters.requiredUpdConfirms with
    | "0" -> Error.raise "Contract is not updatable"
    | s -> int_of_string s
  in

  let open Ton_sdk.ENCODING in
  let old_code_hash =
    match info with
    | Some { acc_code_hash = Some code_hash ; _ } -> code_hash
    | _ -> "unknown"
  in
  let custodians = match reset_custodians, custodians with
    | None, [] -> old_custodians
    | Some true, custodians ->
        List.map (CommandMultisigCreate.pubkey_of_custodian ctxt.net) custodians
    | Some false, custodians ->
        let set = ref StringSet.empty in
        List.iter (fun c -> set := StringSet.add c !set) old_custodians ;
        let rec iter0 custodians =
          match custodians with
          | [] -> ()
          | "add:" :: custodians -> iter_add custodians
          | "del:" :: custodians -> iter_del custodians
          | _ -> Error.raise "You must use add: or del: to change custodians"
        and iter_add custodians =
          match custodians with
          | [] -> ()
          | "add:" :: custodians -> iter_add custodians
          | "del:" :: custodians -> iter_del custodians
          | c :: custodians ->
              let pubkey = CommandMultisigCreate.pubkey_of_custodian
                  ctxt.net c in
              if StringSet.mem pubkey !set then
                Error.raise "%s is already a custodian" c;
              set := StringSet.add pubkey !set;
              iter_add custodians
        and iter_del custodians =
          match custodians with
          | [] -> ()
          | "add:" :: custodians -> iter_add custodians
          | "del:" :: custodians -> iter_del custodians
          | c :: custodians ->
              let pubkey =  CommandMultisigCreate.pubkey_of_custodian
                  ctxt.net c in
              if not ( StringSet.mem pubkey !set ) then
                Error.raise "%s (%s) is not a former custodian" c pubkey;
              set := StringSet.remove pubkey !set;
              iter_del custodians
        in
        iter0 custodians;
        StringSet.to_list !set
    | None, _ ->
        Error.raise "Changing custodians requires --reset-custodians or --keep-custodians"
  in
  let ncustodians = List.length custodians in
  let old_nreqs = int_of_string parameters.requiredTxnConfirms in
  let nreqs = match nreqs with
    | None -> old_nreqs
    | Some nreqs -> nreqs
  in
  if nreqs < 1 then
    Error.raise "Invalid nul or negative required confirmations";
  if nreqs > ncustodians then
    Error.raise "%d required confirmations is more than %d custodians"
      nreqs ncustodians;
  let code_hash = match code_hash with
    | NoCodeHash -> old_code_hash
    | CodeHash code_hash -> code_hash
    | CodeHashFromContract contract ->
        let filename = Misc.get_contract_tvcfile contract in
        Subst.get_code_hash filename
    | CodeHashFromFile filename -> Subst.get_code_hash filename
  in

  if nreqs = old_nreqs &&
     custodians = old_custodians &&
     code_hash = old_code_hash
  then
    Error.raise "Nothing to update in contract";
(*
function submitUpdate(uint256 codeHash, uint256[] owners, uint8 reqConfirms) public returns (uint64 updateId)
         *)

  let meth = "submitUpdate" in
  let params =
    Printf.sprintf
      {|{"codeHash": "0x%s", "owners": [%s ], "reqConfirms": %d }|}
      code_hash
      ( String.concat ", "
          ( List.map (fun c ->
                Printf.sprintf {| "0x%s"|} c
              ) custodians ))
      nreqs
  in
  Printf.eprintf "submitUpdate %s\n%!" params;
  let r =
    Multisig.call ctxt meth ~params ~local:false Types.MULTISIG.submitUpdate_reply_enc ~keypair ~subst
  in
  Printf.printf "Update %s will need %d extra confirmations\n%!"
    r.updateId (requiredUpdConfirms - 1)

let cmd =
  let args = ref [] in
  let nreqs = ref None in
  let code_hash = ref NoCodeHash in
  let src = ref None in
  let reset_custodians = ref None in
  let subst_args, subst = Subst.make_args () in
  Misc.cmd
    ["multisig"; "update"]
    (fun () ->
       let account, custodians = match !args with
         | [] ->
             Error.raise "You must provide the account"
         | account :: custodians -> account, custodians
       in
       action
         ?src:!src
         ~account ~custodians ~nreqs:!nreqs ~code_hash:!code_hash
         ~subst
         ?reset_custodians:!reset_custodians ()
    )
    ~args:(
      subst_args
        [
          [], Arg.Anons (fun list -> args := list),
          EZCMD.info ~docv:"ACCOUNT CUSTODIANS" "The multisig account and the custodians";

          [ "req" ], Arg.Int (fun i -> nreqs := Some i),
          EZCMD.info ~docv:"REQ"
            "New number of required confirmations for transactions";

          [ "code-hash" ], Arg.String (fun s ->
              match Misc.is_uint256 s with
              | None -> Error.raise "%S is not a code-hash" s
              | Some s -> code_hash := CodeHash s),
          EZCMD.info ~docv:"CODEHASH" "New code hash for contract";

          [ "tvm" ], Arg.String (fun s -> code_hash := CodeHashFromFile s),
          EZCMD.info ~docv:"TVM-FILENAME" "New code from file";

          [ "contract" ], Arg.String (fun s ->
              code_hash := CodeHashFromContract s),
          EZCMD.info ~docv:"CONTRACT" "New code from contract";

          [ "src" ], Arg.String (fun s -> src := Some s),
          EZCMD.info ~docv:"ACCOUNT"
            "The custodian signing the multisig transfer";

          [ "reset-custodians" ],
          Arg.Unit (fun () -> reset_custodians := Some true),
          EZCMD.info "Reset all custodians";

          [ "keep-custodians" ],
          Arg.Unit (fun () -> reset_custodians := Some false),
          EZCMD.info "Keep existing custodians, either adding new ones (with add: C1 C2...) or deleting old ones (with del: C1 C2...) or any combination of those commands";
        ]
    )
    ~doc: "Update a multisig wallet with setcode"
    ~man:[
      `S "DESCRIPTION";
      `P "This command can be used to display the pubkeys of the owners/custodians of a multisig wallet";
      `P "To get the list of signers:";
      `Pre {|# ft multisig update MY-ACCOUNT --nreqs 3"|};
    ]
