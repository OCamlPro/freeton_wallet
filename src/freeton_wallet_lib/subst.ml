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

(* open Types *)
open Ez_subst.V1

let rec date_now now rem =
  match rem with
  | [] -> now
  | "plus" :: rem ->
      date_now_delay now (fun now delay -> now + delay) rem
  | "minus" :: rem ->
      date_now_delay now (fun now delay -> now - delay) rem
  | _ ->
      Error.raise "Bad substitution 'now:%s'"
        (String.concat ":" rem)

and date_now_delay now f rem =
  let delay, rem =
  match rem with
  | num :: ( "second" | "sec" | "seconds" | "secs" ) :: rem ->
      int_of_string num * 60 , rem
  | num :: ( "minute" | "min" | "minutes" | "mins" ) :: rem ->
      int_of_string num * 60 , rem
  | num :: ( "hour" | "hours" ) :: rem ->
      int_of_string num * 3600, rem
  | num :: ( "day" | "days" ) :: rem ->
      int_of_string num * 86400, rem
  | num :: ( "year" | "years" ) :: rem ->
      int_of_string num * 365 * 86400, rem
  | _ ->
      Error.raise "Bad delay substitution '%s'"
        (String.concat ":" rem)
  in
  date_now (f now delay) rem

let get_code filename =
  let tvm_linker = Misc.binary_file "tvm_linker" in
  let lines =
    Misc.call_stdout_lines [ tvm_linker ; "decode" ; "--tvc" ; filename ] in
  let code = ref None in
  List.iter (fun line ->
      match EzString.split line ' ' with
      | [ "" ; "code:" ; base64 ] -> code := Some base64
      | _ -> ()
    ) lines;
  match !code with
  | None -> Error.raise "Code not found in file %S" filename
  | Some code -> code


let subst_string ?brace config =
  let net = Config.current_network config in
  let files = ref [] in

  let rec iter = function
    | [ "env" ; var ] -> begin
        match Sys.getenv var with
        | exception Not_found ->
            Error.raise "Env variable %S is not defined" var
        | s -> s
      end

    (* Accounts substitutions: *)
    | [ "addr" ; "zero" ] ->
        "0:0000000000000000000000000000000000000000000000000000000000000000"

    (* Account substitutions *)
    | [ "account" ; "addr" ; account ]
    | [ "account" ; "address" ; account ]
      ->
        let key = Misc.find_key_exn net account in
        Misc.get_key_address_exn key
    | [ "account" ; "wc" ; account ] ->
        let key = Misc.find_key_exn net account in
        let acc = Misc.get_key_account_exn key in
        Misc.string_of_workchain acc.acc_workchain
    | [ "account" ; "pubkey" ; account ] ->
        let key = Misc.find_key_exn net account in
        let key_pair = Misc.get_key_pair_exn key in
        key_pair.public
    | [ "account" ; "seckey" ; account ] ->
        let key = Misc.find_key_exn net account in
        let key_pair = Misc.get_key_pair_exn key in
        begin
          match key_pair.secret with
          | None -> Error.raise "No private key for %s" account
          | Some seckey -> seckey
        end
    | [ "account" ; "passphrase" ; account ] ->
        let key = Misc.find_key_exn net account in
        Misc.get_key_passphrase_exn key
    | [ "account" ; "keyfile" ; account ] ->
        let key = Misc.find_key_exn net account in
        let key_pair = Misc.get_key_pair_exn key in
        let file = Misc.gen_keyfile key_pair in
        files := file :: !files;
        file
    | [ "account" ; "contract" ; account ] ->
        let key = Misc.find_key_exn net account in
        let contract = Misc.get_key_contract_exn key in
        contract
    | [ "account" ; "contract" ; "tvc" ; account ] ->
        let key = Misc.find_key_exn net account in
        let contract = Misc.get_key_contract_exn key in
        Misc.get_contract_tvcfile contract
    | [ "account" ; "contract" ; "abi" ; account ] ->
        let key = Misc.find_key_exn net account in
        let contract = Misc.get_key_contract_exn key in
        Misc.get_contract_abifile contract
    | "account" :: "payload" :: account :: meth :: rem ->
        let key = Misc.find_key_exn net account in
        let contract = Misc.get_key_contract_exn key in
        let params = match rem with
          | [] -> "{}"
          | _ -> String.concat ":" rem in
        let abi_file = Misc.get_contract_abifile contract in
        let abi = EzFile.read_file abi_file in
        Ton_sdk.ABI.encode_body ~abi ~meth ~params
    (* Contracts substitutions *)
    | [ "contract" ; "tvc" ; contract ] ->
        Misc.get_contract_tvcfile contract
    | [ "contract" ; "abi" ; contract ] ->
        Misc.get_contract_abifile contract
    | "contract" :: "payload" :: contract :: meth :: rem ->
        let params = match rem with
          | [] -> "{}"
          | _ -> String.concat ":" rem in
        let abi_file = Misc.get_contract_abifile contract in
        let abi = EzFile.read_file abi_file in
        Base64.encode_string (
          Ton_sdk.ABI.encode_body ~abi ~meth ~params
        )

    (* Node substitutions *)
    | [ "node" ; "url" ] ->
        let node = Config.current_node config in
        node.node_url

    | [ "ton" ; n ] ->
        Int64.to_string ( Misc.nanotokens_of_string n )
    | [ "file" ; file ] ->
        String.trim ( EzFile.read_file file )

    | "string" :: rem -> String.concat ":" rem

    | "now" :: rem ->
        string_of_int (
          date_now (int_of_float (Unix.gettimeofday ())) rem )


    (* encoders *)
    | "read" :: rem -> EzFile.read_file ( iter rem )
    | "hex" :: rem ->
        let `Hex s = Hex.of_string ( iter rem ) in s
    | "base64" :: rem ->
        Base64.encode_string ( iter rem )
    | "get-code" :: rem ->
        get_code ( iter rem )


    (* deprecated *)
    | [ account ; "addr" ]
    | [ "addr" ; account ] ->
        let key = Misc.find_key_exn net account in
        Misc.get_key_address_exn key
    | [ account ; "wc" ]
    | [ "wc" ; account ] ->
        let key = Misc.find_key_exn net account in
        let acc = Misc.get_key_account_exn key in
        Misc.string_of_workchain acc.acc_workchain
    | [ account ; "pubkey" ]
    | [ "pubkey" ; account ]->
        let key = Misc.find_key_exn net account in
        let key_pair = Misc.get_key_pair_exn key in
        key_pair.public
    | [ account ; "passphrase" ]
    | [ "passphrase" ; account ] ->
        let key = Misc.find_key_exn net account in
        Misc.get_key_passphrase_exn key
    | [ account ; "keyfile" ]
    | [ "keyfile" ; account ] ->
        let key = Misc.find_key_exn net account in
        let key_pair = Misc.get_key_pair_exn key in
        let file = Misc.gen_keyfile key_pair in
        files := file :: !files;
        file
    | [ account ; "contract"; "tvc" ]
    | [ "account-tvc" ; account ] ->
        let key = Misc.find_key_exn net account in
        let contract = Misc.get_key_contract_exn key in
        Misc.get_contract_tvcfile contract
    | [ account ; "contract" ; "abi" ]
    | [ "account-abi" ; account ] ->
        let key = Misc.find_key_exn net account in
        let contract = Misc.get_key_contract_exn key in
        Misc.get_contract_abifile contract

    (* Contracts substitutions *)
    | [ contract ; "tvc" ]
    | [ "tvc" ; contract ]->
        Misc.get_contract_tvcfile contract
    | [ contract ; "abi" ]
    | [ "abi" ; contract ] ->
        Misc.get_contract_abifile contract

    | [ n ; "ton" ] -> Int64.to_string ( Misc.nanotokens_of_string n )


    | _ -> raise Not_found
  in
  let brace () s =
    try
      match brace with
      | None ->
          iter ( EzString.split s ':' )
      | Some brace ->
          match brace s with
          | Some res -> res
          | None ->
              iter ( EzString.split s ':' )
    with exn ->
      Error.raise "Cannot substitute %S: exception %s"
        s ( Printexc.to_string exn)
  in
  (fun s -> EZ_SUBST.string ~sep:'%' ~brace ~ctxt:() s), files


let help =
{|
Substitutions are written as %{SUBST}, and can be recursive (substitutions
are allowed within SUBST itself).
Here is a list of allowed expressions within SUBST:

* env:VARIABLE    Environemnt variable
* addr:zero     For 0:0000000000000000000000000000000000000000000000000000000000000000"

On wallet accounts:
* account:address:ACCOUNT
* account:wc:ACCOUNT
* account:pubkey:ACCOUNT      Pubkey of account (without 0x)
* account:passphrase:ACCOUNT
* account:keyfile:ACCOUNT     Name of a keyfile generated in $HOME/.ft/tmp/
* account:contract:ACCOUNT    Name of recorded contract of account in wallet
* account:contract:tvc:ACCOUNT    Contract tvc file for account
* account:contract:abi:ACCOUNT    Contract abi file for account
* account:payload:ACCOUNT:METH:PARAMS Output payload base64

On contracts:
* contract:tvc:CONTRACT
* contract:abi:CONTRACT
* contract:payload:CONTRACT:METH:PARAMS Output payload base64

Misc:
* node:url         Current node URL
* ton:NUMBER       Convert NUMBER of tons to nanotons
* file:FILENAME    Read content of filename
* string:SUBST     Take remaining SUBST without substituting, just as a string
* now              Current date
* now:PLUS         Current date plus some delay ( "plus:NDAYS:days" )

Encoders, working on the rest of the substitution:
* read:SUBST       Do SUBST, then read it as a filename
* hex:SUBST        Do SUBST, then convert to hex
* base64:SUBST     Do SUBST, then convert to base64
* get-code:SUBST   Do SUBST to generate a TVC filename, extract code from it

|}


let with_subst ?brace config f =
  let (subst, files) = subst_string ?brace config in
  let clean () = List.iter Sys.remove !files in
  match
    f subst
  with
  | res -> clean (); res
  | exception exn -> clean () ; raise exn

let with_substituted ?brace config params f =
  with_subst ?brace config (fun subst ->
      let params = subst params in
      f params)

let with_substituted_list ?brace config args f =
  with_subst ?brace config (fun subst ->
      let args = List.map subst args in
      f args
    )
