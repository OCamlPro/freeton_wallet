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

open Types

(* A cache of ABIs *)
type t = {
  abis_address2abi : ( string , (* address *)
                       string * (* account name *)
                       ( string * (* contract *)
                         string Lazy.t (* abi content *)
                       ) option
                     ) Hashtbl.t ;
  abis_contract2abi : ( string, string ) Hashtbl.t ;

  (* These functions are added to all contracts ABIs *)
  abis_funs : Ton_client.ABI.AbiContract.fonction list ;
}


let json_of_abi abi =
  EzEncoding.construct
    ~compact:false Ton_client.ABI.AbiContract.enc abi

let get_contract_abi ~abis contract =
  match Hashtbl.find abis.abis_contract2abi contract with
  | v -> v
  | exception Not_found ->
      let abifile = Misc.get_contract_abifile contract in
      let abi = Ton_sdk.ABI.read abifile in
      let abi = { abi with
                  Ton_client.ABI.AbiContract.functions =
                    abi.Ton_client.ABI.AbiContract.functions @ abis.abis_funs } in
      let json = json_of_abi abi in
      Hashtbl.add abis.abis_contract2abi contract json ;
      json

let check_account ?(unknown=ref []) config ~abis ~address =
  if address <> "" then
    match !unknown with
    | [] -> ()
    | s :: tail ->
        match Hashtbl.find abis.abis_address2abi address with
        | exception Not_found ->
            let key_name, contract = EzString.cut_at s ':' in
            let acc_contract, contract =
              if contract = "" then None, None else
                Some contract,
                Some (contract,
                      lazy ( get_contract_abi ~abis contract ))
            in
            Hashtbl.add abis.abis_address2abi address (key_name, contract);
            let net = Config.current_network config in
            unknown := tail ;
            net.net_keys <-
              {
                key_name ;
                key_passphrase = None ;
                key_pair = None ;
                key_account = Some {
                    acc_address = address ;
                    acc_contract ;
                    acc_workchain = None ;
                    acc_static_vars = None ;
                  } ;
              } :: net.net_keys ;
            config.modified <- true
        | _ -> ()

let replace_addr ~abis ~address =
    match Hashtbl.find abis.abis_address2abi address with
    | ( name, contract ) ->
        Printf.sprintf "%s (%s%s)" address name
          ( match contract with
            | None -> ""
            | Some (contract, _ ) -> " " ^ contract)
    | exception Not_found -> address


let create config ~abis =
  let net = Config.current_network config in
  let abis_address2abi = Hashtbl.create 113 in
  let abis_contract2abi = Hashtbl.create 113 in
  let abis_list = ref [] in
  let abis_funs = List.flatten (List.map (fun contract ->
      let abifile = Misc.get_contract_abifile contract in
      let abi = Ton_sdk.ABI.read abifile in
      abis_list := ( contract, abi ) :: !abis_list ;
      abi.Ton_client.ABI.AbiContract.functions
    ) abis) in
  let abis = {
    abis_address2abi ;
    abis_contract2abi ;
    abis_funs
  } in
  List.iter (fun ( contract, abi ) ->
      let abi = { abi with
                  Ton_client.ABI.AbiContract.functions =
                    abi.Ton_client.ABI.AbiContract.functions @ abis_funs } in
      Hashtbl.add abis_contract2abi contract ( json_of_abi abi )
    ) !abis_list ;

  List.iter (fun key ->
      match key.key_account with
      | Some { acc_address ; acc_contract ; _ } ->
          Hashtbl.add abis_address2abi acc_address
            ( key.key_name ,
              match acc_contract with
              | None -> None
              | Some contract ->
                  Some (
                    contract , lazy ( get_contract_abi ~abis contract ) ))
      | _ -> ()
    ) net.net_keys;
  abis

open Ton_sdk

let parse_message_boc ~client ~abis ~boc ~address =
  match
    let (_ , contract ) = Hashtbl.find abis.abis_address2abi address in
    contract
  with
  | exception Not_found -> None
  | None -> None
  | Some ( _ , abi ) ->
      try
        let abi = Lazy.force abi in
        let decoded =
          BLOCK.decode_message_boc ~client ~boc ~abi in
        let body =
          Printf.sprintf "%s %s %s"
            (match decoded.body_type with
             | 0 -> "Call" (* Input *)
             | 1 -> "Reply" (* Output *)
             | 2 -> "InternalOutput"
             | 3 -> "Event"
             | _ -> assert false)
            decoded.body_name
            (match decoded.body_args with
             | None -> ""
             | Some args -> args)
        in
        Some body
      with
      | _ -> None

let parse_message_body ~client ~abis m =
    match m.ENCODING.msg_boc with
    | None -> None
    | Some boc ->
        match m.ENCODING.msg_msg_type_name with
        | None -> assert false
        | Some "ExtOut" ->
            parse_message_boc ~client ~abis ~boc ~address:m.msg_src
        | Some "Internal" ->
          parse_message_boc ~client ~abis ~boc ~address:m.msg_dst
        | Some kind ->
            Printf.eprintf "msg_msg_type_name: %s\n%!" kind;
            parse_message_boc ~client ~abis ~boc ~address:m.msg_dst
