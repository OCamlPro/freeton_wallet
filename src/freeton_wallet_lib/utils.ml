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

(* These functions are 'misc' functions, except that they depend on
   the 'Config' module, so they cannot be in 'Misc'. *)

open EzCompat
open EzFile.OP
open Types

let net_dir config =
  let net = Config.current_network config in
  Globals.ft_dir // net.net_name
let tonoscli_config config = net_dir config // "tonos-cli.config"

let tonoscli binary config args =
    ( binary ::
      "--config" ::
      tonoscli_config config ::
      args )

let tonoscli config args =
  let config_file = tonoscli_config config in
  let binary = Misc.binary_file "tonos-cli" in
  if not ( Sys.file_exists config_file ) then begin
    let node = Config.current_node config in
    Misc.call (tonoscli binary config ["config" ; "--url"; node.node_url ]);

    let src_file = "tonlabs-cli.conf.json" in
    if Sys.file_exists src_file then begin
      Printf.eprintf "mv %s %s\n%!" src_file config_file ;
      let content = EzFile.read_file src_file in
      Sys.remove src_file ;
      EzFile.write_file config_file content
    end
  end;
  tonoscli binary config args

let call_contract
    config ~address ~contract ~meth ~params
    ?client ?src ?(local=false) ?subst () =
  Misc.with_contract contract
    (fun ~contract_tvc:_ ~contract_abi ->
       if Globals.use_ton_sdk then
         let node = Config.current_node config in
         let keypair = match src with
           | None -> None
           | Some key -> Some (Misc.get_key_pair_exn key)
         in
         let abi = EzFile.read_file contract_abi in
         Printf.eprintf "params: %s\n%!" params;
         let res =
           Ton_sdk.ACTION.call_run ?client ~server_url:node.node_url
             ~address
             ~abi
             ~meth ~params
             ~local
             ?keypair
             ()
         in
         match subst with
         | Some subst -> subst ~msg:"call result" config res
         | None ->
             Printf.printf "call result:\n%s\n%!" res
       else
         let command = if local then "run" else "call" in
         let args =
           [
             command ; address ;
             meth ; params ;
             "--abi" ; contract_abi ;
           ]
         in
         match src with
         | None ->
             Misc.call @@ tonoscli config args
         | Some key ->
             Misc.with_key_keypair key
               (fun ~keypair_file ->
                  Misc.call @@ tonoscli config @@
                  args @ [
                    "--sign" ; keypair_file
                  ]
               )
    )

let deploy_contract config ~key ?sign ~contract ~params ~wc ?client () =
  let sign = match sign with
    | None -> key
    | Some sign -> sign
  in
  match sign.key_pair with
  | None -> Error.raise "Key has no secret key"
  | Some keypair ->
      Misc.with_contract contract
        (fun ~contract_tvc ~contract_abi ->

           let acc_address =
             if Globals.use_ton_sdk then
               let contract_abi = EzFile.read_file contract_abi in
               let node = Config.current_node config in
               Printf.eprintf "node url: %s\n%!" node.node_url;
               let client = match client with
                 | None -> Ton_sdk.CLIENT.create node.node_url
                 | Some client -> client
               in
               let addr = Ton_sdk.ACTION.deploy
                   ~client
                   ~tvc_file: contract_tvc
                   ~abi: contract_abi
                   ~params
                   ~keypair
                   ()
               in
               Printf.eprintf "Contract deployed at %s\n%!" addr;
               addr
             else
               let lines =
                 Misc.with_key_keypair key
                   (fun ~keypair_file ->
                      Misc.call_stdout_lines
                      @@ tonoscli config
                        [ "deploy" ; contract_tvc ;
                          params ;
                          "--abi" ; contract_abi ;
                          "--sign" ; keypair_file ;
                          "--wc" ; Misc.string_of_workchain wc
                        ]
                   )
               in
               Printf.eprintf "output:\n %s\n%!"
                 (String.concat "\n" lines);
               Misc.find_line_exn (function
                   | [ "Contract" ; "deployed" ; "at" ; "address:"; address ] -> Some address
                   | _ -> None) lines
           in
           key.key_account <- Some { acc_address ;
                                     acc_contract = Some contract ;
                                     acc_workchain = wc ;
                                   };
           config.modified <- true
        )


let post config req =
  let node = Config.current_node config in
  let url = node.node_url in
  let open Ton_sdk in
  match REQUEST.post_run url req with
  | Ok r -> r
  | Error exn -> raise exn

let is_address account =
  let _wc, addr = EzString.cut_at account ':' in
  let len = String.length addr in
  if len <> 0 then
    Some ( account ^ String.make (64 - len) '0' )
  else None

let address_of_account config account =
  match is_address account with
  | Some address -> address
  | None ->
      let net = Config.current_network config in
      let key = Misc.find_key_exn net account in
      Misc.get_key_address_exn key


let abi_of_account config account =
  if String.contains account ':' then
    None
  else
    let net = Config.current_network config in
    let key = Misc.find_key_exn net account in
    let contract = Misc.get_key_contract_exn key in
    let filename = Misc.get_contract_abifile contract in
    Some ( EzFile.read_file filename )

let get_account_info config address =

  let result = post config
      (Ton_sdk.REQUEST.account ~level:1 address ) in
  match result with
  | [ acc ] ->
      let balance =
        match acc.acc_balance with
        | None -> assert false
        | Some z -> z
      in
      let exists =
        match acc.acc_type_name with
        | Some "Uninit" (* 0 *) -> false
        | _ -> true
      in
      Some ( exists, balance )
  | [] -> None
  | _ -> assert false
