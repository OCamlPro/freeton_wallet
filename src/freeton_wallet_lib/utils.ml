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

let map_of_json json =
  let json = Ezjsonm.from_string json in
  let map = ref StringMap.empty in
  let add path s =
    let key = String.concat ":" ( List.rev path ) in
    map := StringMap.add key s !map
  in
  let rec iter path json =
    match json with
      `O list ->
        add path (Ezjsonm.value_to_string ~minify:true json);
        List.iter (fun (s,v) ->
            iter (s :: path) v
          ) list
    | `A list ->
        add path (Ezjsonm.value_to_string ~minify:true json);
        List.iteri (fun i v ->
            iter (string_of_int i :: path) v
          ) list
    | `Bool b ->
        add path @@ string_of_bool b
    | `Null -> ()
    | `Float f ->
        add path @@ string_of_float f
    | `String s ->
        add path s
  in
  iter [ "res" ] json;
  !map

let call_contract
    config ~address ~contract ~meth ~params
    ?client ?src ?(local=false) ?output ?subst () =
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
         let res =
           match subst with
           | None -> res
           | Some file ->
               let content = EzFile.read_file file in
               let map = map_of_json res in
               Subst.with_subst config (fun subst ->
                   subst content)
                 ~brace:(fun s ->
                     match StringMap.find s map with
                     | exception Not_found -> None
                     | s -> Some s
                   )
         in
         begin
           match output with
           | Some "-" ->
               Printf.eprintf "call returned, saved to stdout\n%!";
               Printf.printf "%s\n%!" res
           | None ->
               Printf.eprintf "call returned:\n%s\n%!" res
           | Some file ->
               Printf.eprintf "call returned, saved to %s\n%!" file;
               EzFile.write_file file res
         end
       else
         let () =
           match output with
           | Some _ ->
               Error.raise "--output FILE cannot be used with FT_USE_TONOS"
           | None -> ()
         in
         let () =
           match subst with
           | Some _ ->
               Error.raise "--subst FILE cannot be used with FT_USE_TONOS"
           | None -> ()
         in
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

let deploy_contract config ~key ~contract ~params ~wc ?client () =
  match key.key_pair with
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

let address_of_account config account =
  let _wc, addr = EzString.cut_at account ':' in
  let len = String.length addr in
  if len <> 0 then
    account ^ String.make (64 - len) '0'
  else
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
