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

open Ez_file.V1
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
  let toolchain = Config.toolchain config in
  let binary = Misc.binary_file ~toolchain "tonos-cli" in
  if not ( Sys.file_exists config_file ) then begin
    let node = Config.current_node config in
    Misc.call (tonoscli binary config ["--url"; node.node_url ; "config" ]);

    let src_file = "tonlabs-cli.conf.json" in
    if Sys.file_exists src_file then begin
      Printf.eprintf "mv %s %s\n%!" src_file config_file ;
      let content = EzFile.read_file src_file in
      Sys.remove src_file ;
      EzFile.write_file config_file content
    end
  end;
  tonoscli binary config args

let string_of_ppf f =

  let b = Buffer.create 1000 in
  let ppf = Format.formatter_of_buffer b in
  f ppf ;
  Format.pp_print_flush ppf () ;
  Buffer.contents b

let printf_params ppf params =
  let open Ton_sdk.TYPES.AbiContract in
  match params with
  | [] -> Format.fprintf ppf "{}"
  | _ ->
      Format.fprintf ppf "'{@[<1>@ ";
      List.iteri (fun i p ->
          if i > 0 then Format.fprintf ppf ",@ ";
          Format.fprintf ppf "%S:@ %S" p.param_name p.param_type
        ) params ;
      Format.fprintf ppf "@ @]}'"

let printf_function ppf f =
  let open Ton_sdk.TYPES.AbiContract in
  Format.fprintf ppf "  * @[<1>%s@ "
    f.fun_name;
  printf_params ppf f.fun_inputs ;
  begin
    match f.fun_outputs with
    | [] -> ()
    | outputs ->
        Format.fprintf ppf "@ ->@ ";
        printf_params ppf outputs;
  end;
  Format.fprintf ppf "@]@."


let show_abi ~contract =
  let contract_abi = Misc.get_contract_abifile contract in
  let abi = Ton_sdk.ABI.read contract_abi in

  string_of_ppf (fun ppf ->
      Format.fprintf ppf "ABI of contract %S@." contract ;
      Format.fprintf ppf "  File: %s@." contract_abi ;

      let open Ton_sdk.TYPES.AbiContract in

      begin
        match abi.header with
        | [] -> () | headers ->
            Format.fprintf ppf "Headers: %s@."
              ( String.concat " " headers );
      end;
      begin
        match abi.data with
        | [] -> ()
        | data ->
            Format.fprintf ppf "\nStatic variables:@.";
            List.iter (fun d ->
                Printf.printf "  %s: %s@." d.data_name d.data_type
              ) data;
      end;

      let constructors, functions = List.partition (fun f ->
          f.fun_name = "constructor"
        ) abi.functions
      in

      let print_functions msg list =
        match list with
        | [] -> ()
        | _ ->
            Format.fprintf ppf "\n%s:\n" msg;
            List.iter (printf_function ppf) list
      in

      print_functions "Constructors" constructors ;
      print_functions "Methods" functions ;

      begin
        match abi.events with
        | [] -> ()
        | events ->
            Format.fprintf ppf "\nEvents:\n";
            List.iter (fun ev ->
                Format.fprintf ppf " * %s " ev.ev_name ;
                printf_params ppf ev.ev_inputs ;
                Format.fprintf ppf "@."
              ) events
      end)

type param_kind =
  | Address
  | Numerical
  | Bytes
  | String
  | Cell
  | Unknown

let check_abi ~contract ~abifile ~meth ~params =
  let open Ton_sdk.TYPES.AbiContract in
  let abi = Ton_sdk.ABI.read abifile in
  let found = ref None in
  let error f s =
    Error.raise "%s\nMethod has type: %s"
      s (string_of_ppf (fun ppf -> printf_function ppf f))
  in
  let check_params f list params =
    found := Some params ;

    if List.length f.fun_inputs <> List.length list then
      error f
        (Printf.sprintf "Method %S has arity %d, but %d arguments provided"
           f.fun_name
           (List.length f.fun_inputs) (List.length list));

    List.iter (fun (name, _v) ->
        if not ( List.exists (fun p ->
            p.param_name = name
          ) f.fun_inputs ) then begin
          error f (Printf.sprintf "Unknown argument %S" name)
        end
      ) list ;

    let map = ref ( StringMap.of_list list ) in

    List.iter (fun p ->
        match StringMap.find p.param_name !map with
        | exception Not_found ->
            error f (
              Printf.sprintf "Expected argument %S was not provided"
                p.param_name
            )
        | `String s ->
            let param_kind = match p.param_type with
              | "address" -> Address
              | "uint"
              | "uint8"
              | "uint16"
              | "uint32"
              | "uint64"
              | "uint128"
              | "uint256"
              | "int"
              | "int8"
              | "int16"
              | "int32"
              | "int64"
              | "int128"
              | "int256" -> Numerical
              | "string" -> String
              | "bytes" -> Bytes
              | "cell" -> Cell
              | _ -> Unknown
            in
            let len = String.length s in
            begin
              match param_kind with
              | Address ->
                  begin try
                      let before, after = EzString.cut_at s ':' in
                      ignore ( int_of_string before ) ;
                      if String.length after <> 64 then
                        failwith "not an address"
                    with _exn ->
                        error f
                          (Printf.sprintf
                             "Param %S should be an address instead of %s"
                             p.param_name s)
                  end
              | Numerical ->
                  begin
                    let is_hexa = ref false in
                    let is_alpha = ref false in
                    for i = 0 to len -1 do
                      match s.[i] with
                      '0'..'9' -> ()
                      | 'a'..'f' | 'A'..'F' -> is_hexa := true
                      | 'x' | 'X' when i = 1 -> is_hexa := true
                      | _ -> is_alpha := true
                    done;
                    if !is_alpha then
                      error f
                        (Printf.sprintf "Param %S of type %S should be numerical instead of %s"
                           p.param_name p.param_type s)
                    else
                    if !is_hexa then
                      if len > 2 && s.[0] = '0' &&
                         ( s.[1] = 'x' || s.[1] = 'X' ) then
                        ()
                      else
                        error f
                          (Printf.sprintf "Param %S of type %S should be numerical instead of %s\nHINT: maybe you forgot 0x in front."
                             p.param_name p.param_type s)
                  end
              | Bytes ->
                  let is_hexa = ref true in
                  for i = 0 to len -1 do
                    match s.[i] with
                    '0'..'9' | 'a'..'f' | 'A'..'F' -> ()
                             | _ -> is_hexa := false
                  done;
                  if not !is_hexa then
                    error f
                      (Printf.sprintf "Param %S of type %S should be in hexadecimal instead of %s\nHINT: you can use %%{hex:string:%s} instead."
                         p.param_name p.param_type s s)
              | String -> ()
              | Cell -> ()
              | Unknown -> ()
            end
        | _ -> ()
      ) f.fun_inputs
  in
  let maybe_single_param f arg =
    match f.fun_inputs with
      [ p ] ->
        let list = [ p.param_name, arg ] in
        check_params f list
          ( Ezjsonm.value_to_string (`O list) )
    | _ -> Error.raise "Invalid JSON params"
  in
  List.iter (fun f ->
      if f.fun_name = meth then begin
        match Ezjsonm.value_from_string params with
        | exception exn ->
            for i = 0 to String.length params - 1 do
              match params.[i] with
              | ' ' | ':' | '"' | '\'' | '{' | '}' ->
                  Error.raise "Invalid JSON param %S (%s)"
                    params (Printexc.to_string exn)
              | _ -> ()
            done;
            maybe_single_param f (`String params)
        | `O list -> check_params f list params
        | `Bool _
        | `Null
        | `A _ ->
            Error.raise "Invalid JSON param kind %S" params
        | ( `String _ | `Float _ ) as p ->
            maybe_single_param f p
      end) abi.functions;
  match !found with
  | Some params -> params
  | None ->
      Error.raise "The ABI of %S does not contain method %S.\nHINT:Use the following command to show the full ABI:\nft contract abi %s\n"
        contract meth contract


let post config req =
  let node = Config.current_node config in
  let url = node.node_url in
  let open Ton_sdk in
  match REQUEST.post_run url req with
  | Ok r -> r
  | Error exn -> raise exn

let track_messages ?accounts ?(event = fun _ -> ()) config queue =
  let last_time = ref ( Unix.gettimeofday () ) in
  let node = Config.current_node config in
  let client = Ton_sdk.CLIENT.create node.node_url in
  let abis = AbiCache.create ?accounts config in

  while not ( Queue.is_empty queue ) do
    let msg_id = Queue.peek queue in
    begin
      match post config
              Ton_sdk.REQUEST.( transactions ~level:3
                                  ~filter:
                                    (aeq "in_msg" (astring msg_id))
                                  [] ) with
      | exception _ ->
          Unix.sleep 3;
      | [ tr ] ->
          ignore ( Queue.take queue );
          last_time := Unix.gettimeofday ();
          Printf.printf "Msg %s:\nIn trans. %s\n%!" msg_id tr.tr_id;
          Printf.printf "  %s %s, delta: %s%s%s\n%!"
            (match tr.tr_aborted with
             | false -> ""
             | true -> "aborted,") tr.tr_status_name
            (Misc.tons_of_z tr.tr_balance_delta )
            (match tr.tr_action with
             | Some { result_code = Some result_code ; _ } ->
                 if result_code <> 0 then
                   Printf.sprintf ", action.result_code = %d%s" result_code
                     (try
                        let msg = List.assoc result_code
                            CommandPrintError.action_phase_errors in
                        " " ^ msg
                      with _ -> "" )
                 else ""
             | _ -> "" )
            (match tr.tr_compute with
             | Some { exit_code = Some exit_code ; _ } ->
                 if exit_code <> 0 then
                   Printf.sprintf ", compute.exit_code = %d" exit_code
                 else ""
             | _ -> "" )

          ;
          List.iter (fun msg_id ->
              match post config
                      Ton_sdk.REQUEST.( messages ~level:3 ~id: msg_id [] ) with
              | exception exn ->
                  Printf.eprintf "Warning: lookup on msg_id %s failed (%s)\n%!" msg_id ( Printexc.to_string exn )
              | [ msg ] ->
                  begin
                    match msg.msg_msg_type_name with
                    | None -> assert false
                    | Some "Internal" ->
                        Printf.printf "   * sent msg %s\n" msg_id ;
                        Printf.printf "       to %s\n%!"
                          (AbiCache.replace_addr
                             ~abis ~address:msg.msg_dst);
                        let body =
                          AbiCache.string_of_message_body
                            ( AbiCache.parse_message_body ~client ~abis msg )
                        in
                        Printf.printf "       value: %s %s\n%!"
                          ( match msg.msg_value with
                            | None -> "--"
                            | Some v ->
                                Misc.tons_of_z v )
                          body;
                        Queue.add msg_id queue;
                    | Some "ExtOut" ->
                        Printf.printf "   * event %s\n" msg_id ;
                        begin
                          match AbiCache.parse_message_body
                                  ~client ~abis msg with
                          | None -> ()
                          | Some body ->
                              Printf.printf "       %s\n%!"
                                ( AbiCache.string_of_message_body ( Some body ));
                              match body with
                              | _, None -> ()
                              | _, Some m ->
                                  event m
                        end
                    | Some _kind -> ()
                  end
              | _ -> assert false

            ) tr.tr_out_msgs
      | _ -> assert false
    end;
    let time = Unix.gettimeofday () in
    if time -. !last_time > 40. then
      failwith "Timeout (40s without message)"
  done

let call_run config ?client ~wait
    ~server_url ~address ~abi ~meth ~params ~local
    ?keypair ?accounts () =
  if Misc.verbose 2 then begin
    Printf.eprintf "Calling %s: %s %s\n%!" address meth params
  end;
  if wait then
    let client =
      match client with
      | Some client -> client
      | None -> Ton_sdk.CLIENT.create server_url
    in
    let msg = Ton_sdk.ACTION.prepare_message
        ~client ~address ~abi ~meth ~params ?keypair () in
    let res = Ton_sdk.ACTION.send_message ~client
        ~abi ~msg:msg.message
    in
    let result = Ton_sdk.ACTION.wait_for_transaction
        ~client ~abi ~msg:msg.message res in
    let queue = Queue.create () in
    Queue.add msg.message_id queue;
    (*    let shards = Hashtbl.create 100 in
          Hashtbl.add shards address res.shard_block_id ; *)
    track_messages ?accounts config queue ;
    if Misc.verbose 2 then begin
      Printf.eprintf "  result: %s\n%!" result
    end;
    result
  else
    let result =
      Ton_sdk.CALL.call ?client ~server_url
        ~address
        ~abi
        ~meth ~params
        ~local
        ?keypair
        ()
    in
    if Misc.verbose 2 then begin
      Printf.eprintf "  result: %s\n%!" result
    end;
    result


let call_contract
    config ~address ~contract ~meth ~params
    ?client ?src ?(local=false) ?subst ?(wait=false) ?accounts  () =
  Misc.with_contract_abi contract
    (fun ~contract_abi ->
       if Globals.use_ton_sdk then
         let node = Config.current_node config in
         let keypair = match src with
           | None -> None
           | Some key -> Some (Misc.get_key_pair_exn key)
         in
         let params = check_abi ~contract ~abifile:contract_abi ~meth ~params
         in
         let abi = EzFile.read_file contract_abi in
         if Misc.verbose 1 then begin
           Printf.eprintf "call: %s\n%!" address;
           Printf.eprintf "method: %s\n%!" meth;
           Printf.eprintf "params: %s\n%!" params;
           begin
             match src with
             | None -> ()
             | Some key ->
                 Printf.eprintf "signed: %s\n%!" key.key_name
           end;
         end;
         let res =
           call_run config ?client ~server_url:node.node_url
             ~address
             ~abi
             ~meth ~params
             ~local
             ?keypair
             ?accounts
             ~wait
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

let deploy_contract config ~key ?sign ~contract ~params
     ~wc ?initial_data ?initial_pubkey ?client () =
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
                   ?initial_data
                   ?initial_pubkey
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
                                     acc_static_vars = initial_data ;
                                   };
           config.modified <- true
        )



let (let>) p f = Lwt.bind p f

let post_lwt config req =
  let node = Config.current_node config in
  let url = node.node_url in
  let open Ton_sdk in
  let> res = REQUEST.post_lwt url req in
  match res with
  | Ok r -> Lwt.return r
  | Error exn -> raise exn

let address_of_account net account =
  match Misc.is_address account with
  | Some address -> RawAddress address
  | None ->
      let key = Misc.find_key_exn net account in
      Account ( Misc.get_key_account_exn key )


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
        | None -> Z.zero
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
