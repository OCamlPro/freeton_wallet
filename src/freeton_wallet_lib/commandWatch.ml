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
(* open Ez_subst.V1 *)
(* open EzFile.OP *)
open Ton_sdk
open ENCODING

let z_ton = Z.of_string "1000000000"
let z_0 = Z.of_string "0"

let ton_of_z z =
  let z, sign =
    if z >= z_0 then z, "" else Z.neg z, "-" in
  let tons = Z.div z z_ton in
  let nanotons = Z.sub z ( Z.mul tons z_ton ) in
  let nanotons = Z.to_string nanotons in
  let zeros = String.make (9 - String.length nanotons) '0' in
  Printf.sprintf "%s%s.%s%s" sign (Z.to_string tons)
    zeros nanotons

let if_z s z_opt =
  match z_opt with
  | None -> ()
  | Some z ->
      Printf.printf "    %s: %s\n%!" s ( ton_of_z z )

let display_message ~out msg =
  match msg with
  | { msg_id ;
      msg_msg_type_name = Some msg_type_name ;
      msg_status_name = Some msg_status_name ;
      msg_created_at_string = Some msg_created_at_string ;
      _ } ->
      Printf.printf "  MESSAGE%s %S\n%!"
        (if out then " OUT" else "")
        msg_id;
      Printf.printf "    date: %s\n%!" msg_created_at_string;
      (match msg.msg_bounce with
       | Some true -> Printf.printf "    bounce: true\n%!" | _ -> ());
      (match msg.msg_bounced with
       | Some true -> Printf.printf "    bounced: true\n%!" | _ -> ());
      ( match msg.msg_code_hash with
       | None -> ()
       | Some msg_code_hash ->
           Printf.printf "   code_hash: %s\n%!" msg_code_hash );
      ( match msg.msg_data_hash with
       | None -> ()
       | Some msg_data_hash ->
           Printf.printf "   code_hash: %s\n%!" msg_data_hash );
      Printf.printf "    type name: %s\n%!" msg_type_name;
      Printf.printf "    status name: %s\n%!" msg_status_name;
      if out then
        Printf.printf "    dst: %s\n%!" msg.msg_dst
      else
        Printf.printf "    src: %s\n%!" msg.msg_src;
      if_z "value" msg.msg_value;
      if_z "ihr_fee" msg.msg_ihr_fee;
      if_z "fwd_fee" msg.msg_fwd_fee;
      if_z "import_fee" msg.msg_import_fee;

  | _ ->
      Printf.printf "  MESSAGE%s %s\n%!"
        (if out then " OUT" else "")
        (ENCODING.string_of_message msg)

let arg_of_path path =
  "--" ^ String.concat "-" ( List.rev path )

let args_of_json json =
  match json with
  | None -> []
  | Some json ->
      let json = Ezjsonm.from_string json in
      let rec map path json =
        match json with
          `O list ->
            List.flatten (
              List.map (fun (s,v) ->
                  map (s :: path) v
                ) list
            )
        | `A list ->
            List.flatten (List.mapi (fun i v ->
                map (string_of_int i :: path) v
              ) list)
        | `Bool b ->
            [ arg_of_path path ; string_of_bool b ]
        | `Null -> []
        | `Float f ->
            [ arg_of_path path ; string_of_float f ]
        | `String s ->
            [ arg_of_path path ; s ]
      in
      map [ "arg" ] json

let check_message ~block_id ~tr_id
    config ~abi client ?(out=false) ~level ~on_event msg_id =
  match abi with
  | None -> ()
  | Some abi ->
      match Utils.post config (REQUEST.messages ~level:3 ~id:msg_id []) with
        [ msg ] ->
          begin
            match level with
            | 0 -> ()
            | 3 ->
                Printf.printf "  MESSAGE%s: %s\n%!"
                  (if out then " OUT" else "")
                  ( ENCODING.string_of_message msg )
            | _ ->
                display_message ~out msg
          end ;
          begin (* decode_message only works when there is a msg_body too *)
            match msg.msg_body with
            | None -> ()
            | Some _body ->
                match msg.msg_boc with
                | None -> assert false
                | Some boc ->
                    try
                      let decoded =
                        BLOCK.decode_message_boc ~client ~boc ~abi in
                      if level > 0 then
                        Printf.printf "  CALL: %s %s %s\n%!"
                          (match decoded.body_type with
                           | 0 -> "Input"
                           | 1 -> "Output"
                           | 2 -> "InternalOutput"
                           | 3 -> "Event"
                           | _ -> assert false)
                          decoded.body_name
                          (match decoded.body_args with
                           | None -> ""
                           | Some args -> args) ;

                      begin
                        if decoded.body_type = 3 then
                          match on_event with
                          | None -> ()
                          | Some cmd ->

                              Misc.call
                                ( [ cmd ; block_id ;
                                    decoded.body_name ; tr_id ] @
                                  args_of_json decoded.body_args )
                      end ;

                    with exn ->
                      Printf.eprintf "exn: %s for boc = %S\n%!"
                        (Printexc.to_string exn) boc
          end
      | _ -> assert false

let display_transaction tr =
  match tr with
  | {
    tr_id ;
    tr_aborted ; tr_balance_delta ;
    tr_end_status_name = Some tr_end_status_name ;
    tr_total_fees = tr_total_fees ;
    tr_destroyed = Some tr_destroyed ;
    tr_status_name = Some tr_status_name ;
    tr_tr_type_name = Some tr_type_name ;
    _
  } ->
      Printf.printf "\nTRANSACTION %S\n%!" tr_id ;
      if tr_aborted then Printf.printf "  aborted: true\n%!";
      if tr_destroyed then Printf.printf "  destroyed: true\n%!";
      Printf.printf "  balance delta: %s\n%!"
        (ton_of_z tr_balance_delta);
      Printf.printf "  total fees: %s\n%!"
        (ton_of_z tr_total_fees);
      Printf.printf "  end_status_name: %s\n%!" tr_end_status_name;
      Printf.printf "  status_name: %s\n%!" tr_status_name;
      Printf.printf "  type_name: %s\n%!" tr_type_name;

  | _ -> assert false

let action ~account ?blockid ?timeout ~level ~on_event () =
  match account with
  | None -> assert false
  | Some account ->
      let config = Config.config () in
      let address = Utils.address_of_account config account in
      Printf.eprintf "Watching account %s\n%!" address;
      let abi = Utils.abi_of_account config account in
      let node = Config.current_node config in
      let client = CLIENT.create node.node_url in
      let blockid = match blockid with
        | None ->
            BLOCK.find_last_shard_block ~client ~address
        | Some blockid -> blockid
      in
      Printf.eprintf "initial blockid: %S\n%!" blockid ;
      begin
        match on_event with
        | None -> ()
        | Some cmd ->
            Misc.call [ cmd ; blockid ; "start" ]
      end ;
      let timeout = Option.map (fun t ->
          Int64.of_int ( t * 1000 )) timeout in (* in ms *)
      let ton = Ton_sdk.CLIENT.create node.node_url in
      let rec iter blockid =
        let b = BLOCK.wait_next_block
            ~client ~blockid ~address
            ?timeout () in
        let block_id = b.id in
        Printf.eprintf "new blockid: %S\n%!" b.id;
        if !Globals.verbosity > 1 then
          Printf.eprintf "block = %s\n%!"
            (Ton_sdk.TYPES.string_of_block b) ;
        begin
          match
            Utils.post config
              (REQUEST.transactions
                 ~level:3
                 ~block_id
                 ~account_addr:address [])
          with
          | [] -> ()
          | trs ->
              if level > 0 then
                Printf.eprintf "In block with id: %S\n%!" b.id;
              List.iter (fun tr ->
                  begin match level with
                    | 0 -> ()
                    | 3 ->
                        Printf.eprintf "\nTRANSACTION: %s\n%!"
                          (ENCODING.string_of_transaction tr)
                    | _ ->
                        display_transaction tr
                  end ;
                  let tr_id = tr.tr_id in
                  check_message ~block_id ~tr_id
                    config ~abi ton tr.tr_in_msg ~level
                    ~on_event:None;
                  List.iter (fun id ->
                      check_message ~block_id ~tr_id
                        ~out:true config ~abi ton id ~level
                        ~on_event)
                    tr.tr_out_msgs
                ) trs
        end;
        iter block_id
      in
      iter blockid

let cmd =
  let account = ref None in
  let blockid = ref None in
  let timeout = ref (Some 2_000_000) in (* 25 days ? *)
  let level = ref 1 in
  let on_event = ref None in
  EZCMD.sub
    "watch"
    (fun () ->
       action
         ~account:!account
         ?blockid:!blockid
         ?timeout:!timeout
         ~level:!level
         ~on_event:!on_event
         ()
    )
    ~args:
      [
        [ "0" ], Arg.Unit (fun () -> level := 0),
        EZCMD.info "Verbosity level none";

        [ "3" ], Arg.Unit (fun () -> level := 3),
        EZCMD.info "Verbosity level 3";

        [ "account" ], Arg.String (fun s -> account := Some s),
        EZCMD.info "ACCOUNT Output account of account";

        [ "from" ], Arg.String (fun s -> blockid := Some s),
        EZCMD.info "ID Start with blockid ID";

        [ "timeout" ], Arg.Int (fun s ->
            if s > 2_000_000 then
              Error.raise "--timeout cannot exceed 2_000_000 seconds";
            timeout := Some s),
        EZCMD.info "TIMEOUT Timeout in seconds";

        [ "on-event" ], Arg.String (fun cmd -> on_event := Some cmd),
        EZCMD.info {|CMD Call CMD on event emitted. Called once on startup as `CMD <blockid> start` and after every emitted event as `CMD <blockid> <tr_id> <event_name> <args>`|};

      ]
    ~doc: "Monitor a given account"
