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
open Types

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

let if_z ~oc s z_opt =
  match z_opt with
  | None -> ()
  | Some z ->
      Printf.fprintf oc "    %s: %s\n%!" s ( ton_of_z z )

let display_message ~oc ~out msg =
  match msg with
  | { msg_id ;
      msg_msg_type_name = Some msg_type_name ;
      msg_status_name = msg_status_name ;
      msg_created_at_string = Some msg_created_at_string ;
      _ } ->
      Printf.fprintf oc "  MESSAGE%s %S\n%!"
        (if out then " OUT" else "")
        msg_id;
      Printf.fprintf oc "    date: %s\n%!" msg_created_at_string;
      (match msg.msg_bounce with
       | Some true -> Printf.fprintf oc "    bounce: true\n%!" | _ -> ());
      (match msg.msg_bounced with
       | Some true -> Printf.fprintf oc "    bounced: true\n%!" | _ -> ());
      ( match msg.msg_code_hash with
       | None -> ()
       | Some msg_code_hash ->
           Printf.fprintf oc "   code_hash: %s\n%!" msg_code_hash );
      ( match msg.msg_data_hash with
       | None -> ()
       | Some msg_data_hash ->
           Printf.fprintf oc "   code_hash: %s\n%!" msg_data_hash );
      Printf.fprintf oc "    type name: %s\n%!" msg_type_name;
      (match msg_status_name with
       | Some name -> Printf.fprintf oc "    status name: %s\n%!" name
       | _ -> ());
      if out then
        Printf.fprintf oc "    dst: %s\n%!" msg.msg_dst
      else
        Printf.fprintf oc "    src: %s\n%!" msg.msg_src;
      if_z ~oc "value" msg.msg_value;
      if_z ~oc "ihr_fee" msg.msg_ihr_fee;
      if_z ~oc "fwd_fee" msg.msg_fwd_fee;
      if_z ~oc "import_fee" msg.msg_import_fee;

  | _ ->
      Printf.fprintf oc "  MESSAGE%s %s\n%!"
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

let check_message ~oc ~block_id ~tr_id
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
                Printf.fprintf oc "  MESSAGE%s: %s\n%!"
                  (if out then " OUT" else "")
                  ( ENCODING.string_of_message msg )
            | _ ->
                display_message ~oc ~out msg
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
                        Printf.fprintf oc "  CALL: %s %s %s\n%!"
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
                      Printf.fprintf oc "exn: %s for boc = %S\n%!"
                        (Printexc.to_string exn) boc
          end
      | _ -> assert false

let display_transaction ~oc tr =
  match tr with
  | {
    tr_id ;
    tr_aborted ; tr_balance_delta ;
    tr_end_status_name = Some tr_end_status_name ;
    tr_total_fees = tr_total_fees ;
    tr_destroyed = Some tr_destroyed ;
    tr_status_name = tr_status_name ;
    tr_tr_type_name = Some tr_type_name ;
    _
  } ->
      Printf.fprintf oc "\nTRANSACTION %S\n%!" tr_id ;
      if tr_aborted then Printf.fprintf oc "  aborted: true\n%!";
      if tr_destroyed then Printf.fprintf oc "  destroyed: true\n%!";
      Printf.fprintf oc "  balance delta: %s\n%!"
        (ton_of_z tr_balance_delta);
      Printf.fprintf oc "  total fees: %s\n%!"
        (ton_of_z tr_total_fees);
      Printf.fprintf oc "  end_status_name: %s\n%!" tr_end_status_name;
      Printf.fprintf oc "  status_name: %s\n%!" tr_status_name;
      Printf.fprintf oc "  type_name: %s\n%!" tr_type_name;

  | _ -> assert false

let action ~account ?block_id ?timeout ~level ~on_event ?output () =
  match account with
  | None -> assert false
  | Some account ->
      let oc = match output with
        | None -> stdout
        | Some file -> open_out file
      in
      let config = Config.config () in
      let net = Config.current_network config in
      let address = Utils.address_of_account net account in
      let address = Misc.raw_address address in
      Printf.fprintf oc "Watching account %s\n%!"
        ( ADDRESS.to_string address );
      let abi = Utils.abi_of_account config account in
      let node = Config.current_node config in
      let client = CLIENT.create node.node_url in
      let block_id = match block_id with
        | None ->
            let address = ADDRESS.to_string address in
            BLOCK.find_last_shard_block ~client ~address
        | Some block_id -> block_id
      in
      Printf.fprintf oc "initial block_id: %S\n%!" block_id ;
      begin
        match on_event with
        | None -> ()
        | Some cmd ->
            Misc.call [ cmd ; block_id ; "start" ]
      end ;
      let timeout = Option.map (fun t ->
          Int64.of_int ( t * 1000 )) timeout in (* in ms *)
      let ton = Ton_sdk.CLIENT.create node.node_url in
      let rec iter block_id =
        let b = BLOCK.wait_next_block
            ~client ~block_id ~address:( ADDRESS.to_string address )
            ?timeout () in
        let block_id = b.id in
        Printf.fprintf oc "new block_id: %S\n%!" b.id;
        if !Globals.verbosity > 1 then
          Printf.fprintf oc "block = %s\n%!"
            (Ton_sdk.TYPES.string_of_block b) ;
        begin
          match
            Utils.post config
              (REQUEST.transactions
                 ~level:3
                 ~block_id
                 ~account_addr:( ADDRESS.to_string address ) [])
          with
          | [] -> ()
          | trs ->
              if level > 0 then
                Printf.fprintf oc "In block with id: %S\n%!" b.id;
              List.iter (fun tr ->
                  begin match level with
                    | 0 -> ()
                    | 3 ->
                        Printf.fprintf oc "\nTRANSACTION: %s\n%!"
                          (ENCODING.string_of_transaction tr)
                    | _ ->
                        display_transaction ~oc tr
                  end ;
                  let tr_id = tr.tr_id in
                  check_message ~oc ~block_id ~tr_id
                    config ~abi ton tr.tr_in_msg ~level
                    ~on_event:None;
                  List.iter (fun id ->
                      check_message ~oc ~block_id ~tr_id
                        ~out:true config ~abi ton id ~level
                        ~on_event)
                    tr.tr_out_msgs
                ) trs
        end;
        iter block_id
      in
      iter block_id

let cmd =
  let account = ref None in
  let block_id = ref None in
  let timeout = ref (Some 2_000_000) in (* 25 days ? *)
  let level = ref 1 in
  let on_event = ref None in
  let output = ref None in
  EZCMD.sub
    "watch"
    (fun () ->
       action
         ~account:!account
         ?block_id:!block_id
         ?timeout:!timeout
         ~level:!level
         ~on_event:!on_event
         ?output:!output
         ()
    )
    ~args:
      [
        [ "o" ; "output" ], Arg.String (fun s -> output := Some s),
        EZCMD.info ~docv:"FILE" "Output to FILE";

        [ "0" ], Arg.Unit (fun () -> level := 0),
        EZCMD.info "Verbosity level none";

        [ "3" ], Arg.Unit (fun () -> level := 3),
        EZCMD.info "Verbosity level 3";

        [], Arg.Anon (0, fun s -> account := Some s),
        EZCMD.info ~docv:"ACCOUNT" "Watch account ACCOUNT";

        [ "account" ], Arg.String (fun s -> account := Some s),
        EZCMD.info ~docv:"ACCOUNT" "Watch account ACCOUNT";

        [ "from" ], Arg.String (fun s -> block_id := Some s),
        EZCMD.info ~docv:"BLOCKID" "Start with block identifier BLOCKID";

        [ "timeout" ], Arg.Int (fun s ->
            if s > 2_000_000 then
              Error.raise "--timeout cannot exceed 2_000_000 seconds";
            timeout := Some s),
        EZCMD.info ~docv:"TIMEOUT" "Timeout in seconds (default is 25 days)";

        [ "on-event" ], Arg.String (fun cmd -> on_event := Some cmd),
        EZCMD.info ~docv:"CMD"
          "Call CMD on event emitted. Called once on startup as `CMD \
           <block_id> start` and after every emitted event as `CMD \
           <block_id> <tr_id> <event_name> <args>`";

      ]
    ~doc: "Monitor a given account for new transactions."
    ~man: [
      `S "DESCRIPTION";
      `Blocks [
        `P "Wait for transactions happening on the given \
            ACCOUNT. Transactions are immediately displayed on \
            stdout. If the argument --on-event CMD is provided, a \
            command is called for every event emitted by the contract.";
      ]
    ]
