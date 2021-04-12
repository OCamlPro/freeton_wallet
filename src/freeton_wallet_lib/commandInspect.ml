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
open Ton_sdk (* REQUEST, ENCODING *)

let query_message config ~level ?limit msg_id =
  Utils.post config (
    let len = String.length msg_id in
    if len = 64 then
      REQUEST.messages ~level ~id:msg_id []
    else
    match msg_id with
      | "all" -> REQUEST.messages ~level []
      | _ ->
          let field, address =
            if len>0 && msg_id.[0] = '^' then
              "src", String.sub msg_id 1 (len-1)
            else
              "dst", msg_id
          in
          let address = Utils.address_of_account config address in
          let filter =
            REQUEST.(aeq field (astring address))
          in
          let order =
            ( "created_at" , None )
          in
          REQUEST.messages ~level ?limit ~order ~filter [])

let query_messages config ~level ids =
  List.flatten ( List.map  ( query_message ~level config ) ids )

let inspect_transaction ~level ?limit tr_id =
  let config = Config.config () in

  let trs =
    Utils.post config
      (match tr_id with
       | "all" -> REQUEST.transactions ~level ?limit []
       | _ -> REQUEST.transaction ~level tr_id
      )
  in
  List.iter (fun tr ->

      Printf.printf "\nTRANSACTION: %s\n%!"
        (ENCODING.string_of_transaction tr);

      let msgs = query_messages ~level:1 config [ tr.tr_in_msg ] in
      List.iter (fun tr ->
          Printf.printf "\n  IN MESSAGE: %s\n%!"
            (ENCODING.string_of_message tr);
        ) msgs ;

      let msgs = query_messages ~level:1 config tr.tr_out_msgs in
      List.iter (fun tr ->
          Printf.printf "\n  OUT MESSAGE: %s\n%!"
            (ENCODING.string_of_message tr);
        ) msgs ;

    ) trs

let inspect_account ~level ?limit account =
  let config = Config.config () in
  let request = match account with
    | "all" ->
        REQUEST.accounts ~level ?limit []
    | _ ->
        let address = Utils.address_of_account config account in
        REQUEST.account ~level address
  in
  let accounts =
    Utils.post config request
  in
  List.iter (fun account ->
      Printf.printf "\nACCOUNT: %s\n%!"
        (ENCODING.string_of_account account) ) accounts


let inspect_message ~level ?limit id =
  let config = Config.config () in
  let messages = query_message config ~level ?limit id in
  List.iter (fun msg ->
      Printf.printf "MESSAGE:\n %s\n%!"
        (ENCODING.string_of_message msg) ) messages

type shard =
  | Shard of string
  | Blockid of string
  | Account of string

let split_workchain s =
  match EzString.cut_at s ':' with
  | shard, "" -> 0, shard
  | shard, s -> int_of_string s, shard

let filter_of_shard config shard =
  match shard with
  | None -> None
  | Some shard ->
      let workchain_id, shard =
        match shard with
        | Shard id -> split_workchain id
        | _ ->
            let blockid =
              match shard with
              | Shard _ -> assert false
              | Blockid blockid -> blockid
              | Account account ->
                  let address = Utils.address_of_account config account in
                  let node = Config.current_node config in
                  let client = CLIENT.create node.node_url in
                  Printf.eprintf "Querying last shard blockid for address\n%!";
                  BLOCK.find_last_shard_block ~client ~address
            in

            Printf.eprintf "Querying shard of block %S\n%!" blockid;
            begin match
                Utils.post config (
                  REQUEST.block ~level:1 (`string blockid)) with
            | [ { bl_shard = Some id ;
                  bl_workchain_id = Some workchain_id ;
                  _ } ] ->
                workchain_id, id
            | _ ->
                Error.raise "No block with id %S" blockid
            end
      in
      Printf.eprintf "shard = %S\n%!" shard;
      Some ( REQUEST.aeq "shard" ( REQUEST.astring shard ) @
             REQUEST.aeq "workchain_id" ( REQUEST.aint workchain_id ) )

let inspect_block ~level ?shard ?limit id =
  let config = Config.config () in
  let filter = filter_of_shard config shard in
  let blocks =
    Utils.post config (match id with
        | `string "all" -> REQUEST.blocks ~level ?limit ?filter []
        | _ -> REQUEST.block ~level ?filter id)
  in
  List.iter (fun b ->
      Printf.printf "\nBLOCK: %s\n%!"
        (ENCODING.string_of_block b) ) blocks

let inspect_head ~level ~shard ?limit:_ () =
  let config = Config.config () in
  let filter = filter_of_shard config shard in
  let blocks =
    Utils.post config (REQUEST.head ?filter ~level ())
  in
  List.iter (fun b ->
      Printf.printf "BLOCK: %s\n%!"
        (ENCODING.string_of_block b) ) blocks

type inspect =
  | Transaction
  | Account
  | Message
  | BlockId
  | BlockN
  | Head

let cmd =
  let shard = ref None in
  let inspect = ref [] in
  let level = ref 1 in
  let limit = ref None in
  let to_inspect kind x =
    inspect := (kind, x) :: !inspect
  in
  EZCMD.sub
    "inspect"
    (fun () ->
       List.iter (fun (kind, s) ->
           match kind with
           | Transaction ->
               inspect_transaction ~level:!level ?limit:!limit s
           | Account ->
               inspect_account ~level:!level ?limit:!limit s
           | Message ->
               inspect_message ~level:!level ?limit:!limit s
           | BlockId ->
               inspect_block ~level:!level ?limit:!limit (`string s)
           | BlockN ->
               inspect_block ~level:!level ?shard:!shard ?limit:!limit
                 (`int (int_of_string s))
           | Head ->
               inspect_head ~level:!level ?limit:!limit ~shard:!shard ()
         ) (List.rev !inspect)
    )
    ~args:
      [
        [ "2" ], Arg.Unit (fun () -> level := 2),
        EZCMD.info "Verbosity level 2";

        [ "3" ], Arg.Unit (fun () -> level := 3),
        EZCMD.info "Verbosity level 3";

        [ "t" ], Arg.String (to_inspect Transaction),
        EZCMD.info "TR_ID Inspect transaction TR_ID on blockchain";

        [ "a" ], Arg.String (to_inspect Account),
        EZCMD.info "ACCOUNT Inspect account TR_ID on blockchain";

        [ "m" ], Arg.String (to_inspect Message),
        EZCMD.info "MSG_ID Inspect message MSG_ID on blockchain";

        [ "b" ], Arg.String (to_inspect BlockId),
        EZCMD.info "BLOCK Inspect block TR_ID on blockchain";

        (* The following queries require a shard, that is either provided
           directly with --shard SHARD, or indirectly with
           --blockid ID or --account ADDR *)

        [ "shard" ], Arg.String (fun s -> shard := Some (Shard s) ),
        EZCMD.info "SHARD Block info level/head for this shard";
        [ "shard-block" ], Arg.String (fun s -> shard := Some (Blockid s) ),
        EZCMD.info "BLOCK_ID Block info level/head for this shard";
        [ "shard-account" ], Arg.String (fun s -> shard := Some (Account s) ),
        EZCMD.info "ACCOUNT Block info level/head for this shard";

        [ "bn" ], Arg.String (to_inspect BlockN),
        EZCMD.info "LEVEL Inspect block at LEVEL on blockchain";

        [ "h" ], Arg.Unit (fun () -> to_inspect Head ""),
        EZCMD.info "Inspect head";

        [ "limit" ], Arg.Int (fun n -> limit := Some n),
        EZCMD.info "LIMIT Limit the number of results to LIMIT";
      ]
    ~doc: "Monitor a given account"
