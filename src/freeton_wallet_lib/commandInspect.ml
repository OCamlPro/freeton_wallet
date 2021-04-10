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

let query_message config ~level msg_id =
  Utils.post config (match msg_id with
      | "all" -> REQUEST.messages ~level []
      | _ -> REQUEST.messages ~level ~id:msg_id [])


let query_messages config ~level ids =
  List.flatten ( List.map  ( query_message ~level config ) ids )

let inspect_transaction ~level tr_id =
  let config = Config.config () in

  let trs =
    Utils.post config
      (match tr_id with
       | "all" -> REQUEST.transactions ~level []
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

let inspect_account ~level account =
  let config = Config.config () in
  let request = match account with
    | "all" ->
        REQUEST.accounts ~level []
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


let inspect_message ~level id =
  let config = Config.config () in
  let messages = query_message config ~level id in
  List.iter (fun msg ->
      Printf.printf "MESSAGE\n: %s\n%!"
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

let inspect_block ~level ?shard id =
  let config = Config.config () in
  let filter = filter_of_shard config shard in
  let blocks =
    Utils.post config (match id with
        | `string "all" -> REQUEST.blocks ~level ?filter []
        | _ -> REQUEST.block ~level ?filter id)
  in
  List.iter (fun b ->
      Printf.printf "\nBLOCK: %s\n%!"
        (ENCODING.string_of_block b) ) blocks

let inspect_head ~level ~shard () =
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
  let to_inspect level kind x =
    inspect := (level, kind, x) :: !inspect
  in
  EZCMD.sub
    "inspect"
    (fun () ->
       List.iter (fun (level, kind, s) ->
           match kind with
           | Transaction ->
               inspect_transaction ~level:level s
           | Account ->
               inspect_account ~level:level s
           | Message ->
               inspect_message ~level:level s
           | BlockId ->
               inspect_block ~level:level (`string s)
           | BlockN ->
               inspect_block ~level:level ?shard:!shard
                 (`int (int_of_string s))
           | Head ->
               inspect_head ~level:level ~shard:!shard ()
         ) (List.rev !inspect)
    )
    ~args:
      [
        [ "t" ], Arg.String (to_inspect 1 Transaction),
        EZCMD.info "TR_ID Inspect transaction TR_ID on blockchain";
        [ "t2" ], Arg.String (to_inspect 2 Transaction),
        EZCMD.info "TR_ID Inspect transaction TR_ID on blockchain";
        [ "t3" ], Arg.String (to_inspect 3 Transaction),
        EZCMD.info "TR_ID Inspect transaction TR_ID on blockchain";

        [ "a" ], Arg.String (to_inspect 1 Account),
        EZCMD.info "ACCOUNT Inspect account TR_ID on blockchain";
        [ "a2" ], Arg.String (to_inspect 2 Account),
        EZCMD.info "ACCOUNT Inspect account TR_ID on blockchain";
        [ "a3" ], Arg.String (to_inspect 3 Account),
        EZCMD.info "ACCOUNT Inspect account TR_ID on blockchain";

        [ "m" ], Arg.String (to_inspect 1 Message),
        EZCMD.info "MSG_ID Inspect message MSG_ID on blockchain";
        [ "m2" ], Arg.String (to_inspect 2 Message),
        EZCMD.info "MSG_ID Inspect message MSG_ID on blockchain";
        [ "m3" ], Arg.String (to_inspect 3 Message),
        EZCMD.info "MSG_ID Inspect message MSG_ID on blockchain";

        [ "b" ], Arg.String (to_inspect 1 BlockId),
        EZCMD.info "BLOCK Inspect block TR_ID on blockchain";
        [ "b2" ], Arg.String (to_inspect 2 BlockId),
        EZCMD.info "BLOCK Inspect block TR_ID on blockchain";
        [ "b3" ], Arg.String (to_inspect 3 BlockId),
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

        [ "bn" ], Arg.String (to_inspect 1 BlockN),
        EZCMD.info "LEVEL Inspect block at LEVEL on blockchain";
        [ "bn2" ], Arg.String (to_inspect 2 BlockN),
        EZCMD.info "LEVEL Inspect block at LEVEL on blockchain";
        [ "bn3" ], Arg.String (to_inspect 3 BlockN),
        EZCMD.info "LEVEL Inspect block at LEVEL on blockchain";

        [ "h" ], Arg.Unit (fun () -> to_inspect 1 Head ""),
        EZCMD.info "Inspect head";
        [ "h2" ], Arg.Unit (fun () -> to_inspect 2 Head ""),
        EZCMD.info "Inspect head";
        [ "h3" ], Arg.Unit (fun () -> to_inspect 3 Head ""),
        EZCMD.info "Inspect head";
      ]
    ~doc: "Monitor a given account"
