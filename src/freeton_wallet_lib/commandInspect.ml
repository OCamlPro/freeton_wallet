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
open Types


let (let>) p f = Lwt.bind p f

let query_message config ~level ?limit msg_id =
  Utils.post_lwt config (
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

let query_messages ~client ~abis config ~level ids =
  let> res = Lwt_list.map_s (fun msg_id ->
      let> ms = query_message ~level config msg_id in
      Lwt_list.map_s (fun m ->
          match m.ENCODING.msg_boc with
          | Some boc ->
              begin
                try
                  let (_ , contract ) =
                    Hashtbl.find abis m.msg_dst in
                  match contract with
                  | None -> Lwt.return ( m, None )
                  | Some ( _ , abi ) ->
                      let abi = Lazy.force abi in
                      let decoded =
                        BLOCK.decode_message_boc ~client ~boc ~abi in
                      let body =
                        Printf.sprintf "CALL: %s %s %s"
                          (match decoded.body_type with
                           | 0 -> "Input"
                           | 1 -> "Output"
                           | 2 -> "InternalOutput"
                           | 3 -> "Event"
                           | _ -> assert false)
                          decoded.body_name
                          (match decoded.body_args with
                           | None -> ""
                           | Some args -> args)
                      in
                      Lwt.return (
                        { m with msg_body = None ;
                                 msg_boc = None ;
                        },
                        Some body )
                with _ ->
                  Lwt.return ( m, None )
              end
          | _ -> Lwt.return ( m, None )
        ) ms
    ) ids in
  Lwt.return (List.flatten res)

let replace_addr ~abis addr =
    match Hashtbl.find abis addr with
    | ( name, contract ) ->
        Printf.sprintf "%s (%s%s)" addr name
          ( match contract with
            | None -> ""
            | Some (contract, _ ) -> " " ^ contract)
    | exception Not_found -> addr

let transaction ~abis ~level tr =
  let tr_account_addr = replace_addr ~abis tr.ENCODING.tr_account_addr in
  let tr_boc = if level = 4 then tr.tr_boc else None in
  {
    tr with
    tr_boc ;
    tr_account_addr
  }

let message ~abis ~level m =
  let msg_boc, msg_body =
    if level = 1 then None, None else
      m.ENCODING.msg_boc, m.msg_body
  in
  let msg_src = replace_addr ~abis m.msg_src in
  let msg_dst = replace_addr ~abis m.msg_dst in
  { m with
    msg_dst ;
    msg_src ;
    msg_boc ;
    msg_body }


let string_of_transactions_with_messages ~abis ~level trs =
  String.concat ""
    (List.map (fun ( tr, in_message , out_messages ) ->
         Printf.sprintf "%s\n%s%s"
           ( ENCODING.string_of_transaction ( transaction ~abis ~level tr ) )
           (match in_message with
            | None -> ""
            | Some ( in_message, in_body ) ->
                Printf.sprintf "  IN MESSAGE:\n%s%s"
                  ( ENCODING.string_of_message
                      ( message ~abis ~level in_message ))
                  (match in_body with
                   | None -> ""
                   | Some body -> Printf.sprintf "\n    %s\n" body )
           )
           (match out_messages with
            | [] -> ""
            | _ ->
                String.concat ""
                      ( List.map (fun ( out_msg, out_body ) ->
                            Printf.sprintf "  OUT MESSAGE:\n%s%s"
                              ( ENCODING.string_of_message
                                  ( message ~abis ~level out_msg ) )
                              (match out_body with
                               | None -> ""
                               | Some body ->
                                   Printf.sprintf "\n    %s\n" body )
                          ) out_messages ))
       ) trs)


let abis_of_config config =
  let net = Config.current_network config in
  let t = Hashtbl.create 113 in
    List.iter (fun key ->
      match key.key_account with
      | Some { acc_address ; acc_contract ; _ } ->
          Hashtbl.add t acc_address
            ( key.key_name ,
              match acc_contract with
              | None -> None
              | Some contract ->
                  Some (
                    contract ,
                    lazy (
                      EzFile.read_file
                        ( Misc.get_contract_abifile contract ) ) ) )
      | _ -> ()
      ) net.net_keys;
    t

let transaction_with_message ~client ~abis config tr =
  let> res = query_messages ~client ~abis
      ~level:2 config [ tr.ENCODING.tr_in_msg ] in
  let in_message = match res with
    | [] -> None
    | [x] -> Some x
    | _ -> assert false
  in
  let> out_messages = query_messages ~client ~abis
      ~level:2 config tr.tr_out_msgs in
  Lwt.return (
    tr ,
    in_message ,
    out_messages
  )

let inspect_transaction ~level ?limit ~subst tr_id =
  Lwt_main.run (
    let config = Config.config () in
    let abis = abis_of_config config in

    let> trs =
      Utils.post_lwt config
        (match tr_id with
         | "all" -> REQUEST.transactions ~level ?limit []
         | _ -> REQUEST.transaction ~level tr_id
        )
    in
    let node = Config.current_node config in
    let client = CLIENT.create node.node_url in
    let> trs =
      Lwt_list.map_s (transaction_with_message ~abis ~client config) trs in
    subst ~msg:"TRANSACTION" config
      (string_of_transactions_with_messages ~abis ~level trs);
    Lwt.return_unit
  )

let inspect_account ~level ?limit ~subst account =
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
  subst ~msg:"ACCOUNT" config
    (ENCODING.string_of_accounts accounts)

let inspect_account_past ~level ?limit account =
  let config = Config.config () in
  let abis = abis_of_config config in
  let address = Utils.address_of_account config account in
  let node = Config.current_node config in
  let client = CLIENT.create node.node_url in
  let url = node.node_url in
  let n = ref 0 in
  Lwt_main.run @@
    REQUEST.iter_past_transactions
    ~address ~url ~level ?limit
    (fun tr ->
       incr n;
       let> tr = transaction_with_message ~abis ~client config tr in
       Printf.printf "Transaction: %s\n\n%!"
         (string_of_transactions_with_messages ~abis ~level [tr]);
       Lwt.return_unit
    );
  Printf.printf "%d transactions printed\n%!" !n


let inspect_message ~level ?limit ~subst id =
  Lwt_main.run (
    let config = Config.config () in
    let> messages = query_message config ~level ?limit id in
    subst ~msg:"MESSAGE" config
      (ENCODING.string_of_messages messages);
    Lwt.return_unit
  )

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

let inspect_block ~level ?shard ?limit ~subst id =
  let config = Config.config () in
  let filter = filter_of_shard config shard in
  let blocks =
    Utils.post config (match id with
        | `string "all" -> REQUEST.blocks ~level ?limit ?filter []
        | _ -> REQUEST.block ~level ?filter id)
  in
  subst ~msg:"BLOCK" config (ENCODING.string_of_blocks blocks)

let inspect_head ~level ~shard ?limit:_ ~subst () =
  let config = Config.config () in
  let filter = filter_of_shard config shard in
  let blocks =
    Utils.post config (REQUEST.head ?filter ~level ())
  in
  subst ~msg:"HEAD" config (ENCODING.string_of_blocks blocks)

type inspect =
  | Transaction
  | Account
  | Message
  | BlockId
  | BlockN
  | Head
  | AccountPast

let cmd =
  let shard = ref None in
  let inspect = ref [] in
  let level = ref 1 in
  let limit = ref None in
  let to_inspect kind x =
    inspect := (kind, x) :: !inspect
  in
  let args, subst = Subst.make_args () in
  EZCMD.sub
    "inspect"
    (fun () ->
       List.iter (fun (kind, s) ->
           match kind with
           | Transaction ->
               inspect_transaction ~level:!level ?limit:!limit ~subst s
           | Account ->
               inspect_account ~level:!level ?limit:!limit ~subst s
           | Message ->
               inspect_message ~level:!level ?limit:!limit ~subst s
           | BlockId ->
               inspect_block ~level:!level ?limit:!limit (`string s) ~subst
           | BlockN ->
               inspect_block ~level:!level ?shard:!shard ?limit:!limit ~subst
                 (`int (int_of_string s))
           | Head ->
               inspect_head ~level:!level ?limit:!limit ~shard:!shard ~subst ()
           | AccountPast ->
               inspect_account_past ~level:!level ?limit:!limit s
         ) (List.rev !inspect)
    )
    ~args:
      (
        [
          [ "2" ], Arg.Unit (fun () -> level := 2),
          EZCMD.info "Verbosity level 2";

          [ "3" ], Arg.Unit (fun () -> level := 3),
          EZCMD.info "Verbosity level 3";

          [ "4" ], Arg.Unit (fun () -> level := 4),
          EZCMD.info "Verbosity level 4";

          [ "t" ], Arg.String (to_inspect Transaction),
          EZCMD.info ~docv:"TR_ID"
            "Inspect transaction with identifier TR_ID on blockchain";

          [ "past" ], Arg.String (to_inspect AccountPast),
          EZCMD.info ~docv:"ACCOUNT"
            "Inspect past transactions on ACCOUNT on blockchain";

          [ "a" ], Arg.String (to_inspect Account),
          EZCMD.info ~docv:"ACCOUNT"
            "Inspect state of account ACCOUNT (or 'all') on blockchain";

          [ "m" ], Arg.String (to_inspect Message),
          EZCMD.info ~docv:"MSG_ID"
            "Inspect message with identifier MSG_ID on blockchain";

          [ "b" ], Arg.String (to_inspect BlockId),
          EZCMD.info ~docv:"BLOCK"
            "BLOCK Inspect block TR_ID on blockchain";

          (* The following queries require a shard, that is either provided
             directly with --shard SHARD, or indirectly with
             --blockid ID or --account ADDR *)

          [ "shard" ], Arg.String (fun s -> shard := Some (Shard s) ),
          EZCMD.info ~docv:"SHARD"
            "Block info level/head for this shard";
          [ "shard-block" ], Arg.String (fun s -> shard := Some (Blockid s) ),
          EZCMD.info ~docv:"BLOCK_ID"
            "Block info level/head for this shard";
          [ "shard-account" ], Arg.String (fun s -> shard := Some (Account s) ),
          EZCMD.info ~docv:"ACCOUNT"
            "Block info level/head for this shard";

          [ "bn" ], Arg.String (to_inspect BlockN),
          EZCMD.info ~docv:"BLOCK_NUM"
            "Inspect block at level BLOCK_NUM on blockchain";

          [ "h" ], Arg.Unit (fun () -> to_inspect Head ""),
          EZCMD.info "Inspect head";

          [ "limit" ], Arg.Int (fun n -> limit := Some n),
          EZCMD.info ~docv:"NUM" "Limit the number of results to NUM";
        ]
        @ args)
    ~doc: "Monitor a given account"
