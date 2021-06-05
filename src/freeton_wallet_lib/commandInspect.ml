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

(*

    // safe msig
    0x80d6c47c4a25543c9b397b71716f3fae1e2c5d247174c52e2c19bd896442b105
        // surf msig
       0x207dc560c5956de1a2c1479356f8f3ee70a59767db2bf4788b1d61ad42cdad82
        // 24 msig
        0x7d0996943406f7d62a4ff291b1228bf06ebd3e048b58436c5b70fb77ff8b4bf2 &&
        // 24 setcode msig
            code_hash != 0xa491804ca55dd5b28cffdff48cb34142930999621a54acee6be83c342051d884 &&
        // setcode msig
            code_hash != 0xe2b60b6b602c10ced7ea8ede4bdf96342c97570a3798066f3fb50a4b2b27a208) {
            _start();
            return;
        }
        preMain();
    }

*)

open Ezcmd.V2
open EZCMD.TYPES
(* open Ez_subst.V1 *)
(* open EzFile.OP *)
open Ton_sdk (* REQUEST, ENCODING *)
open Types

let indent spaces s =
  match EzString.split s '\n' with
  | [] -> ""
  | lines ->
      String.concat ("\n" ^ spaces) ("" :: lines)

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
          let addr = Misc.raw_address address in
          let filter =
            REQUEST.(aeq field (astring addr))
          in
          let order =
            ( "created_at" , None )
          in
          REQUEST.messages ~level ?limit ~order ~filter [])

let query_messages ~client ~abis queue config ~level ids =
  let unknown = queue in
  let> res = Lwt_list.map_s (fun msg_id ->
      let> ms = query_message ~level config msg_id in
      Lwt_list.map_s (fun m ->
          AbiCache.check_account ~unknown config ~abis
            ~address:m.ENCODING.msg_dst ;
          AbiCache.check_account ~unknown config ~abis
            ~address: m.msg_src ;
          match AbiCache.parse_message_body ~client ~abis m with
          | None -> Lwt.return ( m, None )
          | Some body ->
              Lwt.return (
                { m with msg_body = None ;
                         msg_boc = None ;
                },
                Some body )
        ) ms
    ) ids in
  Lwt.return (List.flatten res)

let transaction ~abis ~level tr =
  let tr_account_addr =
    AbiCache.replace_addr ~abis ~address:tr.ENCODING.tr_account_addr in
  let tr_boc = if level = 4 then tr.tr_boc else None in

  let tr_tr_type_name = match tr.ENCODING.tr_tr_type_name with
    | Some "Ordinary" -> None | x -> x in

  {
    tr with
    tr_boc ;
    tr_account_addr ;
    tr_tr_type_name ;
  }

(* Simplify a message before display *)
let message ~abis ~level m =
  let msg_boc, msg_body =
    if level = 1 then None, None else
      m.ENCODING.msg_boc, m.msg_body
  in
  let msg_src = AbiCache.replace_addr ~abis ~address:m.msg_src in
  let msg_dst = AbiCache.replace_addr ~abis ~address:m.msg_dst in

  (* remove defaults *)
  let msg_bounce = match m.msg_bounce with
    | Some true -> None | x -> x in
  let msg_bounced = match m.msg_bounced with
    | Some false -> None | x -> x in
  let msg_ihr_fee = match m.msg_ihr_fee with
    | Some z when z = Z.zero -> None | x -> x in
  let msg_import_fee = match m.msg_import_fee with
    | Some z when z = Z.zero -> None | x -> x in
  let msg_msg_type_name = match m.msg_msg_type_name with
    | Some "Internal" -> None | x -> x in
  let msg_status_name = match m.msg_status_name with
    | Some "Finalized" -> None | x -> x in

  { m with
    msg_dst ;
    msg_src ;
    msg_boc ;
    msg_body ;
    msg_bounce ;
    msg_bounced ;
    msg_ihr_fee ;
    msg_import_fee ;
    msg_msg_type_name ;
    msg_status_name ;
  }


let string_of_transactions_with_messages ~abis ~level trs =
  String.concat ""
    (List.map (fun ( tr, in_message , out_messages ) ->
         Printf.sprintf "%s\n%s%s"
           ( ENCODING.string_of_transaction ( transaction ~abis ~level tr ) )
           (match in_message with
            | None -> ""
            | Some ( in_message, in_body ) ->
                Printf.sprintf "\n  IN MESSAGE:%s%s"
                  ( indent "    "
                      ( ENCODING.string_of_message
                          ( message ~abis ~level in_message )))
                  (match in_body with
                   | None -> ""
                   | Some body -> Printf.sprintf "\n    %s\n" body )
           )
           (match out_messages with
            | [] -> ""
            | _ ->
                String.concat ""
                      ( List.map (fun ( out_msg, out_body ) ->
                            Printf.sprintf "\n  OUT MESSAGE:%s%s"
                              ( indent "    "
                                  ( ENCODING.string_of_message
                                      ( message ~abis ~level out_msg ) ) )
                              (match out_body with
                               | None -> ""
                               | Some body ->
                                   Printf.sprintf "\n    %s\n" body )
                          ) out_messages ))
       ) trs)

let transaction_with_message ~client ~abis queue config tr =
  let> tr =
    if tr.ENCODING.tr_aborted then
      let> res = Utils.post_lwt config
          ( REQUEST.transaction ~level:3 tr.tr_id )
      in
      match res with
      | [ tr ] -> Lwt.return tr
      | _ -> assert false
    else
      Lwt.return tr
  in
  let> res = query_messages ~client ~abis
      ~level:2 queue config [ tr.ENCODING.tr_in_msg ] in
  let in_message = match res with
    | [] -> None
    | [x] -> Some x
    | _ -> assert false
  in
  let> out_messages = query_messages ~client ~abis
      ~level:2 queue config tr.tr_out_msgs in
  Lwt.return (
    tr ,
    in_message ,
    out_messages
  )

let inspect_transaction queue ~level ?limit ~abis ~subst tr_id =
  Lwt_main.run (
    let config = Config.config () in
    let abis = AbiCache.create config ~abis in

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
      Lwt_list.map_s
        (transaction_with_message ~abis ~client queue config) trs in
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
        let addr = Misc.raw_address address in
        REQUEST.account ~level addr
  in
  let accounts =
    Utils.post config request
  in
  subst ~msg:"ACCOUNT" config
    (ENCODING.string_of_accounts accounts)

let inspect_account_past ~level ?limit ~abis queue account =
  let config = Config.config () in
  let abis = AbiCache.create config ~abis in
  let address = Utils.address_of_account config account in
  let address = Misc.raw_address address in
  let node = Config.current_node config in
  let client = CLIENT.create node.node_url in
  let url = node.node_url in
  let n = ref 0 in
  Lwt_main.run @@
    REQUEST.iter_past_transactions
    ~address ~url ~level ?limit
    (fun tr ->
       incr n;
       let> ( (t, _, _) as tr ) =
         transaction_with_message ~abis ~client queue config tr in
       Printf.printf "Transaction: %Ld %s\n\n%!"
         (match t.ENCODING.tr_lt with
            None -> assert false
          | Some lt -> Int64.of_string lt)
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
                  let address = Misc.raw_address address in
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
  let abis = ref [] in
  let shard = ref None in
  let inspect = ref [] in
  let level = ref 1 in
  let limit = ref None in
  let queue = ref [] in
  let to_inspect kind x =
    inspect := (kind, x) :: !inspect
  in
  let subst_args, subst = Subst.make_args () in
  EZCMD.sub
    "inspect"
    (fun () ->
       List.iter (fun (kind, s) ->
           queue := List.rev !queue ;
           match kind with
           | Transaction ->
               inspect_transaction queue ~level:!level ?limit:!limit
                 ~abis:!abis ~subst s
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
               inspect_account_past queue ~level:!level ?limit:!limit s
                 ~abis:!abis
         ) (List.rev !inspect)
    )
    ~args:
      ( subst_args
          [
            [ "2" ], Arg.Unit (fun () -> level := 2),
            EZCMD.info "Verbosity level 2";

            [ "3" ], Arg.Unit (fun () -> level := 3),
            EZCMD.info "Verbosity level 3";

            [ "4" ], Arg.Unit (fun () -> level := 4),
            EZCMD.info "Verbosity level 4";

            [ "t"; "transaction" ], Arg.String (to_inspect Transaction),
            EZCMD.info ~docv:"TR_ID"
              "Inspect transaction with identifier TR_ID on blockchain";

            [ "past" ], Arg.String (to_inspect AccountPast),
            EZCMD.info ~docv:"ACCOUNT"
              "Inspect past transactions on ACCOUNT on blockchain";

            [ "a"; "account" ], Arg.String (to_inspect Account),
            EZCMD.info ~docv:"ACCOUNT"
              "Inspect state of account ACCOUNT (or 'all') on blockchain";

            [ "m"; "message" ], Arg.String (to_inspect Message),
            EZCMD.info ~docv:"MSG_ID"
              "Inspect message with identifier MSG_ID on blockchain";

            [ "b" ; "block" ], Arg.String (to_inspect BlockId),
            EZCMD.info ~docv:"BLOCK"
              "BLOCK Inspect block TR_ID on blockchain";

            (* The following queries require a shard, that is either provided
               directly with --shard SHARD, or indirectly with
               --blockid ID or --account ADDR *)

            [ "shard" ], Arg.String (fun s -> shard := Some (Shard s) ),
            EZCMD.info ~docv:"SHARD"
              "Block info level/head for this shard";

            [ "shard-block" ],
            Arg.String (fun s -> shard := Some (Blockid s) ),
            EZCMD.info ~docv:"BLOCK_ID"
              "Block info level/head for this shard";

            [ "shard-account" ],
            Arg.String (fun s -> shard := Some (Account s) ),
            EZCMD.info ~docv:"ACCOUNT"
              "Block info level/head for this shard";

            [ "bn"; "block-num" ], Arg.String (to_inspect BlockN),
            EZCMD.info ~docv:"BLOCK_NUM"
              "Inspect block at level BLOCK_NUM on blockchain";

            [ "h" ; "head" ], Arg.Unit (fun () -> to_inspect Head ""),
            EZCMD.info "Inspect head";

            [ "limit" ], Arg.Int (fun n -> limit := Some n),
            EZCMD.info ~docv:"NUM" "Limit the number of results to NUM";

            ["with"], Arg.String (fun s -> queue := s :: !queue),
            EZCMD.info ~docv:"ACCOUNT:CONTRACT"
              "Define partner account automatically defined";

            [ "abis" ], Arg.String (fun s ->
                abis := ( EzString.split s ':' ) @ !abis ),
            EZCMD.info ~docv:"ABI"
              "Shared ABIs. Useful for example if you expect to \
               receive messages that your contract does not implement \
               (IParticipant for SafeMultisigWallet, for example)";

          ]
      )
    ~doc: "Inspect information stored on the blockchain: display \
           information on accounts, blocks, messages and transactions."
    ~man:[
      `S "DESCRIPTION";
      `P  "Inspect information stored on the blockchain: display \
           information on accounts, blocks, messages and transactions.";
      `P "Examples:";
      `P "Display all transactions that happened on the user1 account:";
      `Pre {|$ ft inspect --past user1 --with deployed:Contract|};
      `P "The --with argument is used to name the first unknown \
          address, with the name 'deployed' and type \
          'Contract'. Messages sent to known accounts with known \
          contract types are automatically decoded.";
      `P "Some operations (--block-num and --head) require to know the \
          shard on which they apply. Arguments --shard SHARD, \
          --shard-block BLOCK_ID and --shard-account ACCOUNT can be \
          used to specify the shard.";
      `P "Use the FT_DEBUG_GRAPHQL=1 variable to show Graphql queries";
    ]
