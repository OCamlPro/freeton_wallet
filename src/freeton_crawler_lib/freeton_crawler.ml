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

(* open EzCompat *)
open Ton_sdk

let (let>) = Lwt.bind

let wait_before_retry delay =
  Lwt_unix.sleep (float_of_int delay)

let iter_event ~tr_lt ~time ~network_url ~abi ~client ~msg_id f =
  Printf.eprintf "iter_event\n%!";
  let rec iter () =
    Printf.eprintf "iter()\n%!";
    let> result = REQUEST.post_lwt network_url
        (REQUEST.messages ~level:2 ~id:msg_id []) in
    match result with
    | Error exn ->
      Printf.eprintf "iter_message: exception %s\n%!"
        (Printexc.to_string exn);
      let> () = wait_before_retry 3 in
      iter ()
    | Ok [ msg ] ->
      Printf.printf "  MESSAGE: %s\n%!"
        ( ENCODING.string_of_message msg ) ;
      begin (* decode_message only works when there is a msg_body too *)
        match msg.msg_body with
        | None -> Lwt.return_unit
        | Some _body ->
          match msg.msg_boc with
          | None -> assert false
          | Some boc ->
            match BLOCK.decode_message_boc ~client ~boc ~abi with
            | decoded ->
              if decoded.body_type = 3 (* Event *) then
                f ~tr_lt ~time ~msg_id decoded.body_name decoded.body_args
              else
                Lwt.return_unit
            | exception exn ->
                Printf.eprintf "iter_event: exception %s\n%!"
                  (Printexc.to_string exn);
                Lwt.return_unit
      end
    | _ -> assert false
  in
  iter ()

let rec iter_transactions ~block_id ~network_url ~address f =
  Printf.eprintf "iter_transactions
\n%!";
  let> res =
    REQUEST.post_lwt network_url
      (REQUEST.transactions
         ~level:3
         ~block_id
         ~account_addr:address [])
  in
  match res with
  | Ok [] -> Lwt.return_unit
  | Ok trs ->
    Printf.eprintf "In block with id: %S\n%!" block_id;
    Lwt_list.iter_s (fun tr ->
        Printf.eprintf "\nTRANSACTION: %s\n%!"
          (ENCODING.string_of_transaction tr);
        f tr) trs
  | Error _ ->
    let> () = wait_before_retry 3 in
    iter_transactions ~block_id ~network_url ~address f

let timeout = 1_000_000L
;;
(*
let fast_iter ~network_url ~block_id ~address f =
  let> res = REQUEST.post_lwt network_url (
      REQUEST.block ~level:1 (`string block_id))
  in
  let last =
    match res with
    | Ok [ bl ] -> bl
    | _ ->
      Printf.eprintf "Error: cannot find block %s\n%!" block_id;
      exit 2
  in
  let shard_filter =
    match last with
    | { bl_shard = Some shard ; bl_workchain_id = Some wc ; _ }  ->
      ( REQUEST.aeq "shard" ( REQUEST.astring shard ) @
        REQUEST.aeq "workchain_id" ( REQUEST.aint wc ) )
    | _ ->
      Printf.eprintf "Error: cannot parse block %s\n%!" block_id;
      exit 2
  in
  let> res =
    REQUEST.post_lwt network_url (
      REQUEST.head ~filter:shard_filter ~level:1 ()) in
  let head = match res with
    | Ok [ head ] -> head
    | _ ->
      Printf.eprintf "Error: cannot find block %s\n%!" block_id;
      exit 2
  in
  let rec paginate last.bl
  if head.bl_id <> last.bl_id then
    paginate last head
  else
    Lwt.return_unit

let abi = match Contracts.read "FtdrawitRoot.abi.json" with
  | None -> assert false
  | Some abi -> abi
*)



let iter_blocks ~client ~block_id ~address f =
  let rec iter block_id =
    match BLOCK.wait_next_block
            ~client ~block_id ~address
            ~timeout () with
    | b ->
      let block_id = b.id in
      Printf.eprintf "new blockid: %S\n%!" b.id;
      Printf.eprintf "block = %s\n%!"
        (Ton_sdk.TYPES.string_of_block b) ;
      let> () = f b in
      iter block_id
    | exception exn ->
      Printf.eprintf "Exception wait_next_block: %s\n%!"
        ( Printexc.to_string exn );
      let> () = wait_before_retry 3 in
      iter block_id
  in
  iter block_id
;;

(*
let map_of_json ?(root=[]) json =
  let json = Ezjsonm.from_string json in
  let map = ref StringMap.empty in
  let add path s =
    let key = String.concat ":" ( List.rev path ) in
    map := StringMap.add key s !map
  in
  let rec iter path json =
    add path json;
    match json with
      `O list ->
        List.iter (fun (s,v) ->
            iter (s :: path) v
          ) list
    | `A list ->
        List.iteri (fun i v ->
            iter (string_of_int i :: path) v
          ) list
    | `Bool _
    | `Null
    | `Float _
    | `String _ -> ()
  in
  iter (List.rev root) json;
  !map
*)

let curtime = ref 0.
let update_curtime () = curtime := Unix.gettimeofday ()

(*
let find_uint_param name args =
  match StringMap.find name args with
  | exception Not_found -> assert false
  | `String s -> int_of_string s
  | _ -> assert false
*)

let main ~url ~address ~abi =

  let network_url = url in
  let client = Ton_sdk.CLIENT.create url in


  let known_transactions = Hashtbl.create 11111 in

  (* Load all known transactions from DB *)
  let rec iter before_lt =

    let> transactions = Db.TRANSACTIONS.list ?before_lt () in
    let before_lt = ref Int64.max_int in
    List.iter (fun tr ->
        Hashtbl.add known_transactions tr.Db.TRANSACTIONS.tr_id false;
        if !before_lt > tr.Db.TRANSACTIONS.tr_lt then
          before_lt := tr.Db.TRANSACTIONS.tr_lt
      ) transactions;
    match transactions with
    | [] -> Lwt.return_unit
    | _ -> iter (Some !before_lt)
  in
  let> () = iter None in

  let handle_event ~tr_lt ~time ~msg_id event_name event_args =
    update_curtime ();
    let event_args = match event_args with
      | None -> "{}"
      | Some args -> args in
    Printf.eprintf "*\n**\n***\n\n%Ld\n" time;
    Printf.eprintf "%Ld EVENT: %s %s\n\n\n\n%!" tr_lt event_name event_args;

    let> already_in = Db.EVENTS.mem ~msg_id in
    if not already_in then

      Db.EVENTS.add ~msg_id {
        Db.EVENTS.name = event_name ;
        args = event_args ;
        time ;
        tr_lt ;
      }

    else
      Lwt.return_unit

  in

  let add_transaction tr =
    Hashtbl.add known_transactions tr.ENCODING.tr_id true;
    let tr_lt, now = match tr.tr_lt, tr.tr_now with (* need level>=1 *)
      | Some lt, Some now ->
          Int64.of_string lt, Int64.of_float now
      | _ -> assert false
    in
    let> () =
      if tr.tr_aborted then begin
        Printf.eprintf "EVENT transaction %s aborted\n%!" tr.tr_id;
        Lwt.return_unit
      end else
        Lwt_list.iter_s (fun msg_id ->
            iter_event
              ~tr_lt
              ~time:now
              ~abi ~network_url ~client ~msg_id handle_event
          )
          tr.tr_out_msgs
    in
    let json = EzEncoding.construct ~compact:true
        Ton_sdk.ENCODING.transaction_enc
        tr in
    Db.TRANSACTIONS.add ~tr_lt ~block_id: tr.tr_block_id ~tr_id:tr.tr_id ~json
  in

  let block_id = Ton_sdk.BLOCK.find_last_shard_block ~client ~address in

  let> last_trans_lt =
    let> result = REQUEST.post_lwt network_url
        (REQUEST.account ~level:1 address) in
    match result with
    | Ok [ { acc_last_trans_lt = Some last_trans_lt ;  _} ] ->
        Lwt.return last_trans_lt
    | Ok [] ->
        Printf.eprintf "No contract event_address\n%!";
        exit 2
    | Error exn ->
        Printf.eprintf "Failed to load event_address last_trans_lt: %s\n%!"
          (Printexc.to_string exn);
        exit 2
    | Ok _ -> assert false
  in

  let rec iter_new_transactions trs =
    Printf.eprintf "iter_new_transactions\n%!";
    match trs with
    | [] -> Lwt.return_unit
    | tr :: trs ->
        let> () =
          if Hashtbl.mem known_transactions tr.ENCODING.tr_id then
            Lwt.return_unit
          else begin
            Printf.eprintf "Adding former transaction %s\n%!" tr.tr_id;
            add_transaction tr
          end
        in
        iter_new_transactions trs
  in

  let rec paginate_transactions next last_trans_lt =
    Printf.eprintf "paginate_transactions...\n%!";
    let> result = REQUEST.post_lwt network_url
        (REQUEST.transactions ~level:3
           ~account_addr:address
           ~limit:5
           ~order:("lt", None)
           ~filter:( REQUEST.acomp "lt" ~comp:"lt"
                       (REQUEST.astring last_trans_lt))
           [])
    in
    match result with
    | Ok [] -> iter_new_transactions next
    | Ok ( ( { tr_id ; tr_lt = Some lt ; _ } :: _ ) as trs ) ->
        if Hashtbl.mem known_transactions tr_id then
          iter_new_transactions (trs @ next)
        else
          paginate_transactions ( trs @ next ) lt
    | Error exn ->
        Printf.eprintf "failed to load new transactions %s\n%!"
          (Printexc.to_string exn);
        exit 2
    | Ok _ -> assert false (* maybe level to low ? *)
  in

  Printf.eprintf "Finding former transactions...\n%!";
  let> () = paginate_transactions [] last_trans_lt in

  Printf.eprintf "Waiting for next blocks...\n%!";
  iter_blocks
    ~block_id
    ~client
    ~address
    (fun b ->
       let block_id = b.id in
       let> () =
         iter_transactions
           ~block_id
           ~network_url
           ~address
           (fun tr ->
              if Hashtbl.mem known_transactions tr.tr_id then
                Lwt.return_unit
              else
                add_transaction tr
           )
       in
       Lwt.return_unit
    )
