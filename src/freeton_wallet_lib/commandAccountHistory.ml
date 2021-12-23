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
(* open Types *)

let z_ton = Z.of_string "1_000_000_000"

let action ~account ?limit () =
  let f ~n:_ ~abis ~level tr =
    let ( tr, in_message , out_messages ) = tr in
    (*
    Printf.sprintf "%s\n%s%s"
      ( ENCODING.string_of_transaction ( transaction ~abis ~level tr ) )
*)
    let now =
      let now = Option.get tr.Ton_sdk.ENCODING.tr_now in
      let tm = Unix.localtime now in
      Printf.sprintf "%04d/%02d/%02d %02d:%02d"
        (1900+tm.tm_year) (tm.tm_mon+1) tm.tm_mday
        tm.tm_hour tm.tm_min
    in
    begin
      match in_message with
      | None -> ()
      | Some ( in_msg, _body ) ->
          match in_msg.Ton_sdk.ENCODING.msg_value with
          | None -> ()
          | Some value ->
              if value > z_ton then
                let in_msg =
                  CommandInspect.simplify_message ~abis ~level in_msg in
                Printf.printf "%s: received %s from %s\n%!"
                  now (CommandWatch.ton_of_z value) in_msg.msg_src
    end;
    List.iter (fun ( out_msg, _body ) ->
        match out_msg.Ton_sdk.ENCODING.msg_value with
        | None -> ()
        | Some value ->
            if value > z_ton then
              let out_msg =
                CommandInspect.simplify_message ~abis ~level out_msg in
              Printf.printf "%s: sent %s to %s\n%!"
                now (CommandWatch.ton_of_z value) out_msg.msg_dst
      ) out_messages
  in
  CommandInspect.inspect_account_past
    ~level:1
    ?limit
    ~abis:[]
    ~f
    account

let cmd =
  let limit = ref None in
  let account = ref None in
  EZCMD.sub
    "account history"
    (fun () ->
       match !account with
       | None -> Error.raise "You must provide the ACCOUNT address"
       | Some account ->
           action ~account ?limit:!limit ()
    )
    ~args:
      [
        [],
        Arg.Anon (0, fun s -> account := Some s),
        EZCMD.info "Address of account" ;

        [ "limit" ], Arg.Int (fun n -> limit := Some n),
        EZCMD.info ~docv:"NUM" "Limit the number of results to NUM";

      ]
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command displays the history of the given account"
      ];

    ]
    ~doc:
      "Get account info (local or from blockchain)."
