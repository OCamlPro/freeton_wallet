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
open Types

let matches re field =
  match Re.Str.search_forward re field 0 with
  | exception Not_found -> false
  | _ -> true

let action accounts =
  let config = Config.config () in
  let net = Config.current_network config in
  List.iter (fun address ->
      let re = Re.Str.regexp_string_case_fold address in
      List.iter (fun key ->

          if matches re key.key_name then
            Printf.printf "account match: %S\n%!" key.key_name ;

          begin
            match key.key_pair with
            | None -> ()
            | Some { public ; _ } ->
                if matches re ( PUBKEY.to_string public ) then
                  Printf.printf "pubkey match: %s is %S\n%!"
                    ( PUBKEY.to_string public ) key.key_name ;
          end;

          begin
            match key.key_account with
            | None -> ()
            | Some acc ->
                if matches re ( ADDRESS.to_string acc.acc_address ) then
                  Printf.printf "address match: %s is %S\n%!"
                    ( ADDRESS.to_string acc.acc_address ) key.key_name ;
                begin
                  match acc.acc_contract with
                  | None -> ()
                  | Some contract ->
                      if matches re contract then
                        Printf.printf "contract match: %S has contract %S\n%!"
                          key.key_name contract ;
                end
          end ;
        )
        net.net_keys ;
    ) accounts

let cmd =
  let accounts = ref [] in
  EZCMD.sub
    "account whois"
    (fun () ->
       action !accounts
    )
    ~args:
      [ [],
        Arg.Anons (fun args -> accounts := args),
        EZCMD.info "Name of account" ;

      ]
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command searches existing accounts for a field \
            matching the string" ;
        `P "Examples:";
        `Pre {|ft account whois 1234|} ;
        `Pre {|ft account whois 0:1234|} ;
        `Pre {|ft account whois setcode|} ;
      ];

    ]
    ~doc:
      "Find accounts matching a string"
