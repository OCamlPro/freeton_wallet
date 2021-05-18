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

let (let>) = Db_utils.(let>)

let action_start ~account =
  ProcessManager.start
    ( Printf.sprintf "crawler_%s" account )
    [| Sys.argv.(0) ; "crawler" ; account |]
let action_status ~account =
  ProcessManager.status
    ( Printf.sprintf "crawler_%s" account )
let action_stop ~account =
  ProcessManager.stop
    ( Printf.sprintf "crawler_%s" account )


let action_crawler ~url ~account ~address ~contract =

  let abifile = Misc.get_contract_abifile contract in
  let abi = EzFile.read_file abifile in

  Lwt_main.run (
    let> () = ProcessManager.set_worker
        ( Printf.sprintf "crawler_%s" account )
    in
    Freeton_crawler.main ~url ~address ~abi)

let check_database database dropdb =
  Db_utils.database := database ;
  if dropdb then EzPG.dropdb database;
  let dbh =
    match PGOCaml.connect ~database () with
    | exception _exn ->
        Db_utils.create ();
        PGOCaml.connect ~database ()
    | dbh -> dbh
  in
  EzPG.upgrade_database
    ~upgrades:Freeton_crawler_db_versions.Main.upgrades
    dbh;
  PGOCaml.close dbh

let get ~account =
  match account with
  | None -> Error.raise "You must specify an account to crawl"
  | Some account ->

      let config = Config.config () in
      let net = Config.current_network config in
      let node = Config.current_node config in
      let key = Misc.find_key_exn net account in
      let acc = Misc.get_key_account_exn key in
      match acc with
        { acc_address = address ;
          acc_contract = Some contract ; _ }
        ->
          account, address, contract, node.node_url
      | _ ->
          Error.raise "Contract for address is not known"


type action =
  | Crawler
  | Start
  | Status
  | Stop

let action ~dropdb ~account ~action =
  let ( account, address, contract, url ) = get ~account in
  match action with
  | Crawler ->
      check_database account dropdb ;
      action_crawler ~account ~address ~contract ~url
  | Start ->
      check_database account dropdb ;
      Lwt_main.run ( action_start ~account )
  | Status ->
      Db_utils.database := account ;
      Lwt_main.run ( action_status ~account )
  | Stop ->
      Db_utils.database := account ;
      Lwt_main.run ( action_stop ~account )
