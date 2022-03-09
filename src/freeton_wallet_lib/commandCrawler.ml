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

open Ez_file.V1
open Ezcmd.V2
open EZCMD.TYPES

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

let run_action ~dropdb ~account ~action =
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


let cmd =

  let dropdb = ref false in
  let account = ref None in
  let action = ref Crawler in
  EZCMD.sub
    "crawler"
    (fun () ->
       run_action ~dropdb:!dropdb ~account:!account ~action:!action
    )
    ~doc: "Crawl all transactions to an address and fill a psql database"
    ~args:[
      [], Arg.Anon (0, fun addr -> account := Some addr),
      EZCMD.info ~docv:"ACCOUNT" "Account to crawl" ;

      [ "start" ], Arg.Unit (fun () -> action := Start),
      EZCMD.info "Start with a manager process to restart automatically" ;

      [ "status" ], Arg.Unit (fun () -> action := Status),
      EZCMD.info "Check if a manager process and crawler are running" ;

      [ "stop" ], Arg.Unit (fun () -> action := Stop),
      EZCMD.info "Stop the manager process and the crawler" ;

      [ "dropdb" ], Arg.Set dropdb,
      EZCMD.info "Drop the previous database" ;

    ]
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command will crawl the blockchain and fill a \
            PostgresQL database with all events related to the \
            contract given in argument. The created database has the \
            same name as the account.";
        `P "This command can run as a service, using the --start \
            command to launch a manager program (that will not detach \
            itself, however), --status to check the current status \
            (running or not) and --stop to stop the process and its \
            manager.";
        `P "A simple session looks like:";
        `Pre {|sh> ft crawler myapp --start &> daemon.log &
sh> psql myapp
SELECT * FROM freeton_events;
serial|                              msg_id                              |      event_name       |           event_args                            |    time    | tr_lt
    1 | ec026489c0eb2071b606db0c7e05e5a76c91f4b02c2b66af851d56d5051be8bd | OrderStateChanged     | {"order_id":"31","state_count":"1","state":"1"} | 1620744626 | 96
SELECT * FROM freeton_transactions;
^D
sh> ft crawler myapp --stop
|};
      ];
      `S "ERRORS";
      `Blocks [
        `P "The crawler may fail connecting to the database. You can \
            use PGHOST to set the hostname of the database, or the \
            directory of unix sockets (default is \
            /var/run/postgresql). You can use PGPORT for the port \
            (default is 5432).";
        `P {|The crawler may also fail for authorizations (something like FATAL: 28000: role USER does not exist ). In such a case, you need to configure postgresql to allow your role (<user> is your username):|};
        `Pre {|
     sh> sudo -i -u postgres
     root> psql
     CREATE USER <user>;
     ALTER ROLE <user> CREATEDB;
|}


      ];
    ]
