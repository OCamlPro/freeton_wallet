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

open EzCompat
open Ezcmd.V2
open EZCMD.TYPES

type cmd_node =
  {
    mutable node_cmd : sub option ;
    mutable node_map : cmd_node StringMap.t ref ;
    mutable node_commands : sub list ;
  }

let echo () =
  Printf.eprintf "CMD: \x1B[1;33m %s \x1B[0m \n%!"
    ( String.concat " "
        ( List.filter (fun s -> s <> "--echo")
            ( Array.to_list Sys.argv )))

let increase_verbosity ()=
  if !Globals.verbosity = 1 then
    Printexc.record_backtrace true;
  incr Globals.verbosity

let main () =
  Printexc.record_backtrace false;
  let commands =
    [
      [ "switch" ; "list" ], CommandSwitchList.cmd;
      [ "switch" ; "to" ], CommandSwitchTo.cmd;
      [ "switch" ; "create" ], CommandSwitchCreate.cmd;
      [ "switch" ; "remove" ], CommandSwitchRemove.cmd;
      [ "switch" ; "config" ], CommandSwitchConfig.cmd;

(*
      [ "genaddr" ], CommandGenaddr.cmd; (* DEPRECATED *)
      [ "list" ], CommandList.cmd;    (* DEPRECATED *)
*)

      [ "account" ; "copy" ; "from" ], CommandAccountCopyFrom.cmd;
      [ "account" ; "rename" ], CommandAccountRename.cmd;
      [ "account" ; "create" ], CommandAccountCreate.cmd;
      [ "account" ; "whois" ], CommandAccountWhois.cmd;
      [ "account" ; "info" ], CommandAccountInfo.cmd;
      [ "account" ; "list" ], CommandAccountList.cmd;
      [ "account" ; "set" ], CommandAccountSet.cmd;
      [ "account" ; "remove" ], CommandAccountRemove.cmd;

      [ "client" ], CommandClient.cmd;
      [ "exec" ], CommandExec.cmd;
      [ "output" ], CommandOutput.cmd;
      [ "print"; "error" ], CommandPrintError.cmd;


      [ "multisig" ; "create" ], CommandMultisigCreate.cmd;
      [ "multisig" ; "transfer" ], CommandMultisigTransfer.cmd;
      [ "multisig" ; "list" ; "transactions" ],
      CommandMultisigListTransactions.cmd;
      [ "multisig" ; "list" ; "custodians" ],
      CommandMultisigListCustodians.cmd;
      [ "multisig" ; "confirm" ], CommandMultisigConfirm.cmd;
      [ "multisig" ; "debot" ], CommandMultisigDebot.cmd;

      [ "contract"; "list" ], CommandContractList.cmd;
      [ "contract"; "new" ], CommandContractNew.cmd;
      [ "contract"; "new" ; "intf" ], CommandContractNewIntf.cmd;
      [ "contract"; "build" ], CommandContractBuild.cmd;
      [ "contract"; "deploy" ], CommandContractDeploy.cmd;
      [ "contract"; "import" ], CommandContractImport.cmd;
      [ "contract"; "abi" ], CommandContractAbi.cmd;
      [ "contract"; "abi" ; "impl" ], CommandContractAbiImpl.cmd;
      [ "contract"; "abi" ; "intf" ], CommandContractAbiIntf.cmd;

      [ "test" ], CommandTest.cmd;
      [ "init" ], CommandInit.cmd;
      [ "doc" ], CommandDoc.cmd;
      [ "doc" ; "list" ], CommandDocList.cmd;

      [ "node" ; "start" ], CommandNodeStart.cmd;
      [ "node" ; "stop" ], CommandNodeStop.cmd;
      [ "node" ; "give" ], CommandNodeGive.cmd;
      [ "node" ; "live" ], CommandNodeLive.cmd;
      [ "node" ; "web" ], CommandNodeWeb.cmd;
      [ "node" ; "update" ], CommandNodeUpdate.cmd;

      [ "config" ], CommandConfig.cmd;
      [ "call" ], CommandCall.cmd;
      [ "utils" ], CommandUtils.cmd;
      [ "watch" ], CommandWatch.cmd;
      [ "inspect" ], CommandInspect.cmd;
      [ "crawler" ], CommandCrawler.cmd;
      [ "debot" ], CommandDebot.cmd;
    ]
  in

  let cmdmap =
    let cmdmap = ref StringMap.empty in
    let rec add_cmd map path cmd =
      match path with
        [] -> assert false
      | name :: path ->
          let node = match StringMap.find name !map with
            | exception Not_found ->
                let node = {
                  node_cmd = None ;
                  node_map = ref StringMap.empty ;
                  node_commands = [] ;
                } in
                map := StringMap.add name node !map;
                node
            | node -> node
          in
          node.node_commands <- cmd :: node.node_commands ;
          match path with
          | [] ->
              assert ( node.node_cmd = None );
              node.node_cmd <- Some cmd
          | _ -> add_cmd node.node_map path cmd
    in
    List.iter (fun ( path, cmd ) ->
        add_cmd cmdmap path cmd
      ) commands ;
    !cmdmap
  in

  let ez_commands = List.map snd commands in
  let common_args =
    [
      [ "v"; "verbose" ],
      Arg.Unit increase_verbosity,
      EZCMD.info "Increase verbosity level" ;
      [ "q"; "quiet" ],
      Arg.Unit (fun () -> Globals.verbosity := 0),
      EZCMD.info "Set verbosity level to 0";
      [ "switch" ],
      Arg.String (fun s ->
          Error.raise
            "Argument --switch %S must be used before subcommand" s),
      EZCMD.info "Set switch" ;
    ]
  in
  let args = Array.to_list Sys.argv in

  let rec iter_initial_args path map commands args =
    match args with
    | [] ->
        begin
          match path with
          | [] ->
              Printf.eprintf "Use 'ft --help' for help on commands\n%!";
              Config.print ();
              exit 0
          | _ -> path, args, commands
        end
    | "--switch" :: switch :: args ->
        Config.set_temporary_switch switch;
        iter_initial_args path map commands args
    | "--echo" :: args ->
        echo () ;
        iter_initial_args path map commands args
    | [ "--version" ] ->
        Printf.printf "%s\n%!" Version.version;
        exit 0
    | [ "--about" ] ->
        Printf.printf "%s\n%!" Globals.about;
        exit 0
    | ( "-v" | "--verbose" ) :: args ->
        increase_verbosity ();
        iter_initial_args path map commands args
    | ( "-q" | "--quiet" ) :: args ->
        Globals.verbosity := 0;
        iter_initial_args path map commands args
    | [ "rst" ] ->
        Printf.printf "%s%!" ( EZCMD.to_rst ez_commands common_args );
        exit 0
    | name :: rem_args ->
        match StringMap.find name map with
        | exception Not_found -> path, args, commands
        | node ->
            iter_initial_args
              ( name :: path ) (! (node.node_map) )
              node.node_commands
              rem_args
  in

  let path, args, ez_commands =
    iter_initial_args [] cmdmap ez_commands (List.tl args ) in
  let args = match path with
    | [] -> args
    | path -> ( String.concat " " (List.rev path) ) :: args
  in
  let args = List.map (fun arg ->
      let len = String.length arg in
      if len > 20 && arg.[0] = '-' &&
         match arg.[1] with
         '1'..'9' when String.contains arg ':' -> true
         | _ -> false
      then
        "0" ^ arg
      else
        arg
    ) args in
  let argv = Array.of_list ( Sys.argv.(0) :: args ) in
  (* OpambinMisc.global_log "args: %s"
         (String.concat " " (Array.to_list Sys.argv)); *)
  try
    begin
      match args with
      | [] -> ()
      | _ ->
          EZCMD.main_with_subcommands
            ~name:Globals.command ~version:Version.version
            ~doc:"Create and manage an OCaml project" ~man:[] ~argv ez_commands
            ~common_args;
    end;
    if Config.loaded () then
      let config = Config.config () in
      if config.modified then
        Config.save config
  with
  | Error.Error s ->
      Printf.eprintf "Error: %s\n%!" s;
      exit 2
  | exn ->
      let bt = Printexc.get_backtrace () in
      let error = Printexc.to_string exn in
      Printf.eprintf "fatal exception %s\n%s\n%!" error bt;
      exit 2
