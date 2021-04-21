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

let echo () =
  Printf.eprintf "CMD: \x1B[1;33m %s \x1B[0m \n%!"
    ( String.concat " "
        ( List.filter (fun s -> s <> "--echo")
            ( Array.to_list Sys.argv )))

let main () =
  let commands =
    [
      CommandSwitch.cmd;
      CommandGenaddr.cmd;
      CommandList.cmd;
      CommandAccount.cmd;
      CommandClient.cmd;
      CommandOutput.cmd;
      CommandMultisig.cmd;
      CommandContract.cmd;
      CommandTest.cmd;
      CommandInit.cmd;
      CommandNode.cmd;
      CommandConfig.cmd;
      CommandCall.cmd;
      CommandUtils.cmd;
      CommandWatch.cmd;
      CommandInspect.cmd;
    ]
  in
  let common_args =
    [
      [ "v"; "verbose" ],
      Arg.Unit (fun () -> incr Globals.verbosity),
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
  Printexc.record_backtrace true;
  let args = Array.to_list Sys.argv in
  let rec iter_initial_args args =
    match args with
    | [] ->
        Printf.eprintf "Use 'ft --help' for help on commands\n%!";
        Config.print ();
        []
    | "--switch" :: switch :: args ->
        Config.set_temporary_switch switch;
        iter_initial_args args
    | "--echo" :: args ->
        echo () ;
        iter_initial_args args
    | [ "--version" ] ->
        Printf.printf "%s\n%!" Version.version;
        exit 0
    | [ "--about" ] ->
        Printf.printf "%s\n%!" Globals.about;
        exit 0
    | ( "-v" | "--verbose" ) :: args ->
        incr Globals.verbosity;
        iter_initial_args args
    | ( "-q" | "--quiet" ) :: args ->
        Globals.verbosity := 0;
        iter_initial_args args
    | [ "rst" ] ->
        Printf.printf "%s%!" ( EZCMD.to_rst commands common_args );
        exit 0
    | _ -> args
  in

  let args = iter_initial_args (List.tl args ) in
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
            ~doc:"Create and manage an OCaml project" ~man:[] ~argv commands
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
