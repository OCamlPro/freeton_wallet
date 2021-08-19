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

let action ?client_repo ?solc_repo ?linker_repo ?(add_multisigs=[]) () =
  let config = Config.config () in
  let net = Config.current_network config in
  let repos = Config.toolchain config in
  match client_repo, solc_repo, linker_repo, add_multisigs with
  | None, None, None, [] ->
      Printf.printf "Config: toolchain: %S\n" net.net_toolchain;
      Printf.printf "  client-repo: %s\n" repos.repo_tonos_cli ;
      Printf.printf "  solc-repo: %s\n" repos.repo_solc ;
      Printf.printf "  linker-repo: %s\n" repos.repo_tvm_linker ;
      Printf.printf "  multisigs: %s\n"
        ( String.concat ", " config.multisigs );
      Printf.printf "%!"

  | _ ->
      let maybe_set option name default setter =
        match option with
        | None -> ()
        | Some s ->
            let s =
              match s with
              | "" | "-" | "default" -> default
              | _ -> s
            in
            setter s ;
            Printf.eprintf "%s set to %S\n5!" name s;
            config.modified <- true
      in
      maybe_set client_repo "client-repo" Config.default_repos.repo_tonos_cli
        (fun s -> repos.repo_tonos_cli <- s ) ;
      maybe_set solc_repo "solc-repo" Config.default_repos.repo_solc
        (fun s -> repos.repo_solc <- s ) ;
      maybe_set linker_repo "linker-repo" Config.default_repos.repo_tvm_linker
        (fun s -> repos.repo_tvm_linker <- s ) ;
      List.iter (fun s ->
          if not ( List.mem s config.multisigs ) then begin
            config.multisigs <- s :: config.multisigs ;
            Printf.printf "%s added to multisigs\n%!" s;
            config.modified <- true ;
          end
        ) add_multisigs ;
      ()

let cmd =
  let client_repo = ref None in
  let solc_repo = ref None in
  let linker_repo = ref None in
  let add_multisigs = ref [] in
  let deployer = ref None in
  EZCMD.sub
    "config"
    (fun () ->
       begin
         match !deployer with
         | None -> ()
         | Some deployer ->
             CommandSwitchConfig.action ~deployer ()
       end;
       action
         ?client_repo:!client_repo
         ?solc_repo:!solc_repo
         ?linker_repo:!linker_repo
         ~add_multisigs:!add_multisigs
         ()
    )
    ~args:
      [

        [ "client-repo" ], Arg.String ( fun s -> client_repo := Some s ),
        EZCMD.info ~docv:"GIT_URL"
          "The URL of the GIT repo of tonos-cli for `ft init`" ;

        [ "solc-repo" ], Arg.String ( fun s -> solc_repo := Some s ),
        EZCMD.info ~docv:"GIT_URL"
          "The URL of the GIT repo of TON-Solidity-Compiler for `ft init`" ;

        [ "linker-repo" ], Arg.String ( fun s -> linker_repo := Some s ),
        EZCMD.info ~docv:"GIT_URL"
          "The URL of the GIT repo of TVM-linker for `ft init`" ;

        [ "add-multisig" ], Arg.String ( fun s ->
            add_multisigs := s :: !add_multisigs ),
        EZCMD.info ~docv:"CONTRACT"
          "Add a new multisig contract name" ;

        [ "deployer" ], Arg.String ( fun s -> deployer := Some s ),
        EZCMD.info ~docv:"ACCOUNT"
          "DEPRECATED. Use `ft switch config --deployer` instead." ;

      ]
    ~doc: "Modify configuration"
    ~man:[
      `S "DESCRIPTION";
      `P "Change the global configuration or the network configuration.";
    ]
