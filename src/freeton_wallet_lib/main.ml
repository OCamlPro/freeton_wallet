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

module MAIN = struct

  let common_args = [
    [ "switch" ],
    Arg.String (fun switch ->
        Config.set_temporary_switch switch;
      ),
    EZCMD.info "Set switch" ;
  ]

  let commands = List.map snd
      [
        [ "switch" ; "list" ], CommandSwitchList.cmd;
        [ "switch" ; "to" ], CommandSwitchTo.cmd;
        [ "switch" ; "create" ], CommandSwitchCreate.cmd;
        [ "switch" ; "remove" ], CommandSwitchRemove.cmd;
        [ "switch" ; "config" ], CommandSwitchConfig.cmd;
        [ "account" ; "copy" ; "from" ], CommandAccountCopyFrom.cmd;
        [ "account" ; "rename" ], CommandAccountRename.cmd;
        [ "account" ; "create" ], CommandAccountCreate.cmd;
        [ "account" ; "whois" ], CommandAccountWhois.cmd;
        [ "account" ; "info" ], CommandAccountInfo.cmd;
        CommandAccountState.cmd;
        [ "account" ; "list" ], CommandAccountList.cmd;
        [ "account" ; "set" ], CommandAccountSet.cmd;
        [ "account" ; "remove" ], CommandAccountRemove.cmd;
        [ "account" ; "history" ], CommandAccountHistory.cmd;

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
        CommandMultisigConfirmTransaction.cmd;
        [ "multisig" ; "debot" ], CommandMultisigDebot.cmd;
        CommandMultisigListUpdates.cmd;
        CommandMultisigConfirmUpdate.cmd;
        CommandMultisigInfo.cmd;
        CommandMultisigUpdate.cmd;
        CommandMultisigExecuteUpdate.cmd;
        CommandMultisigNew.cmd;

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
        (*      [ "debot" ], CommandDebot.cmd; *)
        [ "debot" ; "new" ], CommandDebotNew.cmd;
        [ "debot" ; "fetch" ], CommandDebotFetch.cmd;

        (*        CommandWallet.cmd; *)
        (*        CommandTokenRootCreate.cmd; *)
        CommandTokenList.cmd;
        CommandTokenTransfer.cmd;
        CommandTokenSwap.cmd;
        CommandTokenWTON.cmd;
        CommandTokenWTONCredit.cmd;
        CommandTokenWTONWithdraw.cmd;
      ]

  let main () =

   let argv =
     Array.map (fun arg ->
         let len = String.length arg in
         if len > 20 && arg.[0] = '-' &&
            match arg.[1] with
            | '1'..'9' when String.contains arg ':' -> true
            | _ -> false
         then
           "0" ^ arg
         else
           arg
       ) Sys.argv
   in
   Globals.MAIN.main
     ~on_error:(fun () -> Solidity_parser.keep_temporary_files () )
     ~on_exit:(fun () ->
         if Config.loaded () then
           let config = Config.config () in
           if config.modified then
             Config.save config
       )
     ~print_config:(fun () -> Config.print ())
     ~common_args
     ~argv
     commands

end

let main = MAIN.main
