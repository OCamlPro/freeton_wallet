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

open Types.MULTISIG

let get_waiting ?(f = fun _ -> ()) account =
  let config = Config.config () in
  let net = Config.current_network config in
  let key = Misc.find_key_exn net account in
  let contract = CommandMultisigCreate.check_key_contract key in
  let address = Misc.get_key_address_exn key in

  let subst ~msg:_ _config res =
    match EzEncoding.destruct Types.MULTISIG.updates_enc res with
    | exception exn ->
        Printf.eprintf "Can't parse: %s\n%!" ( Printexc.to_string exn );
        Printf.printf "call result:\n%s\n%!" res
    | trs ->
        let trs = trs.updates in
        Printf.printf "%d updates waiting\n%!" (List.length trs);
        List.iter (fun tr ->
            let id = Int64.of_string tr.update_id in
            let generation_time = Int64.to_float
                (Int64.shift_right id 32 ) in
            let delay = Unix.gettimeofday () -. generation_time in
            let delay = int_of_float delay in
            let hours = delay / 3600 in
            let secs = delay - hours * 3600 in
            let mins = secs / 60 in
            let secs = secs - mins * 60 in
            Printf.printf "Update id: %Ld\n%!" id ;
            Printf.printf "   Age: %dh%dm%ds\n%!" hours mins secs ;
            Printf.printf "   Confirmations: %s/??\n%!"
              tr.update_signs (* tr.update_reqConfirms *) ;
            Printf.printf "   Confirmed by:";
            let confirmationsMask = int_of_string tr.update_confirmationsMask in
            for i = 0 to 31 do
              if confirmationsMask land (1 lsl i) <> 0 then
                Printf.printf " (%d)" i
            done;
            Printf.printf "\n%!";
            Printf.printf "   Creator: %s (%s)\n%!"
              tr.update_creator tr.update_index ;
            Printf.printf "     CodeHash: %s\n%!" tr.update_codeHash ;
            Printf.printf "     Confirms: %s\n%!" tr.update_reqConfirms ;
            Printf.printf "     Custodians: %d\n%!"
              ( List.length tr.update_custodians);
            List.iter (fun c -> Printf.printf "       %s\n%!" c)
              tr.update_custodians;
            f tr
          ) trs
  in

  Utils.call_contract config
    ~contract
    ~address
    ~meth:"getUpdateRequests"
    ~params:"{}"
    ~local:true
    ~subst
    ()

let action account =

  let account = match account with
    | None ->
        Error.raise "You must provide the account"
    | Some account -> account
  in
  get_waiting account ;
  ()

let cmd =
  let account = ref None in
  Misc.cmd
    [ "multisig"; "list"; "updates" ]
    (fun () ->
       action !account
    )
    ~args:
      [
        [], Arg.Anon (0, fun s -> account := Some s),
        EZCMD.info ~docv:"ACCOUNT" "The multisig account";
      ]
    ~doc: "Display waiting transactions in a multisig wallet"
    ~man:[
      `S "DESCRIPTION";
      `P "This command can be used to display the currently waiting transactions in a multisig wallet" ;
      `S "LIST WAITING TRANSACTIONS";
      `P "Display transactions waiting for confirmations:";
      `Pre {|# ft multisig list transactions MY-ACCOUNT|};
    ]
