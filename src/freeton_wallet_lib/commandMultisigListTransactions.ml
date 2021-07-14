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

let get_waiting account =
  let config = Config.config () in
  let net = Config.current_network config in
  let key = Misc.find_key_exn net account in
  let contract = CommandMultisigCreate.check_key_contract key in
  let address = Misc.get_key_address_exn key in

  let subst ~msg:_ _config res =
    match EzEncoding.destruct Types.MULTISIG.transactions_enc res with
    | exception exn ->
        Printf.eprintf "Can't parse: %s\n%!" ( Printexc.to_string exn );
        Printf.printf "call result:\n%s\n%!" res
    | trs ->
        let trs = trs.transactions in
        Printf.printf "%d transactions waiting\n%!" (List.length trs);
        List.iter (fun tr ->
            let id = Int64.of_string tr.id in
            let generation_time = Int64.to_float
                (Int64.shift_right id 32 ) in
            let delay = Unix.gettimeofday () -. generation_time in
            let delay = int_of_float delay in
            let hours = delay / 3600 in
            let secs = delay - hours * 3600 in
            let mins = secs / 60 in
            let secs = secs - mins * 60 in
            Printf.printf "Transaction id: %Ld\n%!" id ;
            Printf.printf "   Age: %dh%dm%ds\n%!" hours mins secs ;
            Printf.printf "   Confirmations: %s/%s\n%!"
              tr.signsReceived tr.signsRequired ;
            Printf.printf "   Confirmed by:";
            let confirmationsMask = int_of_string tr.confirmationsMask in
            for i = 0 to 31 do
              if confirmationsMask land (1 lsl i) <> 0 then
                Printf.printf " (%d)" i
            done;
            Printf.printf "\n%!";
            Printf.printf "   Creator: %s (%s)\n%!"
              tr.creator tr.index ;
            Printf.printf "     Dest: %s\n%!" tr.dest ;
            Printf.printf "     Value: %s\n%!"
              ( Misc.string_of_nanoton (Int64.of_string tr.value ) ) ;
            if tr.sendFlags <> "0" then
              Printf.printf "     Flags: %s\n%!" tr.sendFlags ;
            if not tr.bounce then
              Printf.printf "     Bounce: %b\n%!" tr.bounce ;
          ) trs

  in

  Utils.call_contract config
    ~contract
    ~address
    ~meth:"getTransactions"
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
  EZCMD.sub
    "multisig list transactions"
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
