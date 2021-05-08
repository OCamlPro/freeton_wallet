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

(* Use %{account}, %{account:addr}, %{account:keyfile}, %{acccount:pubkey}
   %{contract:tvc}, %{contract:abi}
 *)

let action ?(exec=false) ?stdout args =
  let config = Config.config () in
  let (subst, files) = Subst.subst_string config in
  let args = List.map (fun arg ->
      subst arg
    ) args in
  let clean () =
    List.iter Sys.remove !files
  in
  let cmd = if exec then args else Utils.tonoscli config args in
  match
    match stdout with
    | None -> Misc.call cmd
    | Some file ->
        let ( _ : string ) = Misc.call_stdout_file ~file cmd in
        ()
  with
  | exception exn -> clean () ; raise exn
  | v -> v

let cmd =
  let args = ref [] in
  let stdout = ref None in
  EZCMD.sub
    "client"
    (fun () ->
       action !args ~exec:false ?stdout:!stdout
    )
    ~args:
      [ [],
        Arg.Anons (fun list -> args := list),
        EZCMD.info ~docv:"-- ARGUMENTS" "Arguments to tonos-cli" ;

        [ "stdout" ], Arg.String (fun s -> stdout := Some s),
        EZCMD.info ~docv:"FILE" "Save command stdout to file";

      ]
    ~doc: "Call tonos-cli, use -- to separate arguments. Use 'ft exec \
           -- CMD ARGS' for other commands."
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command calls the tonos-cli executable while \
            performing substitutions on arguments, and using the node \
            of the current network switch. It is useful for commands \
            that 'ft' cannot perform directly (calling debots for \
            example).";
        `P "'ft' uses the executable stored in \
            $HOME/.ft/bin/tonos-cli. To create this executable, use:";
        `Pre {|$ ft init|};
        `P "or:";
        `Pre {|$ ft init client|};
        `P "The available substitutions on the arguments can be listed using:";
        `Pre {|$ ft output --list-subst|};
        `P "For example, to substitute the address of the account 'multisig-debot':";
        `Pre {|$ ft client -- debot %{account:address:multisig-debot}|};
        `P "Note that it is also possible to ask 'ft' to call \
            'tonos-cli' instead of performing calls through TON-SDK \
            Rust binding for other commands, using the FT_USE_TONOS=1 \
            env. variable.";
      ];
    ]
