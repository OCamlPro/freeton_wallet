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

let list_substitutions () =
  Printf.printf "%s\n%!" Subst.help

let action ~stdout ~file ~string ~keyfile ~addr =
  let config = Config.config () in
  let subst, _files = Subst.subst_string config in
  let content =
    match file, string with
    | Some file, None ->
        subst ( EzFile.read_file file )
    | None, Some s ->
        subst s
    | Some _, Some _ ->
        Error.raise "Cannot use both --file and --string"
    | None, None ->
        match keyfile with
        | Some account ->
            let net = Config.current_network config in
            let key = Misc.find_key_exn net account in
            let key_pair = Misc.get_key_pair_exn key in
            EzEncoding.construct ~compact:false Encoding.keypair key_pair
        | None ->
            match addr with
            | Some account ->
                let net = Config.current_network config in
                let key = Misc.find_key_exn net account in
                let acc = Misc.get_key_account_exn key in
                acc.acc_address
            | None ->
                Error.raise "Use one of the arguments"
  in
  match stdout with
  | None -> Printf.printf "%s\n%!" content
  | Some stdout ->
      EzFile.write_file stdout content

let cmd =
  let stdout = ref None in
  let file = ref None in
  let string = ref None in
  let keyfile = ref None in
  let addr = ref None in
  EZCMD.sub
    "output"
    (fun () ->
       action
         ~stdout:!stdout
         ~file:!file
         ~string:!string
         ~keyfile:!keyfile
         ~addr:!addr
    )
    ~args:
      [
        [ "o" ], Arg.String (fun s -> stdout := Some s),
        EZCMD.info ~docv:"FILE" "Save command stdout to file";

        [ "file" ], Arg.String (fun s -> file := Some s),
        EZCMD.info ~docv:"FILE" "Output content of file after substitution";

        [ "string" ], Arg.String (fun s -> string := Some s),
        EZCMD.info ~docv:"STRING" "Output string after substitution";

        [ ], Arg.Anon (0, fun s -> string := Some s),
        EZCMD.info ~docv:"STRING" "Output string after substitution";

        [ "keyfile" ], Arg.String (fun s -> keyfile := Some s ),
        EZCMD.info ~docv:"ACCOUNT" "Output key file of account";

        [ "addr" ], Arg.String (fun s -> addr := Some s),
        EZCMD.info ~docv:"ACCOUNT" "Output address of account";

        [ "list-subst" ], Arg.Unit (fun () ->
              list_substitutions (); exit 0),
          EZCMD.info "List all substitutions";
      ]
    ~doc: "Perform substitutions on the output"
    ~man:[
      `S "DESCRIPTION";
      `P "This command performs substitutions on its input. By \
          default, the output goes to stdout, unless the '-o' option \
          is used.";
      `P "Examples:";
      `P "Load a file INPUT, substitute its content, and save to OUTPUT:";
      `Pre {|$ ft output --file INPUT --o OUTPUT|};
      `P "List available substitutions:";
      `Pre {|$ ft output --list-subst|};
      `P "Output address of account ACCOUNT:";
      `Pre {|$ ft output --addr ACCOUNT|};
      `P "or:";
      `Pre {|$ ft output --string %{account:address:ACCOUNT}|};
      `P "Output keyfile of account ACCOUNT to file KEYFILE:";
      `Pre {| ft output --keyfile ACCOUNT -o KEYFILE|};
    ]
