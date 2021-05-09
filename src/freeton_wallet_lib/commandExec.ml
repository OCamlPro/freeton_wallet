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

let cmd =
  let args = ref [] in
  let stdout = ref None in
  EZCMD.sub
    "exec"
    (fun () ->
       CommandClient.action !args ~exec:true ?stdout:!stdout
    )
    ~args:
      [ [],
        Arg.Anons (fun list -> args := list),
        EZCMD.info ~docv:"-- COMMAND ARGUMENTS" "Command and arguments" ;

        [ "stdout" ], Arg.String (fun s -> stdout := Some s),
        EZCMD.info ~docv:"FILENAME" "Save command stdout to file FILENAME";
      ]
    ~doc: "Call command with substitution on arguments, use -- before \
           the command."
    ~man:[
      `S "DESCRIPTION";
      `P "This command can be used to call external commands while performing substitutions on arguments.";
      `P "The available substitutions on the arguments can be listed using:";
      `Pre {|$ ft output --list-subst|};
      `P "For example:";
      `P {|$ ft exec -- echo %{account:address:giver}|};

    ]
