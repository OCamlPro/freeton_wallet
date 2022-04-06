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

let create_interface name =
  let filename = name ^ ".sol" in
  if Sys.file_exists filename then
    Printf.eprintf "Skipping generation of %S: already exists\n%!" filename
  else
    let content =
      Printf.sprintf
        {|/* Interface %s */

pragma ton-solidity >= 0.32.0;

interface %s {

  /* some functions like:
  function do_something( uint256 arg ) external ;
  */
}
|} name name
    in
    EzFile.write_file filename content;
    Printf.eprintf "Interface %S generated\n%!" filename

let cmd =
  EZCMD.sub
    "contract new interface"
    (fun () ->
       Error.raise "Error: You must provide a contrat name\n%!"
    )
    ~args:[
      [], Arg.Anons (List.iter create_interface),
      EZCMD.info ~docv:"CONTRACT" "Create interface file for contract" ;
    ]
    ~doc: "Generate a new contract interface source file"
    ~man:[]
