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
open EzFile.OP

let known_contracts () =
  let contracts = ref StringMap.empty in
  List.iter (fun file ->
      if Filename.dirname file = "contracts" then
        match EzString.split (Filename.basename file) '.' with
        | [ name ; "abi" ; "json" ] ->
            contracts := StringMap.add name "(embedded)" !contracts
        | _ -> ()
    ) Files.file_list;

  Array.iter (fun file ->
      match EzString.split (Filename.basename file) '.' with
      | [ name ; "tvc" ] ->
          contracts := StringMap.add name
              ( Globals.contracts_dir // file ) !contracts
      | [ name ] ->
          let dir = Globals.contracts_dir // name in
          if Sys.is_directory dir then
            let current = EzFile.read_file ( dir // "CURRENT" )
                          |> String.trim in
            let filename = dir // current ^ ".tvc" in
            if not ( Sys.file_exists filename ) then
              Printf.eprintf "Warning: %s does not exist\n%!" filename
            else
              contracts := StringMap.add name
                  ( Printf.sprintf "%s (version %s)" dir current )
                  !contracts
      | _ -> ()
    ) (try Sys.readdir Globals.contracts_dir with _ -> [||]);
  !contracts

let list_contracts () =
  let set = known_contracts () in
  Printf.printf "Known contracts:\n";
  StringMap.iter  (fun name s ->
      Printf.printf "* %s %s\n" name s) set;
  Printf.printf "%!"

let action () =
  list_contracts ()

let cmd =
  EZCMD.sub
    "list"
    (fun () -> action ())
    ~args: []
    ~doc: "List known contracts (DEPRECATED). Use 'ft contract --list'"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "DEPRECATED";
        `P "This command is deprecated and will disappear soon. Use 'ft contract --list' instead."
      ];

    ]
