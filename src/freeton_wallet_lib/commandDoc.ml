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
open EzFile.OP

let action ~file =
  let file = match file with
    | None -> "API.md"
    | Some file -> file
  in
  let file = Globals.doc_dir // file in
  if not ( Sys.file_exists file ) then
    Error.raise "File %S does not exist" file;
  let _retcode = Printf.kprintf Sys.command "less '%s'" file in
  ()

let cmd =
  let file = ref None in
  EZCMD.sub
    "doc"
    (fun () -> action ~file:!file )
    ~args:
      [
        [], Arg.Anon (0, fun s -> file := Some s),
        EZCMD.info ~docv:"DOCFILE" "File to display" ;
      ]
    ~doc: "Display useful documentation files (stored in ~/.ft/doc/)"
    ~man:[]
